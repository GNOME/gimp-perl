package Gimp::Net;

# This package is loaded by the Gimp, and is !private!, so don't
# use it standalone, it won't work.

# the protocol is quite easy ;)
# at connect() time the server returns
# PERL-SERVER protocolversion [AUTH]
#
# length_of_packet cmd
#
# cmd			response		description
# AUTH password		ok [message]		authorize yourself
# QUIT						quit server
# EXEC in-args		status out-args		run simple command
# TRCE trace in-args	trace status out-args	run simple command (with tracing)
# TEST procname		bool			check for procedure existance
# DTRY in-args					destroy all argument objects
# LOCK lock? shared?				lock or unlock
# RSET						reset server (NYI)
#
# args is "number of arguments" arguments preceded by length
# type is first character
# Sscalar-value
# Aelem1\0elem2...
# Rclass\0scalar-value

use strict 'vars';
use vars qw(
   $VERSION
   $default_tcp_port $default_unix_dir $default_unix_sock
   $server_fh $trace_level $trace_res $auth $gimp_pid
   $PROTOCOL_VERSION
);
use subs qw(gimp_call_procedure);
use base qw(DynaLoader);

use Socket; # IO::Socket is _really_ slow, so don't use it!

use Gimp ('croak','__');
use Fcntl qw(F_SETFD);

use constant {
  PS_FLAG_QUIET => 1 << 0, # do not output messages
  PS_FLAG_BATCH => 1 << 1, # started via Gimp::Net, extra = filehandle
};

$PROTOCOL_VERSION = "3"; # protocol version

# TODO: use dynaloader
require DynaLoader;

$VERSION = 2.3001;

bootstrap Gimp::Net $VERSION;

$default_tcp_port  = 10009;
$default_unix_dir  = "/tmp/gimp-perl-serv-uid-$>/";
$default_unix_sock = "gimp-perl-serv";

$trace_res = *STDERR;
$trace_level = 0;

my $initialized = 0;

sub initialized { $initialized }

sub import {
   my $pkg = shift;

   return if @_;

   # overwrite some destroy functions
   *Gimp::Tile::DESTROY=
   *Gimp::PixelRgn::DESTROY=
   *Gimp::GimpDrawable::DESTROY=sub {
      my $req="DTRY".args2net(0,@_);
      print $server_fh pack("N",length($req)).$req;

      # make this synchronous to avoid deadlock due to using non sys*-type functions
      my $len;
      read($server_fh,$len,4) == 4 or die "protocol error (11)";
   };
}

sub gimp_procedural_db_proc_exists {
   my $req="TEST".$_[0];
   print $server_fh pack("N",length($req)).$req;
   read($server_fh,$req,1);
   return $req;
}

# this is hardcoded into gimp_call_procedure!
sub response {
   my($len,$req);
   read($server_fh,$len,4) == 4 or die "protocol error (1)";
   $len=unpack("N",$len);
   read($server_fh,$req,$len) == $len or die "protocol error (2)";
   net2args(0,$req);
}

# this is hardcoded into gimp_call_procedure!
sub command {
   my $req=shift;
   $req.=args2net(0,@_);
   print $server_fh pack("N",length($req)).$req;
}

sub gimp_call_procedure {
   my (@args,$trace,$req);
   warn "Net::gimp_call_procedure[$trace_level](@_)" if $Gimp::verbose;
   $req = ($trace_level ? "TRCE" : "EXEC") . args2net(
     0, ($trace_level ? $trace_level : ()), @_
   );
   print $server_fh pack("N",length($req)).$req;
   do {
      my $len;
      read($server_fh,$len,4) == 4 or die "protocol error (3)";
      $len=unpack("N",$len);
      read($server_fh,$req,abs($len)) == $len or die "protocol error (4)";
      if ($len<0) {
	 ($req,@args)=net2args(0,$req);
	 print "ignoring callback $req\n";
	 redo;
      }
      @args = net2args(0,$req);
      $trace = shift @args if $trace_level;
      $req = shift @args;
      if ($trace_level) {
	 if (ref $trace_res eq "SCALAR") {
	    $$trace_res = $trace;
	 } else {
	    print $trace_res $trace;
	 }
      }
   } while 0;
   die $req if $req;
   wantarray ? @args : $args[0];
}

sub server_quit {
   print $server_fh pack("N",4)."QUIT";
   undef $server_fh;
}

sub lock {
   print $server_fh pack("N",12)."LOCK".pack("N*",1,0);
}

sub unlock {
   print $server_fh pack("N",12)."LOCK".pack("N*",0,0);
}

sub set_trace {
   my($trace)=@_;
   my $old_level = $trace_level;
   if(ref $trace) {
      $trace_res=$trace;
   } elsif (defined $trace) {
      $trace_level=$trace;
   }
   $old_level;
}

sub start_server {
   my $opt = shift;
   $opt = $Gimp::spawn_opts unless $opt;
   print __"trying to start gimp with options \"$opt\"\n" if $Gimp::verbose;
   $server_fh=local *SERVER_FH;
   my $gimp_fh=local *CLIENT_FH;
   socketpair $server_fh,$gimp_fh,AF_UNIX,SOCK_STREAM,PF_UNSPEC
      or socketpair $server_fh,$gimp_fh,AF_LOCAL,SOCK_STREAM,PF_UNSPEC
      or croak __"unable to create socketpair for gimp communications: $!";

   # do it here so it is done only once
   require Gimp::Config;
   $gimp_pid = fork;
   if ($gimp_pid > 0) {
      Gimp::ignore_functions(@Gimp::gimp_gui_functions) unless $opt=~s/(^|:)gui//;
      return $server_fh;
   } elsif ($gimp_pid == 0) {
      close $server_fh;
      fcntl $gimp_fh, F_SETFD, 0;
      delete $ENV{GIMP_HOST};
      unless ($Gimp::verbose) {
         open STDIN,"</dev/null";
         open STDOUT,">/dev/null";
         open STDERR,">&1";
      }
      my @args;
      my $flags = PS_FLAG_BATCH | ($Gimp::verbose ? PS_FLAG_QUIET : 0);
      my $args = join ' ',
	&Gimp::RUN_NONINTERACTIVE,
	$flags,
	fileno($gimp_fh),
	int($Gimp::verbose);
      push(@args,"--no-data") if $opt=~s/(^|:)no-?data//;
      push(@args,"-i") unless $opt=~s/(^|:)gui//;
      push(@args,"--verbose") if $Gimp::verbose;
      exec $Gimp::Config{GIMP},
           "--no-splash",
           #"never",
           "--console-messages",
           @args,
	   "--batch-interpreter",
	   "plug-in-script-fu-eval",
           "-b",
	   "(if (defined? 'extension-perl-server) (extension-perl-server $args))",
	   "-b",
           "(gimp-quit 0)";
      exit(255);
   } else {
      croak __"unable to fork: $!";
   }
}

sub try_connect {
   local $_=$_[0];
   my $fh;
   $auth = s/^(.*)\@// ? $1 : "";	# get authorization
   if ($_ ne "") {
      if (s{^spawn/}{}) {
         return start_server($_);
      } elsif (s{^unix/}{/}) {
         my $server_fh=local *FH;
         return ((socket($server_fh,AF_UNIX,SOCK_STREAM,PF_UNSPEC)
                 || socket $server_fh,AF_LOCAL,SOCK_STREAM,PF_UNSPEC)
                && connect($server_fh,sockaddr_un $_)
                ? $server_fh : ());
      } else {
         s{^tcp/}{};
         my($host,$port)=split /:/,$_;
         $port=$default_tcp_port unless $port;
         my $server_fh=local *FH;
         return socket($server_fh,PF_INET,SOCK_STREAM,scalar getprotobyname('tcp') || 6)
                && connect($server_fh,sockaddr_in $port,inet_aton $host)
                ? $server_fh : ();
      }
   } else {
      return $fh if $fh = try_connect ("$auth\@unix$default_unix_dir$default_unix_sock");
      return $fh if $fh = try_connect ("$auth\@tcp/127.1:$default_tcp_port");
      return $fh if $fh = try_connect ("$auth\@spawn/");
   }
   undef $auth;
}

sub gimp_init {
   $Gimp::in_top=1;
   if (@_) {
      $server_fh = try_connect ($_[0]);
   } elsif (defined($Gimp::host)) {
      $server_fh = try_connect ($Gimp::host);
   } elsif (defined($ENV{GIMP_HOST})) {
      $server_fh = try_connect ($ENV{GIMP_HOST});
   } else {
      $server_fh = try_connect ("");
   }
   defined $server_fh or croak __"could not connect to the gimp server (make sure Perl-Server is running)";
   { my $fh = select $server_fh; $|=1; select $fh }
   
   my @r = response();
   
   die __"expected perl-server at other end of socket, got @r\n"
      unless $r[0] eq "PERL-SERVER";
   shift @r;
   die __"expected protocol version $PROTOCOL_VERSION, but server uses $r[0]\n"
      unless $r[0] eq $PROTOCOL_VERSION;
   shift @r;
   
   for(@r) {
      if($_ eq "AUTH") {
         die __"server requests authorization, but no authorization available\n"
            unless $auth;
         my $req = "AUTH".$auth;
         print $server_fh pack("N",length($req)).$req;
         my @r = response();
         die __"authorization failed: $r[1]\n" unless $r[0];
         print __"authorization ok, but: $r[1]\n" if $Gimp::verbose and $r[1];
      }
   }

   $initialized = 1;
   Gimp::_initialized_callback;
}

sub gimp_end {
   $initialized = 0;

   #close $server_fh if $server_fh;
   undef $server_fh;
   kill 'KILL',$gimp_pid if $gimp_pid;
   undef $gimp_pid;
}

sub gimp_main {
   gimp_init;
   no strict 'refs';
   $Gimp::in_top=0;
   eval { Gimp::callback("-net") };
   if($@ && $@ ne "IGNORE THIS MESSAGE\n") {
      Gimp::logger(message => substr($@,0,-1), fatal => 1, function => 'DIE');
      gimp_end;
      -1;
   } else {
      gimp_end;
      0;
   }
}

sub get_connection() {
   [$server_fh,$gimp_pid];
}

sub set_connection($) {
   ($server_fh,$gimp_pid)=@{+shift};
}

END {
   gimp_end;
}

# start of server-used block
use vars qw($use_unix $use_tcp $trace_res $server_quit $max_pkt $unix $tcp
            $ps_flags $auth @authorized $exclusive $rm $saved_rm %stats);
# you can enable unix sockets, tcp sockets, or both (or neither...)
#
# enabling tcp sockets can be a security risk. If you don't understand why,
# you shouldn't enable it!
#
$use_unix	= 1;
$use_tcp	= 1;	# tcp is enabled only when authorization is available

$server_quit = 0;

my $max_pkt = 1024*1024*8;
my $exclusive = 0;

sub slog {
  return if $ps_flags & &PS_FLAG_QUIET;
  print time(),": ",@_,"\n";
}

# this is hardcoded into handle_request!
sub reply {
   my $fh=shift;
   my $data=args2net(0,@_);
   print $fh pack("N",length($data)).$data;
}

sub handle_request($) {
   my($fh)=@_;
   my ($req,$data);

   eval {
      local $SIG{ALRM}=sub { die "1\n" };
      #alarm(6) unless $ps_flags & &PS_FLAG_BATCH;
      my $length;
      read($fh,$length,4) == 4 or die "2\n";
      $length=unpack("N",$length);
      $length>0 && $length<$max_pkt or die "3\n";
      #alarm(6) unless $ps_flags & &PS_FLAG_BATCH;
      read($fh,$req,4) == 4 or die "4\n";
      #alarm(20) unless $ps_flags & &PS_FLAG_BATCH;
      read($fh,$data,$length-4) == $length-4 or die "5\n";
      #alarm(0);
   };
   return 0 if $@;

   if(!$auth or $authorized[fileno($fh)]) {
      if ($req eq "TRCE" or $req eq "EXEC") {
         no strict 'refs';
         my @args = net2args(1, $data);
         my $trace_level = shift @args if $req eq "TRCE";
	 my $function = shift @args;
         Gimp::set_trace($trace_level) if $req eq "TRCE";
         $trace_res = "" if $req eq "TRCE";
         @args = eval { Gimp->$function(@args) };
	 unshift @args, $@;
	 unshift @args, $trace_res if $req eq "TRCE";
         $data = args2net(1,@args);
         print $fh pack("N",length($data)).$data;
         Gimp::set_trace(0) if $req eq "TRCE";
      } elsif ($req eq "TEST") {
         no strict 'refs';
         print $fh (defined(*{"Gimp::Lib::$data"}{CODE}) || Gimp::gimp_procedural_db_proc_exists($data)) ? "1" : "0";
      } elsif ($req eq "DTRY") {
         destroy_objects net2args 0,$data;
         print $fh pack("N",0); # fix to work around using non-sysread/write functions
      } elsif ($req eq "QUIT") {
         slog __"received QUIT request";
         $server_quit = 1;
      } elsif($req eq "AUTH") {
         $data=args2net(0,1,__"authorization unnecessary");
         print $fh pack("N",length($data)).$data;
      } elsif($req eq "LOCK") {
         my($lock,$shared)=unpack("N*",$data);
         slog __"WARNING: shared locking requested but not implemented" if $shared;
         if($lock) {
            unless($exclusive) {
               $saved_rm=$rm;
               undef $rm; vec($rm,fileno($fh),1)=1;
            }
            $exclusive++;
         } else {
            if ($exclusive) {
               $exclusive--;
               $rm = $saved_rm unless $exclusive;
            } else {
               slog __"WARNING: client tried to unlock without holding a lock";
            }
         }
      } else {
         print $fh pack("N",0);
         slog __"illegal command received, aborting connection";
         return 0;
      }
   } else {
      if($req eq "AUTH") {
         my($ok,$msg);
         if($data eq $auth) {
            $ok=1;
            $authorized[fileno($fh)]=1;
         } else {
            $ok=0;
            $msg=__"wrong authorization, aborting connection";
            slog $msg;
            sleep 5; # safety measure
         }
         $data=args2net(0,$ok,$msg);
         print $fh pack("N",length($data)).$data;
         return $ok;
      } else {
         print $fh pack("N",0);
         slog __"unauthorized command received, aborting connection";
         return 0;
      }
   }
   return 1;
}

my %handles;

sub new_connection {
  my $fh = shift;
  select $fh; $|=1; select STDOUT;
  $handles{fileno($fh)}=$fh;
  my @r = ("PERL-SERVER",$PROTOCOL_VERSION);
  push(@r,"AUTH") if $auth;
  reply $fh,@r;
  vec($rm,fileno($fh),1)=1;
  $stats{fileno($fh)}=[0,time];
}

sub extension_perl_server {
  my $run_mode=$_[0];
  $ps_flags=$_[1];
  my $extra=$_[2];
  $Gimp::verbose=$_[3];

  if ($run_mode == &Gimp::RUN_NONINTERACTIVE) {
     if ($ps_flags & &PS_FLAG_BATCH) {
        my($fh) = local *FH;
        open $fh,"+<&$extra" or die __"unable to open Gimp::Net communications socket: $!\n";
        select $fh; $|=1; select STDOUT;
        reply $fh,"PERL-SERVER",$PROTOCOL_VERSION;
        while(!$server_quit and !eof($fh)) {
           last unless handle_request($fh);
        }
        Gimp->quit(0);
        exit(0);
     }
  } else {
     $run_mode=&Gimp::RUN_INTERACTIVE;
     $ps_flags=0;
  }

  my $host = $ENV{'GIMP_HOST'};
  $auth = $host=~s/^(.*)\@// ? $1 : undef;	# get authorization

  slog __"server version $Gimp::VERSION started".($auth ? __", authorization required" : "");

  $SIG{PIPE}='IGNORE'; # may not work, since libgimp (eech) overwrites it.        
  my($unix_path)=$default_unix_dir.$default_unix_sock;

  if ($host ne "") {
     if ($host=~s{^spawn/}{}) {
        die __"invalid GIMP_HOST: 'spawn' is not a valid connection method for the server";
     } elsif ($host=~s{^unix/}{/}) {
        $unix = local *FH;
        socket($unix,AF_UNIX,SOCK_STREAM,PF_UNSPEC)
          && bind($unix,sockaddr_un $host)
          && listen($unix,5)
            or die __"unable to create listening unix socket: $!\n";
        slog __"accepting connections in $host";
        vec($rm,fileno($unix),1)=1;
     } else {
        $host=~s{^tcp/}{};
        my($host,$port)=split /:/,$host;
        $port=$default_tcp_port unless $port;
        $tcp = local *FH;
        socket($tcp,PF_INET,SOCK_STREAM,scalar getprotobyname('tcp') || 6)
           && setsockopt($tcp,SOL_SOCKET,SO_REUSEADDR,1)
           && bind($tcp,sockaddr_in $port,INADDR_ANY)
           && listen($tcp,5)
             or die __"unable to create listening tcp socket: $!\n";
        slog __"accepting connections on port $port";
        vec($rm,fileno($tcp),1)=1;
     }
  } else {
     if ($use_unix) {
        unlink $unix_path;
        rmdir $default_unix_dir;
        mkdir $default_unix_dir,0700 or die "$!";
        $unix = local *FH;
        socket($unix,AF_UNIX,SOCK_STREAM,PF_UNSPEC)
           && bind($unix,sockaddr_un $unix_path)
           && listen($unix,5)
             or die __"unable to create listening unix socket: $!\n";
        slog __"accepting connections on $unix_path";
        vec($rm,fileno($unix),1)=1;
     }
     if ($use_tcp && $auth) {
        $tcp = local *FH;
        socket($tcp,PF_INET,SOCK_STREAM,scalar getprotobyname('tcp') || 6)
           && setsockopt($tcp,SOL_SOCKET,SO_REUSEADDR,1)
           && bind($tcp,sockaddr_in $default_tcp_port,INADDR_ANY)
           && listen($tcp,5)
             or die __"unable to create listening tcp socket: $!\n";
        slog __"accepting connections on port $default_tcp_port";
        vec($rm,fileno($tcp),1)=1;
    }
  }

  !$tcp || $auth or die __"authorization required for tcp connections";

  while(!$server_quit) {
    my $r;
    if(select($r=$rm,undef,undef,undef)>0) {
      if ($tcp && vec($r,fileno($tcp),1)) {
        my $h = local *FH;
        my ($port,$host) = sockaddr_in (accept ($h,$tcp)) or die __"unable to accept tcp connection: $!\n";
        new_connection($h);
        slog __"accepted tcp connection from ",inet_ntoa($host),":$port";
      }
      if ($unix && vec($r,fileno($unix),1)) {
        my $h = local *FH;
        accept ($h,$unix) or die __"unable to accept unix connection: $!\n";
        new_connection($h);
        slog __"accepted unix connection";
      }
      for my $f (keys(%handles)) {
        if(vec($r,$f,1)) {
          my $fh=$handles{$f};
          if(handle_request($fh)) {
            $stats{$f}[0]++;
          } else {
            slog sprintf __"closing connection %d (%d requests in %g seconds)", $f, $stats{$f}[0], time-$stats{$f}[1];
            if ($exclusive) {
               $rm = $saved_rm;
               $exclusive = 0;
               slog __"WARNING: client disconnected while holding an active lock\n";
            }
            vec($rm,$f,1)=0;
            delete $handles{$f};
            undef $fh;
          }
          last; # this is because the client might have called lock()
        }
      }
    }
  }

  slog __"server going down...";
  if ($use_tcp) {
    undef $tcp;
  }
  if ($use_unix) {
    undef $unix;
    unlink $unix_path;
    rmdir $default_unix_dir;
  }
}

1;

__END__

=head1 NAME

Gimp::Net - Communication module for the gimp-perl server.

=head1 SYNOPSIS

  use Gimp;

=head1 DESCRIPTION

For Gimp::Net (and thus commandline and remote scripts) to work, you
first have to install the "Perl-Server" extension somewhere where Gimp
can find it (e.g in your .gimp/plug-ins/ directory). Usually this is
done automatically while installing the Gimp extension. If you have a
menu entry C<<Xtns>/Perl-Server> then it is probably installed.

The Perl-Server can either be started from the C<<Xtns>> menu in Gimp,
or automatically when a perl script can't find a running Perl-Server.

When started from within The Gimp, the Perl-Server will create a unix
domain socket to which local clients can connect. If an authorization
password is given to the Perl-Server (by defining the environment variable
C<GIMP_HOST> before starting The Gimp), it will also listen on a tcp port
(default 10009). Since the password is transmitted in cleartext, using the
Perl-Server over tcp effectively B<lowers the security of your network to
the level of telnet>. Even worse: the current Gimp::Net-protocol can be
used for denial of service attacks, i.e. crashing the Perl-Server. There
also *might* be buffer-overflows (although I do care a lot for these).

=head1 ENVIRONMENT

The environment variable C<GIMP_HOST> specifies the default server to
contact and/or the password to use. The syntax is
[auth@][tcp/]hostname[:port] for tcp, [auth@]unix/local/socket/path for unix
and spawn/ for a private gimp instance. Examples are:

 www.yahoo.com               # just kidding ;)
 yahoo.com:11100             # non-standard port
 tcp/yahoo.com               # make sure it uses tcp
 authorize@tcp/yahoo.com:123 # full-fledged specification
 
 unix/tmp/unx                # use unix domain socket
 password@unix/tmp/test      # additionally use a password
 
 authorize@                  # specify authorization only
 
 spawn/                      # use a private gimp instance
 spawn/nodata                # pass --no-data switch
 spawn/gui                   # don't pass -n switch

=head1 CALLBACKS

=over 4

=item net()

is called after we have succesfully connected to the server. Do your dirty
work in this function, or see L<Gimp::Fu> for a better solution.

=back

=head1 FUNCTIONS

=over 4

=item server_quit()

sends the perl server a quit command.

=item get_connection()

return a connection id which uniquely identifies the current connection.

=item set_connection(conn_id)

set the connection to use on subsequent commands. C<conn_id> is the
connection id as returned by get_connection().

=back

=head1 AUTHOR

Marc Lehmann <pcg@goof.com>

=head1 SEE ALSO

perl(1), L<Gimp>.

=cut
