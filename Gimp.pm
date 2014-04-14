package Gimp;

use strict 'vars';
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK $AUTOLOAD %EXPORT_TAGS @EXPORT_FAIL
            $interface_pkg $interface_type
            @PREFIXES @GUI_FUNCTIONS
            $function $basename $spawn_opts
            $in_quit $in_run $in_net $in_init $in_query $no_SIG
            $host $in_top);
use subs qw(init end lock unlock);

BEGIN {
   $VERSION = 2.3001;
   eval {
      require XSLoader;
      XSLoader::load Gimp $VERSION;
   } or do {
      require DynaLoader;
      @ISA = qw(DynaLoader);
      bootstrap Gimp $VERSION;
   };
}

use Gimp::ColorDB;
use Carp qw(croak);

@GUI_FUNCTIONS = qw(
   gimp_progress_init
   gimp_progress_update
   gimp_displays_flush
   gimp_display_new
   gimp_display_delete
);

my @_procs = ('__', 'N_');
#my @_default = (@_procs, ':consts' ,':_auto2');
my @_default = (@_procs, ':consts');
my @POLLUTE_CLASSES;
my $net_init;

sub import($;@) {
   my $pkg = shift;
   warn "$$-$pkg->import(@_)" if $Gimp::verbose;
   my $up = caller;
   my @export;

   # make sure we can call GIMP functions - start net conn if required
   map { $net_init = $1 if /net_init=(\S+)/; } @_;
   if ($interface_type eq "net" and not &Gimp::Net::initialized) {
      map { *{"Gimp::$_"} = \&{"Gimp::Constant::$_"} }
	 qw(RUN_INTERACTIVE RUN_NONINTERACTIVE);
      Gimp::Net::gimp_init(grep {defined} $net_init);
   }
   # do this here as not guaranteed access to GIMP before
   require Gimp::Constant;
   if (not defined &{$Gimp::Constant::EXPORT[-1]}) {
     warn "$$-Loading constants" if $Gimp::verbose;
     # now get constants from GIMP
     import Gimp::Constant;
   }

   @_=@_default unless @_;

   for(map { $_ eq ":DEFAULT" ? @_default : $_ } @_) {
      if ($_ eq ":auto") {
         push @export,@Gimp::Constant::EXPORT,@_procs;
         *{"$up\::AUTOLOAD"} = sub {
            croak "Cannot call '$AUTOLOAD' at this time" unless initialized();
            my ($class,$name) = $AUTOLOAD =~ /^(.*)::(.*?)$/;
            *{$AUTOLOAD} = sub { unshift @_, 'Gimp'; $AUTOLOAD = "Gimp::$name"; goto &AUTOLOAD };
            #*{$AUTOLOAD} = sub { Gimp->$name(@_) }; # old version
            goto &$AUTOLOAD;
         };
      } elsif ($_ eq ":_auto2") {
         push @export,@Gimp::Constant::EXPORT,@_procs;
         *{"$up\::AUTOLOAD"} = sub {
            warn __"$function: calling $AUTOLOAD without specifying the :auto import tag is deprecated!\n";
            croak __"Cannot call '$AUTOLOAD' at this time" unless initialized();
            my ($class,$name) = $AUTOLOAD =~ /^(.*)::(.*?)$/;
            *{$AUTOLOAD} = sub { unshift @_, 'Gimp'; $AUTOLOAD = "Gimp::$name"; goto &AUTOLOAD };
            #*{$AUTOLOAD} = sub { Gimp->$name(@_) }; # old version
            goto &$AUTOLOAD;
         };
      } elsif ($_ eq ":pollute") {
	for my $class (@POLLUTE_CLASSES) {
	  push @{"$class\::ISA"}, "Gimp::$class";
	  push @{"$class\::PREFIXES"}, @{"Gimp::$class\::PREFIXES"};
	}
      } elsif ($_ eq ":consts") {
         push @export,@Gimp::Constant::EXPORT;
      } elsif ($_ eq ":param") {
         push @export,@Gimp::Constant::PARAMS;
      } elsif (/^interface=(\S+)$/) {
         croak __"interface=... tag is no longer supported\n";
      } elsif (/spawn_options=(\S+)/) {
         $spawn_opts = $1;
      } elsif (/net_init=(\S+)/) {
	 # already used above, no-op
      } elsif ($_ ne "") {
         push(@export,$_);
      } elsif ($_ eq "") {
         #nop #d#FIXME, Perl-Server requires this!
      } else {
         croak __"$_ is not a valid import tag for package $pkg";
      }
   }

   for(@export) {
      *{"$up\::$_"} = \&$_;
   }
}

# the monadic identity function
sub N_($) { shift }

my @init_functions;

my $gtk_init = 1;

sub gtk_init() {
   if ($gtk_init) {
      require Gtk2;
      Gtk2->init;
      Gtk2::Rc->parse (Gimp->gtkrc);
      $gtk_init = 0;
   }
   &{shift @init_functions} while @init_functions;
}

sub gtk_init_hook(&) {
   push @init_functions, @_;
   unless ($gtk_init) {
      &{shift @init_functions} while @init_functions;
   }
}

# internal utility function for Gimp::Fu and others
sub wrap_text {
   my $x=$_[0];
   $x=~s/\G(.{1,$_[1]})(\s+|$)/$1\n/gm;
   $x=~s/[ \t\r\n]+$//g;
   $x;
}

# section on command-line handling/interface selection

($basename = $0) =~ s/^.*[\\\/]//;

$spawn_opts = "";

$in_query=0 unless defined $in_query;
$in_top=$in_quit=$in_run=$in_net=$in_init=0;
($function)=$0=~/([^\/\\]+)$/;

$Gimp::verbose=0 unless defined $Gimp::verbose;
# $Gimp::verbose=1;

$interface_type = "net";
if (@ARGV) {
   if ($ARGV[0] eq "-gimp") {
      $interface_type = "lib";
      # ignore other parameters completely
   } else {
      while(@ARGV) {
         $_=shift(@ARGV);
         if (/^-h$|^--?help$|^-\?$/) {
            $Gimp::help=1;
            print __<<EOF;
Usage: $0 [gimp-args..] [interface-args..] [script-args..]
           gimp-arguments are
           -h | -help | --help | -?   print some help
           -v | --verbose             be more verbose in what you do
           --host|--tcp HOST[:PORT]   connect to HOST (optionally using PORT)
                                      (for more info, see Gimp::Net(3))
EOF
         } elsif (/^-v$|^--verbose$/) {
            $Gimp::verbose++;
         } elsif (/^--host$|^--tcp$/) {
            $host=shift(@ARGV);
         } else {
            unshift(@ARGV,$_);
            last;
         }
      }
   }
}

# section on logging

my @log;

sub format_msg {
   $_=shift;
   "$_->[0]: $_->[2] ".($_->[1] ? "($_->[1])":"");
}

sub _initialized_callback {
   if (@log) {
      my $oldtrace = set_trace(0);
      unless ($in_net || $in_query || $in_quit || $in_init) {
         for(@log) {
            Gimp->message(format_msg($_)) if $_->[3];
         }
      }
      Gimp->_gimp_append_data ('gimp-perl-log', map join("\1",@$_)."\0",@log);
      @log=();
      set_trace($oldtrace);
   }
}

# message
# function
# fatal
sub logger {
   my %args = @_;
   $args{message}  = "unknown message"    unless defined $args{message};
   $args{function} = $function            unless defined $args{function};
   $args{function} = ""                   unless defined $args{function};
   $args{fatal}    = 1                    unless defined $args{fatal};
   push(@log,[$basename,@args{'function','message','fatal'}]);
   print STDERR format_msg($log[-1]),"\n" if !($in_query || $in_init || $in_quit) || $Gimp::verbose;
   _initialized_callback if initialized();
}

sub die_msg {
   logger(message => substr($_[0],0,-1), fatal => 1, function => 'ERROR');
}

# section on error-handling

# this needs to be improved
sub quiet_die {
   $in_top ? exit(1) : die "IGNORE THIS MESSAGE\n";
}

unless($no_SIG) {
   $SIG{__DIE__} = sub {
      unless ($^S || !defined $^S || $in_quit) {
         die_msg $_[0];
         initialized() ? &quiet_die : exit quiet_main();
      } else {
         die $_[0];
      }
   };

   $SIG{__WARN__} = sub {
      unless ($in_quit) {
         warn $_[0];
      } else {
         logger(message => substr($_[0],0,-1), fatal => 0, function => __"WARNING");
      }
   };
}

# section on callbacks

my %callback;

sub cbchain {
  map { $callback{$_} ? @{$callback{$_}} : (); } @_;
}

sub callback {
  my $type = shift;
  my @cb;
  if ($type eq "-run") {
    local $function = shift;
    local $in_run = 1;
    _initialized_callback;
    @cb = cbchain(qw(run lib), $function);
    die_msg __"required callback 'run' not found\n" unless @cb;
    # returning list of last func's return values
    my @retvals;
    for (@cb) {
      @retvals = &$_;
    }
    @retvals;
  } elsif ($type eq "-net") {
    local $in_net = 1;
    _initialized_callback;
    @cb = cbchain(qw(run net), $function);
    die_msg __"required callback 'net' not found\n" unless @cb;
    # returning list of last func's return values
    my @retvals;
    for (@cb) {
      @retvals = &$_;
    }
    @retvals;
  } elsif ($type eq "-query") {
    local $in_query = 1;
    _initialized_callback;
    @cb = cbchain(qw(query));
    die_msg __"required callback 'query' not found\n" unless @cb;
    for (@cb) { &$_ }
  } elsif ($type eq "-quit") {
    local $in_quit = 1;
    @cb = cbchain(qw(quit));
    for (@cb) { &$_ }
  }
}

sub register_callback($$) {
   push(@{$callback{$_[0]}},$_[1]);
}

sub on_query(&) { register_callback "query", $_[0] }
sub on_net  (&) { register_callback "net"  , $_[0] }
sub on_lib  (&) { register_callback "lib"  , $_[0] }
sub on_run  (&) { register_callback "run"  , $_[0] }

sub main {
   &{"$interface_pkg\::gimp_main"};
}

# same as main, but callbacks are ignored
sub quiet_main {
   main;
}

# section on interface_pkg

if ($interface_type=~/^lib$/i) {
   $interface_pkg="Gimp::Lib";
} elsif ($interface_type=~/^net$/i) {
   $interface_pkg="Gimp::Net";
} else {
   croak __"interface '$interface_type' unsupported.";
}
warn "$$-Using interface '$interface_type'" if $Gimp::verbose;

eval "require $interface_pkg" or croak $@;
$interface_pkg->import;
warn "$$-Finished loading '$interface_pkg'" if $Gimp::verbose;

# create some common aliases
for(qw(gimp_procedural_db_proc_exists gimp_call_procedure set_trace initialized)) {
   *$_ = \&{"$interface_pkg\::$_"};
}

*end   = \&{"$interface_pkg\::gimp_end"};
*lock  = \&{"$interface_pkg\::lock"};
*unlock= \&{"$interface_pkg\::unlock"};

# section on AUTOLOAD

my %ignore_function = (DESTROY => 1);

@PREFIXES=("gimp_", "");

sub ignore_functions(@) {
   @ignore_function{@_}++;
}

sub AUTOLOAD {
  my ($class,$name) = $AUTOLOAD =~ /^(.*)::(.*?)$/;
  warn "$$-AUTOLOAD $AUTOLOAD(@_)" if $Gimp::verbose;
  for(@{"$class\::PREFIXES"}) {
    my $sub = $_.$name;
    if (exists $ignore_function{$sub}) {
      *{$AUTOLOAD} = sub { () };
      goto &$AUTOLOAD;
    } elsif (UNIVERSAL::can(Gimp::Util,$sub)) {
      my $ref = \&{"Gimp::Util::$sub"};
      *{$AUTOLOAD} = sub {
	shift unless ref $_[0];
	#goto &$ref; # does not work, PERLBUG! #FIXME
	my @r = eval { &$ref };
	die $@ if $@; wantarray ? @r : $r[0];
      };
      goto &$AUTOLOAD;
    } elsif (UNIVERSAL::can($interface_pkg,$sub)) {
      my $ref = \&{"$interface_pkg\::$sub"};
      *{$AUTOLOAD} = sub {
	shift unless ref $_[0];
	#goto &$ref; # does not work, PERLBUG! #FIXME
	my @r = eval { &$ref };
	die $@ if $@; wantarray ? @r : $r[0];
      };
      goto &$AUTOLOAD;
    } elsif (gimp_procedural_db_proc_exists($sub)) {
      *{$AUTOLOAD} = sub {
	warn "$$-gimp_call_procedure{0}(@_)" if $Gimp::verbose;
	shift unless ref $_[0];
	unshift @_, $sub;
	#goto &gimp_call_procedure; # does not work, PERLBUG! #FIXME
	warn "$$-gimp_call_procedure{1}(@_)" if $Gimp::verbose;
	my @r = eval { gimp_call_procedure (@_) };
	die $@ if $@; wantarray ? @r : $r[0];
      };
      goto &$AUTOLOAD;
    }
  }
  croak __"function/macro \"$name\" not found in $class";
}

# section on classes

sub _pseudoclass {
  my ($class, @prefixes)= @_;
  unshift(@prefixes,"");
  *{"Gimp::$class\::AUTOLOAD"} = \&AUTOLOAD;
  push(@{"Gimp::$class\::PREFIXES"}	, @prefixes); @prefixes=@{"Gimp::$class\::PREFIXES"};
  push @POLLUTE_CLASSES, $class;
}

my @plugin_prefixes = qw(plug_in_ perl_fu_);
my @image_prefixes = (qw(gimp_image_ gimp_), @plugin_prefixes);
my @item_prefixes = (qw(gimp_item_), @image_prefixes);
my @drawable_prefixes = (qw(gimp_drawable_), @item_prefixes);

_pseudoclass qw(Item		), @item_prefixes;
_pseudoclass qw(Layer		gimp_layer_ gimp_floating_sel_), @drawable_prefixes;
_pseudoclass qw(Image		), @image_prefixes;
_pseudoclass qw(Drawable	), @drawable_prefixes;
_pseudoclass qw(Selection	gimp_selection_);
_pseudoclass qw(Vectors		gimp_vectors_);
_pseudoclass qw(Channel		gimp_channel_), @drawable_prefixes;
_pseudoclass qw(Display		gimp_display_ gimp_);
_pseudoclass qw(Plugin		), @plugin_prefixes;
_pseudoclass qw(Gradient	gimp_gradient_);
_pseudoclass qw(Gradients	gimp_gradients_);
_pseudoclass qw(Edit		gimp_edit_);
_pseudoclass qw(Progress	gimp_progress_);
_pseudoclass qw(GimpParasite	);

push @Gimp::Drawable::ISA, qw(Gimp::Item);
push @Gimp::Vectors::ISA, qw(Gimp::Item);
push @Gimp::Channel::ISA, qw(Gimp::Drawable);
push @Gimp::Layer::ISA, qw(Gimp::Drawable);

# "C"-Classes
_pseudoclass qw(GimpDrawable	gimp_gdrawable_);
_pseudoclass qw(PixelRgn	gimp_pixel_rgn_);
_pseudoclass qw(Tile		gimp_tile_);

# Classes without GIMP-Object
_pseudoclass qw(Palette		gimp_palette_);
_pseudoclass qw(Context         gimp_context_);
_pseudoclass qw(Brushes		gimp_brush_ gimp_brushes_);
_pseudoclass qw(Brush		gimp_brush_);
_pseudoclass qw(Edit		gimp_edit_);
_pseudoclass qw(Patterns	gimp_patterns_);
_pseudoclass qw(Pattern	        gimp_pattern_);

{
package Gimp::PixelRgn;

sub new($$$$$$$$) {
   shift;
   Gimp::PixelRgn->init(@_);
}
}

{
package Gimp::Parasite;

sub is_type($$)		{ $_[0]->[0] eq $_[1] }
sub is_persistent($)	{ $_[0]->[1] & &Gimp::PARASITE_PERSISTENT }
sub is_error($)		{ !defined $_[0]->[0] }
sub has_flag($$)	{ $_[0]->[1] & $_[1] }
sub copy($)		{ [@{$_[0]}] }
sub name($)		{ $_[0]->[0] }
sub flags($)		{ $_[0]->[1] }
sub data($)		{ $_[0]->[2] }
sub compare($$)		{ $_[0]->[0] eq $_[1]->[0] and
			  $_[0]->[1] eq $_[1]->[1] and
			  $_[0]->[2] eq $_[1]->[2] }
sub new($$$$)		{ shift; [@_] }
}

=head1 NAME

Gimp - a Perl extension for writing Gimp Extensions/Plug-ins/Load &
Save-Handlers

This is a release of gimp-perl for gimp-2.8.  It is not compatible with
version 2.6 or below of GIMP.

This is mostly a reference manual. For a quick intro, look at
L<Gimp::Fu>.

=head1 SYNOPSIS

  use Gimp;
  use Gimp::Fu;		# easy scripting environment

=head2 IMPORT TAGS

Place these in your C<use Gimp qw(...)> command to have added features
available to your plug-in.

=over 2

=item :auto

Import useful constants, like RGB, RUN_NONINTERACTIVE... as well as all
libgimp and pdb functions automagically into the caller's namespace.
This will overwrite your AUTOLOAD function, if you have one. The AUTOLOAD
function that gets installed must only be used in OO mode - either as
an object or a class method call - the only exception is when the first
argument is a reference (including objects):

 use Gimp qw(:auto);
 Gimp->displays_flush; # fine
 my $name = $layer->get_name; # also fine
 gimp_quit(0); # will lose its parameter, due to Perl's OO implementation!
 Gimp->quit(0); # works correctly
 gimp_image_undo_disable($image); # as does this, by a coincidence

=item :param

Import constants for plugin parameter types (PDB_INT32, PDB_STRING
etc.) only.

=item :consts

All constants found by querying GIMP (BG_IMAGE_FILL, RUN_NONINTERACTIVE,
NORMAL_MODE, PDB_INT32 etc.).

=item :pollute

In previous version of C<gimp-perl>, you could refer to GIMP classes
as either e.g. Gimp::Image, OR Image. Now in order to not pollute the
namespace, the second option will be available only when this option
is specified.

=item spawn_options=I<options>

Set default spawn options to I<options>, see L<Gimp::Net>.

=item :DEFAULT

The default set (see below).

=back

The default (unless '' is specified) is C<':consts', 'N_', '__'>.
(C<'__'> is used for i18n purposes).

=head1 GETTING STARTED

L<Gimp::Fu> is recommended for scripts not requiring custom interfaces
or specialized execution.  Lots of examples are in the C<examples/>
directory of your gimp-perl source tree, or installed in your plug-ins
directory if you are running from a package.

Using the C<Help/Procedure Browser> is a good way to learn GIMP's
Procedural Database(pdb).  For referencing functions you already know of,
the included script L<gimpdoc> is useful.

=head1 DESCRIPTION

Gimp-Perl is a module for writing plug-ins, extensions, standalone
scripts, and file-handlers for The GNU Image Manipulation Program (The
GIMP).  It can be used to automate repetitive tasks, achieve a precision
hard to get through manual use of GIMP, interface to a web server,
or other tasks that involve Gimp.

It is developed on Linux, and should work with similar OSes.

Some highlights:

=over 2

=item *
Access to GIMP's Procedural Database (pdb) for manipulation of
most objects.

=item *
Use either a plain pdb (scheme-like but with perl OO
class method) interface or a fully object-oriented syntax,
i.e. C<Gimp-E<gt>image_new(600,300,RGB)> is the same as C<new
Gimp::Image(600,300,RGB)>.

=item *
Networked plug-ins look/behave the same as those running from within gimp.

=item *
Gimp::Fu will start GIMP for you, if it cannot connect to an existing
GIMP process.

=item *
You can access the pixel-data functions using piddles (see
L<Gimp::PixelRgn>) giving the same level of control as a C plug-in,
with a data language wrapper.

=item *
Over 50 example scripts to give you a good starting point, or use as is.

=back

=head1 ARCHITECTURE

There are two modes of operation: the perl is called by GIMP (as a
plugin/filter) ("plugin mode"), or GIMP is called by perl (which uses the
Gimp::Net functionality) - either connecting to an existing GIMP process
("net mode"), or starting its own one ("batch mode").

=head2 Plugin

The perl script is written as a plug-in using C<Gimp::Fu> as described
above. The architecture is simply that Gimp, on start-up, runs all
its plug-ins at startup including all the perl scripts in its plugins
directory. The perl scripts will register themselves as GIMP "procedures"
in the PDB. When these procedures are called, typically from the menu
system, the perl script will be run and supplied with the appropriate
arguments.

=head2 From outside GIMP

The script will use C<Gimp> as above, and use Gimp functions as it
wishes. If you are using GIMP interactively, you need to run the Perl
server (under "Filters/Perl" to allow your script to connect. Otherwise,
the script will start its own GIMP, in "batch mode".  Either way,
your script, when it uses GIMP procedures (and Gimp-Perl functions),
will actually be communicating with the perl server running under GIMP.

The architecture may be visualised like this:

 perlscript <-> Gimp::Net <-> Perl-Server <-> Gimp::Lib <-> GIMP

This has certain consequences; native GIMP objects like images and layers
obviously persist between Perl method calls, but C<libgimp> entities such
as C<GimpDrawable>, with the perl interface L<Gimp::PixelRgn>, require
special handling. Currently they do not work when used over C<Gimp::Net>.

=head1 OUTLINE OF A GIMP PLUG-IN

All plug-ins (running in "plugin mode") I<must> finish with a call to
C<Gimp::main>.

The return code should be immediately handed out to exit:

 exit Gimp::main;

Before the call to C<Gimp::main>, I<no> other PDB function must be called.

In a C<Gimp::Fu>-script, it will actually call C<Gimp::Fu::main> instead
of C<Gimp::main>:

 exit main; # Gimp::Fu::main is exported by default when using Gimp::Fu

This is similar to Gtk, Tk or similar modules, where you have to call the
main eventloop.

Although you call C<exit> with the result of C<main>, the main function
might not actually return. This depends on both the version of GIMP and
the version of the Gimp-Perl module that is in use.  Do not depend on
C<main> to return at all, but still call C<exit> immediately.

=head2 CALLBACKS

The C<Gimp> module provides routines to be optionally filled in by a
plug-in writer.  This does not apply if using C<Gimp::Fu>, as these are
done automatically.

=over 2

=item Gimp::on_query

Do any activities that must be performed at Gimp startup, when the
procedure is queried.  Should typically have at least one call to
Gimp->install_procedure.

=item Gimp::on_net

Run when called from a network interface (from the Perl-Server or from
running it standalone).

=item Gimp::on_lib

Run only when called from within Gimp.

=item Gimp::on_run

Run when anything calls it (network or lib).

=back

=head1 OUTLINE OF A GIMP EXTENSION

A GIMP extension is a special type of plugin. Once started, it stays
running all the time. Typically during its run-initialisation (not on
query) it will install temporary procedures.

If it has no parameters, then rather than being run when called, either
from a menu or a scripting interface, it is run at GIMP startup.

A working, albeit trivial, example is provided in
examples/example-extension. A summarised example:

  use Gimp;
  Gimp::register_callback extension_gp_test => sub {
    # do some relevant initialisation here
    Gimp->install_temp_proc(
      "perl_fu_temp_demo", "help", "blurb", "id", "id", "2014-04-11",
      "<Toolbox>/Xtns/Perl/Test/Temp Proc demo", undef,
      &Gimp::TEMPORARY,
      [ [ &Gimp::PDB_INT32, 'run_mode', 'Run-mode', 0 ], ],
      [],
    );
    Gimp->extension_ack;
    while (1) {
      Gimp->extension_process(0);
    }
  };
  Gimp::register_callback perl_fu_temp_demo => sub {
    my ($run_mode) = @_;
    # here could bring up UI if $run_mode == RUN_INTERACTIVE
  };
  Gimp::on_query {
     Gimp->install_procedure(
	"extension_gp_test", "help", "blurb", "id", "id", "2014-04-11",
	undef, undef,
	&Gimp::EXTENSION,
	[], [],
     );
  };
  exit Gimp::main;

=head1 CALLING GIMP FUNCTIONS

There are two different flavours of gimp-functions. Functions from the
B<PDB> (the Procedural DataBase), and functions from B<libgimp> (the
C-language interface library).

You can get a listing and description of every PDB function by starting
the B<DB Browser> extension in GIMP's B<Xtns> menu (but remember to change
 "-" (dashes) to  "_" (underscores)).

C<libgimp> functions can't be traced (and won't be traceable in the
foreseeable future).

To call pdb functions (or equivalent libgimp functions), just treat them like
normal perl (this requires the use of the C<:auto> import tag - see
the import C<:auto> note for non-OO limitation; see below for another
possibility!):

 Gimp->palette_set_foreground([20,5,7]);
 Gimp->palette_set_background("cornsilk");

If you don't use the C<:auto> import tag, you can call all Gimp functions
using OO-Syntax:

 Gimp->palette_set_foreground([20,5,7]);
 Gimp->palette_set_background("cornsilk");
 Gimp::Palette->set_foreground('#1230f0');

As you can see, you can also drop part of the name prefixes with this
syntax, so it's actually shorter to write and hopefully clearer to read.

=head1 SPECIAL FUNCTIONS

In this section, you can find descriptions of special functions, functions
having different calling conventions/semantics than might be expected
or otherwise interesting functions. All of these functions must either
be imported explicitly or called using a namespace override (C<Gimp::>),
not as Methods (C<Gimp-E<gt>>).

=over 4

=item Gimp::main()

Should be called immediately when perl is initialized. Arguments are not
supported. Initializations can later be done in the init function.

=item Gimp::gtk_init()

Initialize Gtk in a similar way GIMP itself did it. This automatically
parses gimp's gtkrc and sets a variety of default settings (visual,
colormap, gamma, shared memory...).

=item Gimp::gtk_init_add { init statements ... };

Add a callback function that should be called when gtk is being
initialized (i.e. when Gimp::gtk_init is called, which should therefore be
done even in Gnome applications).

This is different to Gtk->init_add, which only gets called in Gtk->main,
which is too late for registering types.

This function has not been well tested.

=item use Gimp qw(net_init=...);

This is how to use Gimp-Perl in "net mode". Previous versions of this
package required a call to Gimp::init. This is no longer necessary. The
technical reason for this change is that when C<Gimp.pm> loads, it must
connect to GIMP to load its constants, like C<PDB_INT32>.

=item Gimp::lock(), Gimp::unlock()

These functions can be used to gain exclusive access to GIMP. After
calling lock, all accesses by other clients will be blocked and executed
after the call to unlock. Calls to lock and unlock can be nested.

Currently, these functions only lock the current Perl-Server instance
against exclusive access, they do nothing when used via the Gimp::Lib
interface.

=item Gimp::set_rgb_db(filespec)

Use the given rgb database instead of the default one. The format is
the same as the one used by the X11 Consortiums rgb database (you might
have a copy in /usr/lib/X11/rgb.txt). You can view the default database
with C<perldoc -m Gimp::ColorDB>, at the end of the file (the default
database is similar, but not identical to the X11 default rgb.txt)

=item Gimp::initialized()

this function returns true whenever it is safe to call gimp functions. This is
usually only the case after gimp_main has been called.

=item Gimp::register_callback(gimp_function_name, perl_function)

Using this function you can overwrite the standard Gimp behaviour of
calling a perl subroutine of the same name as the GIMP function.

The first argument is the name of a registered gimp function that you want
to overwrite ('perl_fu_make_something'), and the second argument can be
either a name of the corresponding perl sub (C<'Elsewhere::make_something'>)
or a code reference (C<\&my_make>).

=item Gimp::canonicalize_colour/Gimp::canonicalize_color

Take in a color specifier in a variety of different formats, and return
a valid GIMP color specifier (a C<GimpRGB>), consisting of 3 or 4 numbers
in the range between 0 and 1.0.

For example:

 $color = canonicalize_colour ("#ff00bb"); # html format
 $color = canonicalize_colour ([255,255,34]); # RGB
 $color = canonicalize_colour ([255,255,34,255]); # RGBA
 $color = canonicalize_colour ([1.0,1.0,0.32]); # actual GimpRGB
 $color = canonicalize_colour ('red'); # uses the color database

Note that bounds checking is somewhat lax; this assumes relatively
good input.

=back

=head1 SPECIAL METHODS

This chapter describes methods that behave differently than you might
expect, or methods uniquely implemented in perl (that is, not in the
PDB). All of these must be invoked using the method syntax (C<Gimp-E<gt>>
or C<$object-E<gt>>).

=over 4

=item Gimp->install_procedure(name, blurb, help, author, copyright, date, menu_path, image_types, type, [params], [return_vals])

Mostly same as gimp_install_procedure from the C library. The
parameters and return values for the functions are specified as an
array ref containing either integers or array-refs with three elements,
[PARAM_TYPE, \"NAME\", \"DESCRIPTION\"].

=item Gimp::Progress->init(message,[])

Initializes a progress bar. In networked modules this is a no-op.

=item Gimp::Progress->update(percentage)

Updates the progress bar. No-op in networked modules.

=item gimp_tile_*, gimp_pixel_rgn_*, gimp_drawable_get

With these functions you can access the raw pixel data of drawables. They
are documented in L<Gimp::PixelRgn>, to keep this manual page short.

=item gimp_call_procedure(procname, arguments...)

This function is actually used to implement the fancy stuff. It's your basic
interface to the PDB. Every function call is eventually done through his
function, i.e.:

 gimp_image_new(args...);

is replaced by

 gimp_call_procedure "gimp_image_new",args...;

at runtime.

=item gimp_list_images, gimp_image_get_layers, gimp_image_get_channels

These functions return what you would expect: an array of images, layers or
channels. The reason why this is documented is that the usual way to return
C<PDB_INT32ARRAY>'s would be to return a B<reference> to an B<array of
integers>, rather than blessed objects.

=item gimp_drawable_bounds drawable/gdrawable

Returns an array (x,y,w,h) containing the upper left corner and the
size of currently selected parts of the drawable, just as needed by
Gimp::PixelRgn->new and similar functions.

=item server_eval(string)

This evaluates the given string in array context and returns the
results. It's similar to C<eval>, but with two important differences: the
evaluating always takes place on the server side/server machine (which
might be the same as the local one) and compilation/runtime errors are
reported as runtime errors (i.e. throwing an exception).

=back

=head1 OBJECT ORIENTED SYNTAX

In this manual, only the plain syntax (that lesser languages like C use)
is described. See L<Gimp::OO> for details on using the object oriented
syntax.  The 'gimpdoc' script will also return OO varients when functions
are described.  For example:

gimpdoc image_new

has a section:

SOME SYNTAX ALTERNATIVES
       $image = Gimp->image_new (width,height,type)
       $image = new Gimp::Image (width,height,type)
       $image = image_new Gimp::Display (width,height,type)

=head1 DEBUGGING AIDS

How to debug your scripts:

=over 4

=item $Gimp::verbose

If set to true, will make Gimp say what it's doing on STDOUT. It will
also stop L<Gimp::Net>'s normal behaviour of the server-side closing
STDIN, STDOUT and STDERR. If you want it to be set during loading Gimp.pm,
make sure to do so in a prior C<BEGIN> block:

 BEGIN { $Gimp::verbose = 1; }
 use Gimp;

=item Gimp::set_trace (tracemask)

Tracking down bugs in gimp scripts is difficult, due to a lack of
reasonable error messages.  Often, if anything goes wrong, you only get
an execution failure.

You can switch on tracing to see which parameters are used to call pdb
functions, so you can at least see what was called to cause the error.

This function is never exported, so you have to qualify it when calling.

tracemask is any number of the following flags or'ed together.

=over 8

=item TRACE_NONE

nothing is printed (default).

=item TRACE_CALL

all pdb calls (and only pdb calls!) are printed
with arguments and return values.

=item TRACE_TYPE

the parameter types are printed additionally.

=item TRACE_NAME

the parameter names are printed.

=item TRACE_DESC

the parameter descriptions.

=item TRACE_ALL

all of the above.

=back

C<set_trace> returns the old tracemask.

=item Gimp::set_trace(\$tracevar)

write trace into $tracevar instead of printing it to STDERR. $tracevar only
contains the last command traces, i.e. it's cleared on every PDB invocation
invocation.

=item Gimp::set_trace(*FILEHANDLE)

write trace to FILEHANDLE instead of STDERR.

=item GLib debugging

GIMP makes use of GLib. Environment variables including
C<G_DEBUG>, and setting C<G_SLICE> to
C<always-malloc>, control some behaviour. See
L<https://developer.gnome.org/glib/unstable/glib-running.html>
for details. Additionally, the behaviour of C<malloc> can
be controlled with other environment variables as shown at
L<http://man7.org/linux/man-pages/man3/mallopt.3.html>, especially
setting C<MALLOC_CHECK_> (note trailing underscore) to 3.

=back

=head1 SUPPORTED GIMP DATA TYPES

Gimp supports different data types like colors, regions, strings. In
perl, these are represented as:

=over 4

=item INT32, INT16, INT8, FLOAT, STRING

normal perl scalars. Anything except STRING will be mapped
to a perl-double.

=item INT32ARRAY, INT16ARRAY, INT8ARRAY, FLOATARRAY, STRINGARRAY, COLORARRAY

array refs containing scalars of the same type, i.e. [1, 2, 3, 4]. Gimp
implicitly swallows or generates a preceeding integer argument because the
preceding argument usually (this is a de-facto standard) contains the number
of elements.

=item COLOR

on input, either an array ref with 3 or 4 elements (i.e. [0.1,0.4,0.9]
or [233,40,40]), a X11-like string ("#rrggbb") or a colour name
("papayawhip") (see set_rgb_db).

=item DISPLAY, IMAGE, LAYER, CHANNEL, DRAWABLE, SELECTION, VECTORS, ITEM

these will be mapped to corresponding objects (IMAGE => Gimp::Image). In
trace output you will see small integers (the image/layer/etc..-ID)

=item PARASITE

represented as an array ref [name, flags, data], where name and data
should be perl strings and flags is the numerical flag value.

=item STATUS

Not yet supported, except implicitly - this is how exceptions (from
"die") get returned in "net mode".

=back

=head1 AUTHOR

Marc Lehmann <pcg@goof.com> (pre-2.0)

Seth Burgess <sjburge@gimp.org> (2.0+)

Ed J (with oversight and guidance from Kevin Cozens) (2.3+)

=head1 SEE ALSO

perl(1), gimp(1), L<Gimp::OO>, L<Gimp::Data>, L<Gimp::PixelRgn>,
L<Gimp::Util>, L<Gimp::UI>, L<Gimp::Config>, L<Gimp::Net>, and L<Gimp::Lib>.

=cut

1;
