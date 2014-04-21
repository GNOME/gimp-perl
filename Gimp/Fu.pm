package Gimp::Fu;

use Gimp::Data;
use File::Basename;
use strict;
use Carp qw(croak carp);
use vars qw($run_mode @EXPORT_OK @EXPORT %EXPORT_TAGS);
use base 'Exporter';
use FindBin qw($RealBin $RealScript);
use File::stat;

# manual import
sub __ ($) { goto &Gimp::__ }

use constant {
  PF_INT8 => Gimp::PDB_INT8,
  PF_INT16 => Gimp::PDB_INT16,
  PF_INT32 => Gimp::PDB_INT32,
  PF_FLOAT => Gimp::PDB_FLOAT,
  PF_STRING => Gimp::PDB_STRING,
  PF_INT32ARRAY => Gimp::PDB_INT32ARRAY,
  PF_INT16ARRAY => Gimp::PDB_INT16ARRAY,
  PF_INT8ARRAY => Gimp::PDB_INT8ARRAY,
  PF_FLOATARRAY => Gimp::PDB_FLOATARRAY,
  PF_STRINGARRAY => Gimp::PDB_STRINGARRAY,
  PF_COLOR => Gimp::PDB_COLOR,
  PF_ITEM => Gimp::PDB_ITEM,
  PF_IMAGE => Gimp::PDB_IMAGE,
  PF_LAYER => Gimp::PDB_LAYER,
  PF_CHANNEL => Gimp::PDB_CHANNEL,
  PF_DRAWABLE => Gimp::PDB_DRAWABLE,
  PF_COLORARRAY => Gimp::PDB_COLORARRAY,
  PF_VECTORS => Gimp::PDB_VECTORS,
  PF_PARASITE => Gimp::PDB_PARASITE,
  PF_TOGGLE => Gimp::PDB_END + 1,
  PF_SLIDER => Gimp::PDB_END + 2,
  PF_FONT => Gimp::PDB_END + 3,
  PF_SPINNER => Gimp::PDB_END + 4,
  PF_ADJUSTMENT => Gimp::PDB_END + 5,
  PF_BRUSH => Gimp::PDB_END + 6,
  PF_PATTERN => Gimp::PDB_END + 7,
  PF_GRADIENT => Gimp::PDB_END + 8,
  PF_RADIO => Gimp::PDB_END + 9,
  PF_CUSTOM => Gimp::PDB_END + 10,
  PF_FILE => Gimp::PDB_END + 11,
  PF_TEXT => Gimp::PDB_END + 12,
  RUN_FULLINTERACTIVE => Gimp::RUN_INTERACTIVE+100, # you don't want to know
};
use constant {
  PF_BOOL => PF_TOGGLE,
  PF_VALUE => PF_STRING,
  PF_COLOUR => Gimp::PDB_COLOR,
};

# key is text, bit in array-ref is number!
# [int, human-description, GIMP-data-type-if>PDB_END-or-infer, passthru]
my %pfname2info = (
   PF_INT8		=> [ PF_INT8, 'integer (8-bit)', ],
   PF_INT16		=> [ PF_INT16, 'integer (16-bit)', ],
   PF_INT32		=> [ PF_INT32, 'integer (32-bit)', ],
   PF_FLOAT		=> [ PF_FLOAT, 'number', ],
   PF_STRING		=> [ PF_STRING, 'string', undef, 1 ],
   PF_INT32ARRAY	=> [ PF_INT32ARRAY, 'list of integers (32-bit)' ],
   PF_INT16ARRAY	=> [ PF_INT16ARRAY, 'list of integers (16-bit)' ],
   PF_INT8ARRAY		=> [ PF_INT8ARRAY, 'list of integers (8-bit)' ],
   PF_FLOATARRAY	=> [ PF_FLOATARRAY, 'list of numbers' ],
   PF_STRINGARRAY	=> [ PF_STRINGARRAY, 'list of strings' ],
   PF_COLOR		=> [ PF_COLOR, 'colour', ],
   PF_ITEM		=> [ PF_ITEM, 'item' ],
   PF_IMAGE		=> [ PF_IMAGE, 'image', ],
   PF_LAYER		=> [ PF_LAYER, 'layer', ],
   PF_CHANNEL		=> [ PF_CHANNEL, 'channel', ],
   PF_DRAWABLE		=> [ PF_DRAWABLE, 'drawable (%number or %a = active)', ],
   PF_COLORARRAY	=> [ PF_COLORARRAY, 'list of colours' ],
   PF_VECTORS		=> [ PF_VECTORS, 'vectors' ],
   PF_PARASITE		=> [ PF_PARASITE, 'parasite' ],
   PF_BRUSH		=> [ PF_BRUSH, 'brush', Gimp::PDB_STRING, 1 ],
   PF_GRADIENT		=> [ PF_GRADIENT, 'gradient', Gimp::PDB_STRING, 1 ],
   PF_PATTERN		=> [ PF_PATTERN, 'pattern', Gimp::PDB_STRING, 1 ],
   PF_FONT		=> [ PF_FONT, 'font', Gimp::PDB_STRING, 1 ],
   PF_TOGGLE		=> [ PF_TOGGLE, 'boolean', Gimp::PDB_INT8, ],
   PF_SLIDER		=> [ PF_SLIDER, 'number', Gimp::PDB_FLOAT, ],
   PF_SPINNER		=> [ PF_SPINNER, 'integer', Gimp::PDB_INT32, ],
   PF_ADJUSTMENT	=> [ PF_ADJUSTMENT, 'number', Gimp::PDB_FLOAT, ],
   PF_RADIO		=> [ PF_RADIO, 'data', ],
   PF_CUSTOM		=> [ PF_CUSTOM, 'string', Gimp::PDB_STRING, 1 ],
   PF_FILE		=> [ PF_FILE, 'filename', Gimp::PDB_STRING, 1 ],
   PF_TEXT		=> [ PF_TEXT, 'string', Gimp::PDB_STRING, 1 ],
);
$pfname2info{PF_COLOUR} = $pfname2info{PF_COLOR};
$pfname2info{PF_BOOL} = $pfname2info{PF_TOGGLE};
$pfname2info{PF_VALUE} = $pfname2info{PF_FLOAT};
my %pf2info = map {
   my $v = $pfname2info{$_}; ($v->[0] => [ @$v[1..3] ])
} keys %pfname2info;

@EXPORT_OK = qw($run_mode save_image);
%EXPORT_TAGS = (
   params => [ keys %pfname2info ]
);
@EXPORT = (qw(podregister register main), @{$EXPORT_TAGS{params}});

my @scripts;

# Some Standard Arguments
my @image_params = ([PF_IMAGE, "image", "Input image"],
                    [PF_DRAWABLE, "drawable", "Input drawable", '%a']);

my @load_params  = ([PF_STRING, "filename", "Filename"],
                    [PF_STRING, "raw_filename", "User-given filename"]);

my @save_params  = (@image_params, @load_params);

my $image_retval = [PF_IMAGE, "image", "Output image"];

sub interact {
   require Gimp::UI;
   goto &Gimp::UI::interact;
}

sub this_script {
   return $scripts[0] if @scripts == 1;
   # well, not-so-easy-day today
   require File::Basename;
   my ($exe) = File::Basename::fileparse($RealScript, qr/\.[^.]*/);
   my @names;
   for my $this (@scripts) {
      my $fun = $this->[0];
      $fun =~ s/^(?:perl_fu|plug_in)_//;
      return $this if lc($exe) eq lc($fun);
      push(@names,$fun);
   }
   die __"function '$exe' not found in this script (must be one of ".join(", ",@names).")\n";
}

my $latest_image;

sub string2pf($$) {
   my ($s, $type, $name, $desc) = ($_[0], @{$_[1]});
   if($pf2info{$type}->[2]) {
      $s;
   } elsif($pf2info{$type}->[0] =~ /integer/) {
      die __"$s: not an integer\n" unless $s==int($s);
      $s*1;
   } elsif($pf2info{$type}->[0] eq 'number') {
      die __"$s: not a number\n" unless $s==1.0*$s;
      $s*1.0;
   } elsif($type == PF_COLOUR) {
      Gimp::canonicalize_colour($s);
   } elsif($pf2info{$type}->[0] eq 'boolean') {
      $s?1:0;
   } elsif($type == PF_IMAGE) {
      my $image;
      if ((my $arg) = $s =~ /%(.+)/) {
	 die "Image % argument not integer - if file, put './' in front\n"
	    unless $arg eq int $arg;
	 $image = Gimp::Image->existing($arg);
	 die "'$arg' not a valid image - need to run Perl Server?\n"
	    unless $image->is_valid;
      } else {
	 $image = Gimp->file_load(Gimp::RUN_NONINTERACTIVE, $s, $s),
      }
      $latest_image = $image; # returned as well
   } elsif($type == PF_DRAWABLE) {
      if ((my $arg) = $s =~ /%(.+)/) {
	 if ($arg eq 'a') {
	    $latest_image->get_active_drawable;
	 } else {
	    # existing GIMP object - rely on autobless
	    die "Drawable % argument not integer\n"
	       unless $arg eq int $arg;
	    $arg;
	 }
      } else {
	 die "Must specify drawable as %number or %a (active)\n";
      }
   } else {
      die __"Can't convert '$name' from string to '$pf2info{$type}->[0]'\n";
   }
}

# mangle argument switches to contain only a-z0-9 and the underscore,
# for easier typing.
sub mangle_key {
   my $key = shift;
   $key=~y/A-Z /a-z_/;
   $key=~y/a-z0-9_//cd;
   $key;
}

Gimp::on_net {
   require Getopt::Long;
   my $this = this_script;
   my(%mangleparam2index,@args);
   my ($interact, $outputfile) = 1;
   my ($function,$blurb,$help,$author,$copyright,$date,
       $menupath,$imagetypes,$type,$params,$results,$perl_sub) = @$this;
   @mangleparam2index{map mangle_key($_->[1]), @$params} = (0..$#{$params});
   die "$0: error - try $0 --help\n" unless Getopt::Long::GetOptions(
      'interact|i' => sub { $interact = 1e6 },
      'output|o=s' => \$outputfile,
      map {
	 ("$_=s"=>sub {$args[$mangleparam2index{$_[0]}] = $_[1]; $interact--;})
      } keys %mangleparam2index,
   );
   die "$0: too many arguments. Try $0 --help\n" if @ARGV > @$params;
   $interact -= @ARGV;
   map { $args[$_] = $ARGV[$_] } (0..$#ARGV); # can mix & match --args and bare
   # Fill in default arguments
   foreach my $i (0..@$params-1) {
      next if defined $args[$i];
      my $entry = $params->[$i];
      $args[$i] = $entry->[3];
      die __"parameter '$entry->[1]' is not optional\n"
	 unless defined $args[$i] or $interact>0;
   }
   for my $i (0..$#args) { $args[$i] = string2pf($args[$i], $params->[$i]); }
   my $input_image = $args[0] if ref $args[0] eq "Gimp::Image";
   my @retvals = $perl_sub->(
      ($interact>0 ? RUN_FULLINTERACTIVE : Gimp::RUN_NONINTERACTIVE),
      @args
   );
   if ($outputfile) {
      my @images = grep { defined $_ and ref $_ eq "Gimp::Image" } @retvals;
      if (@images) {
	 for my $i (0..$#images) {
	    my $path = sprintf $outputfile, $i;
	    if (@images > 1 and $path eq $outputfile) {
	       $path=~s/\.(?=[^.]*$)/$i./; # insert number before last dot
	    }
	    save_image($images[$i],$path);
	    $images[$i]->delete;
	 }
      } elsif ($input_image) {
	 save_image($input_image, sprintf $outputfile, 0);
      } else {
	 die "$0: outputfile specified but plugin returned no image and no input image\n";
      }
   }
};

sub datatype(@) {
   warn __PACKAGE__."::datatype(@_)" if $Gimp::verbose;
   for(@_) {
      return Gimp::PDB_STRING unless /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/; # perlfaq4
      return Gimp::PDB_FLOAT  unless /^[+-]?\d+$/; # again
   }
   return Gimp::PDB_INT32;
}

Gimp::on_query {
   for my $s (@scripts) {
      for my $p (@{$s->[9]}) {
	 next if $p->[0] < Gimp::PDB_END;
	 $p->[0] = $pf2info{$p->[0]}->[1] // datatype(values %{+{@{$p->[4]}}});
      }
      unshift @{$s->[9]}, [Gimp::PDB_INT32,"run_mode","Interactive:0=yes,1=no"];
      Gimp->install_procedure(@$s[0..10]);
   }
};

sub podregister (&) { unshift @_, ('') x 9; goto &register; }
sub getpod ($$) {
   require Gimp::Pod; $_[0] ||= new Gimp::Pod; $_[0]->section($_[1]);
}
my %IND2SECT = (
   2 => 'DESCRIPTION', 3 => 'AUTHOR', 4 => 'LICENSE',
   5 => 'DATE', 6 => 'SYNOPSIS', 7 => 'IMAGE TYPES',
);
#(func,blurb,help,author,copyright,date,menupath,imagetypes,params,return,code)
sub register($$$$$$$$$;@) {
   no strict 'refs';
   my @p = @_;
   splice @p, 9, 0, [] if @p == 10; # optional return values
   die __"register called with too many or wrong arguments\n" unless @p == 11;
   my $pod;
   @p[0,1] = getpod($pod,'NAME') =~ /(.*?)\s*-\s*(.*)/ unless $p[0] or $p[1];
   ($p[0]) = File::Basename::fileparse($RealScript, qr/\.[^.]*/) unless $p[0];
   while (my ($k, $v) = each %IND2SECT) { $p[$k] ||= getpod($pod, $v); }
   $p[8] ||= [ eval getpod($pod, 'PARAMETERS') ]; die $@ if $@;
   $p[9] ||= [ eval getpod($pod, 'RETURN VALUES') ]; die $@ if $@;
   for my $i (0..6, 10) {
      croak "$0: Need arg $i (or POD $IND2SECT{$i} section)" unless $p[$i]
   }
   my ($function, $blurb, $help, $author, $copyright, $date,
       $menupath, $imagetypes, $params, $results, $code) = @p;
   my $type;
   if ($menupath =~ /^<Image>\//) {
      $type = Gimp::PLUGIN;
      if ($imagetypes) {
	 unshift @$params, @image_params;
      } else {
	 # undef or ''
	 unshift @$results, $image_retval
	    if !@$results or $results->[0]->[0] != PF_IMAGE;
      }
   } elsif ($menupath =~ /^<Load>\//) {
      $type = Gimp::PLUGIN;
      unshift @$params, @load_params;
      unshift @$results, $image_retval;
   } elsif ($menupath =~ /^<Save>\//) {
      $type = Gimp::PLUGIN;
      unshift @$params, @save_params;
   } elsif ($menupath =~ m#^<Toolbox>/Xtns/#) {
      $type = Gimp::PLUGIN;
      undef $imagetypes;
   } elsif ($menupath =~ /^<None>/) {
      $type = Gimp::PLUGIN;
      undef $menupath;
      undef $imagetypes;
   } else {
      die __<<EOF;
menupath _must_ start with <Image>, <Load>, <Save>, <Toolbox>/Xtns/, or <None>!
(got '$menupath')
EOF
   }

   for my $p (@$params,@$results) {
      next unless ref $p;
      croak __"$function: argument/return value '$p->[1]' has illegal type '$p->[0]'"
	unless int($p->[0]) eq $p->[0];
      carp(__"$function: argument name '$p->[1]' contains illegal characters, only 0-9, a-z and _ allowed")
	unless $p->[1]=~/^[0-9a-z_]+$/;
   }

   $function="perl_fu_".$function unless $function =~ /^(?:perl_fu_|extension_|plug_in_|file_)/ || $function =~ s/^\+//;

   $function=~/^[0-9a-z_]+(-ALT)?$/ or carp(__"$function: function name contains unusual characters, good style is to use only 0-9, a-z and _");

   carp __"function name contains dashes instead of underscores\n"
      if $function =~ y/-//;

   my $perl_sub = sub {
      $run_mode = shift;	# global!
      my(@pre,@defaults,@lastvals);

      Gimp::ignore_functions(@Gimp::GUI_FUNCTIONS)
	 unless $run_mode == Gimp::RUN_INTERACTIVE or
	        $run_mode == RUN_FULLINTERACTIVE;

      # set default arguments
      for (0..$#{$params}) {
         next if defined $_[$_];
         my $default = $params->[$_]->[3];
         $default = $default->[0] if $params->[$_]->[0] == PF_ADJUSTMENT;
         $_[$_] = $default;
      }

      for($menupath) {
         if (/^<Image>\//) {
	    if (defined $imagetypes and length $imagetypes) {
	       @_ >= 2 or die __"<Image> plug-in called without both image and drawable arguments!\n";
	       @pre = (shift,shift);
	    }
         } elsif (/^<Load>\//) {
            @_ >= 2 or die __"<Load> plug-in called without the 3 standard arguments!\n";
            @pre = (shift,shift);
         } elsif (/^<Save>\//) {
            @_ >= 4 or die __"<Save> plug-in called without the 5 standard arguments!\n";
            @pre = (shift,shift,shift,shift);
	 } elsif (m#^<Toolbox>/Xtns/#) {
	    # no-op
         } elsif (defined $_) {
	    die __"menupath _must_ start with <Image>, <Load>, <Save>, <Toolbox>/Xtns/, or <None>!";
         }
      }
      warn "perlsub: rm=$run_mode" if $Gimp::verbose;
      if ($run_mode == Gimp::RUN_INTERACTIVE
          || $run_mode == Gimp::RUN_WITH_LAST_VALS) {
         my $fudata = $Gimp::Data{"$function/_fu_data"};
	 if ($fudata) {
	    my $data_savetime = shift @$fudata;
	    my $script_savetime = stat("$RealBin/$RealScript")->mtime;
	    undef $fudata if $script_savetime > $data_savetime;
	 }
	 if ($Gimp::verbose) {
	    require Data::Dumper;
	    warn "$$-retrieved fudata: ", Data::Dumper::Dumper($fudata);
	 }

         if ($run_mode == Gimp::RUN_WITH_LAST_VALS && $fudata) {
            @_ = @$fudata;
         } else {
            if (@_) {
               # prevent the standard arguments from showing up in interact
               my @hide = splice @$params, 0, scalar @pre;

               my $res;
               ($res,@_)=interact($function,$blurb,$help,$params,@{$fudata});
               return (undef) x @$results unless $res;

               unshift @$params, @hide;
            }
         }
      } elsif ($run_mode == RUN_FULLINTERACTIVE) {
         if (@_) {
            my($res);
            ($res,@_)=interact($function,$blurb,$help,$params,@pre,@_);
            undef @pre;
            return (undef) x @$results unless $res; # right AMOUNT of nothing
         }
      } elsif ($run_mode == Gimp::RUN_NONINTERACTIVE) {
         # nop
      } else {
         die __"run_mode must be INTERACTIVE, NONINTERACTIVE or RUN_WITH_LAST_VALS\n";
      }

      if ($Gimp::verbose) {
	 require Data::Dumper;
	 warn "$$-storing fudata: ", Data::Dumper::Dumper(\@_);
      }
      $Gimp::Data{"$function/_fu_data"}=[time, @_];

      warn "$$-Gimp::Fu-generated sub: $function(",join(",",(@pre,@_)),")\n"
	 if $Gimp::verbose;

      my @retvals = $code->(@pre,@_);
      Gimp->displays_flush;
      wantarray ? @retvals : $retvals[0];
   };

   Gimp::register_callback($function,$perl_sub);
   push(@scripts,[$function,$blurb,$help,$author,$copyright,$date,
                  $menupath,$imagetypes,$type,$params,$results,$perl_sub]);
}

sub save_image($$) {
   my($img,$path)=@_;
   print "saving image $path\n" if $Gimp::verbose;
   my($flatten,$type);

   my $interlace=0;
   my $quality=0.75;
   my $smooth=0;
   my $compress=7;
   my $loop=0;
   my $delay=0;
   my $dispose=0;
   my $noextra=0;

   $_=$path=~s/^([^:]+):// ? $1 : "";
   $type=uc($1) if $path=~/\.([^.]+)$/;
   $type=uc($1) if s/^(GIF|JPG|JPEG|PNM|PNG)//i;
   while($_ ne "") {
      $interlace=$1 eq "+",	next if s/^([-+])[iI]//;
      $flatten=$1 eq "+",	next if s/^([-+])[fF]//;
      $noextra=$1 eq "+",	next if s/^([-+])[eE]//;
      $smooth=$1 eq "+",	next if s/^([-+])[sS]//;
      $quality=$1*0.01,		next if s/^-[qQ](\d+)//;
      $compress=$1,		next if s/^-[cC](\d+)//;
      $loop=$1 eq "+",		next if s/^([-+])[lL]//;
      $delay=$1,		next if s/^-[dD](\d+)//;
      $dispose=$1,		next if s/^-[pP](\d+)//;
      croak __"$_: unknown/illegal file-save option";
   }
   $flatten=(()=$img->get_layers)>1 unless defined $flatten;

   $img->flatten if $flatten;

   # always save the active layer
   my $layer = $img->get_active_layer;

   if ($type eq "JPG" or $type eq "JPEG") {
      eval { $layer->file_jpeg_save($path,$path,$quality,$smooth,1) };
      $layer->file_jpeg_save($path,$path,$quality,$smooth,1,$interlace,"",0,1,0,0) if $@;
   } elsif ($type eq "GIF") {
      unless ($layer->is_indexed) {
         eval { $img->convert_indexed(1,256) };
         $img->convert_indexed(2,Gimp::MAKE_PALETTE,256,1,1,"") if $@;
      }
      $layer->file_gif_save($path,$path,$interlace,$loop,$delay,$dispose);
   } elsif ($type eq "PNG") {
      $layer->file_png_save($path,$path,$interlace,$compress,(!$noextra) x 5);
   } elsif ($type eq "PNM") {
      $layer->file_pnm_save($path,$path,1);
   } else {
      $layer->file_save($path,$path);
   }
}

sub main {
   return Gimp::main unless $Gimp::help;
   my $this=this_script;
   print __<<EOF;
       interface-arguments are
           -o | --output <filespec>   write image to disk
           -i | --interact            let the user edit the values first
EOF
   print "       script-arguments are\n" if @{$this->[9]};
   for(@{$this->[9]}) {
      my $type=$pf2info{$_->[0]}->[0];
      my $key=mangle_key($_->[1]);
      my $default_text = defined $_->[3]
	  ? " [".(ref $_->[3] eq 'ARRAY' ? "[@{$_->[3]}]" : $_->[3])."]"
	  : "";
      printf "           --%-24s %s%s\n",
	"$key $type",
	$_->[2],
	$default_text;
   }
   0;
}

1;
__END__

=head1 NAME

Gimp::Fu - Easy framework for Gimp-Perl scripts

=head1 SYNOPSIS

  use Gimp;
  use Gimp::Fu;
  podregister {
    # your code
  };
  exit main;
  __END__
  =head1 NAME

  function_name - Short description of the function

  =head1 SYNOPSIS

  <Image>/Filters/Menu/Location...

  =head1 DESCRIPTION

  Longer description of the function...

=head1 DESCRIPTION

This module provides all the infrastructure you need to write Gimp-Perl
plugins. Dov Grobgeld has written an excellent tutorial for Gimp-Perl.
You can find it at C<http://www.gimp.org/tutorials/Basic_Perl/>.

This distribution comes with many example scripts. One is
C<examples/example-fu.pl>, which is a small Gimp::Fu-script you can take
as a starting point for your experiments. You should be able to run it
from GIMP already by looking at "Filters/Languages/_Perl/Test/Dialog".

Your main interface for using C<Gimp::Fu> is the C<podregister> function.

=head1 PODREGISTER

This:

  podregister {
    # your code
  };

does the same as this:

  register '', '', '', '', '', '', '', '', '', sub {
    # your code
  };

It extracts all the relevant values from your script's POD documentation
- see the section on L</"EMBEDDED POD DOCUMENTATION"> for an
explanation. You will also notice you don't need to provide the C<sub>
keyword, thanks to Perl's prototyping.

=head1 THE REGISTER FUNCTION

  register
    "function_name",
    "blurb", "help",
    "author", "copyright",
    "date",
    "menu path",
    "imagetypes",
    [
      [PF_TYPE,name,desc,optional-default,optional-extra-args],
      [PF_TYPE,name,desc,optional-default,optional-extra-args],
      # etc...
    ],
    [
      # like above, but for return values (optional)
    ],
    sub { code };

All these parameters except the code-ref can be replaced with C<''>, in
which case they will be substituted with appropriate values from various
sections (see below) of the POD documentation in your script.

It is B<highly> recommended you use the L</PODREGISTER> interface.

=over 2

=item function name

Defaults to the NAME section of the POD, the part B<before> the first
C<->. Falls back to the script's filename.

The PDB name of the function, i.e. the name under which it will be
registered in the GIMP database. If it doesn't start with "perl_fu_",
"file_", "plug_in_" or "extension_", it will be prepended. If you
don't want this, prefix your function name with a single "+". The idea
here is that every Gimp::Fu plug-in will be found under the common
C<perl_fu_>-prefix.

=item blurb

Defaults to the NAME section of the POD, the part B<after> the first C<->.

A one-sentence description of this script/plug-in.

=item help

Defaults to the DESCRIPTION section of the POD.

A help text describing this script. Should give more information than
C<blurb>.

=item author

Defaults to the AUTHOR section of the POD.

The name (and also the e-mail address if possible!) of the script-author.

=item copyright

Defaults to the LICENSE section of the POD.

The copyright designation for this script. Important! Save your intellectual
rights!

=item date

Defaults to the DATE section of the POD.

The "last modified" date of this script. There is no strict syntax here, but
I recommend ISO format (yyyymmdd or yyyy-mm-dd).

=item menu path

Defaults to the SYNOPSIS section of the POD.

The menu entry GIMP should create. B<Note> this is different from
Script-Fu, which asks only for which B<menu> in which to place the entry,
using the second argument to (its equivalent of) C<register> as the actual
label; here, you spell out the B<full> menu entry including label name.

It should start with one of the following:

=over 2

=item <Image>/*/Plugin-menu-label

If the plugin works on or produces an image.

If the "imagetypes" argument (see below) is defined and non-zero-length,
L<Gimp::Fu> will supply a C<PF_IMAGE> and C<PF_DRAWABLE> as the first
two parameters to the plugin.

If the plugin is intending to create an image rather than to work on
an existing one, make sure you supply C<undef> or C<""> as the
"imagetypes". In that case, L<Gimp::Fu> will supply a C<PF_IMAGE> return
value if the first return value is not a C<PF_IMAGE>.

In any case, the plugin will be installed in the specified menu location;
almost always under C<File/Create> or C<Filters>.

=item <Save>/FILETYPE

If the script is an export-handler. Make sure you also have something like:

 Gimp::on_query {
   Gimp->register_save_handler("file_filetype_save", "filetype", "");
 };

=item <Toolbox>/Xtns/Label

This will place the plugin in a special section (as of GIMP 2.8) of the
"Filters" menu. This type of plugin will also not have the image and
drawable passed, nor will it require it.

=item <None>

If the script does not need to have a menu entry.

=back

=item imagetypes

Defaults to the "IMAGE TYPES" section of the POD.

The types of images your script will accept. Examples are "RGB", "RGB*",
"GRAY, RGB" etc... Most scripts will want to use "*", meaning "any type".
Either C<undef> or "" will mean "none". Not providing the relevant POD
section is perfectly valid, so long as you intend to create and return
an image.

=item the parameter array

Defaults to the "PARAMETERS" section of the POD, passed to C<eval>, e.g.:

  =head PARAMETERS

    [ PF_COLOR, 'color', 'Colour', 'black' ],
    [ PF_FONT, 'font', 'Font', 'Arial' ],

You don't B<have> to indent it so that POD treats it as verbatim, but
it will be more readable in the Help viewer if you do.

An array reference containing parameter definitions. These are similar to
the parameter definitions used for C<gimp_install_procedure> but include an
additional B<default> value used when the caller doesn't supply one, and
optional extra arguments describing some types like C<PF_SLIDER>.

Each array element has the form C<[type, name, description, default_value,
extra_args]>.

<Image>-type plugins get two additional parameters, image (C<PF_IMAGE>)
and drawable (C<PF_DRAWABLE>) B<if and only if> the "image types"
are defined and non-zero-length. Do not specify these yourself - see
the C<menupath> entry above. Also, the C<run_mode> argument is never
given to the script but its value can be accessed in the package-global
C<$Gimp::Fu::run_mode>. The B<description> will be used in the dialog
box as a label.

See the section PARAMETER TYPES for the supported types.

The default values have an effect when called from a menu in GIMP, and
when the script is called from the command line. However, they have a
limited effect when called from Gimp::Net; data types that do not have
an "invalid" value, like text does, may not be passed as an undefined
value; this is because while Perl can use C<undef> instead of anything,
GIMP cannot. For instance, it is possible to pass a C<PF_STRING> as
undef, which will then be set to the supplied default value, but not
a C<PF_COLOR>.

=item the return values

Defaults to the "RETURN VALUES" section of the POD, passed to C<eval>.
Not providing the relevant POD section is perfectly valid, so long as
you intend to return no values.

This is just like the parameter array except that it describes the
return values. Specify the type, variable name and description only. This
argument is optional. If you wish your plugin to return an image, you
must specify that (unless your "image types" is false, see below), e.g.:

  use Gimp;
  use Gimp::Fu;
  register
     'function_name', "help", "blurb", "author", "copyright", "2014-04-11",
     "<Image>/Filters/Render/Do Something...",
     "*",
     [ [PF_INT32, "imagesize", "Image size", 1] ],
     [ [PF_IMAGE, "output image", "Output image"] ],
     sub { Gimp::Image->new($_[0], $_[0], RGB) };

If your "image types" is false, then L<Gimp::Fu> will ensure your first
return parameter is a C<PF_IMAGE>. If for some reason you need to return
an image value that will satisfy the requirement to return the right
number of values but is invalid, you can return either -1 or C<undef>.

You B<must> return the correct number (and types) of values from your
function.

=item the code

This is either an anonymous sub declaration (C<sub { your code here; }>, or a
coderef, which is called when the script is run. Arguments (including the
image and drawable for <Image> plug-ins) are supplied automatically.

You B<must> make sure your plugin returns the correct types of value, or none:

 sub {
   # no return parameters were specified
   ();
 };

If you want to display images, you must have your script do
that. Gimp::Fu will no longer automatically do that for you, so your
plugins will thereby be good GIMP "citizens", able to fit in with
plugins/filters written in other languages.

=back

=head1 PARAMETER TYPES

=over 2

=item PF_INT8, PF_INT16, PF_INT32

All mapped to sliders or spinners with suitable min/max.

=item PF_FLOAT, PF_VALUE

For C<PF_FLOAT> (or C<PF_VALUE>, a synonym), you should probably use a
C<PF_SPINNER> or C<PF_SLIDER> with suitable values.

=item PF_STRING

A string.

=item PF_COLOR, PF_COLOUR

Will accept a colour argument. In dialogs, a colour preview will be created
which will open a colour selection box when clicked. The default value
needs to be a suitable Gimp-Perl colour; see L<Gimp::canonicalize_colour>.

 [ PF_COLOR, 'colour', 'Input colour', 'white' ],
 [ PF_COLOR, 'colour2', 'Input colour 2', [ 255, 128, 0 ] ],

=item PF_IMAGE

A GIMP image.

=item PF_DRAWABLE

A GIMP drawable (channel or layer).

=item PF_TOGGLE, PF_BOOL

A boolean value (anything Perl would accept as true or false).

=item PF_SLIDER

Uses a horizontal scale. To set the range and stepsize, append an
array ref (see L<Gtk2::Adjustment> for an explanation) C<[range_min,
range_max, step_size, page_increment, page_size]> as "extra argument"
to the description array.  Default values will be substituted for missing
entries, like in:

 [PF_SLIDER, "alpha value", "the alpha value", 100, [0, 255, 1] ]

=item PF_SPINNER

The same as PF_SLIDER, except that this one uses a spinbutton instead of a
scale.

=item PF_RADIO

In addition to a default value, an extra argument describing the various
options I<must> be provided. That extra argument must be a reference
to an array filled with C<Option-Name =E<gt> Option-Value> pairs. Gimp::Fu
will then generate a horizontal frame with radio buttons, one for each
alternative. For example:

 [PF_RADIO, "direction", "direction to move to", 5, [Left => 5,  Right => 7]]]

draws two buttons, when the first (the default, "Left") is activated, 5
will be returned. If the second is activated, 7 is returned.

=item PF_FONT

Lets the user select a font whose name is returned as a string.

=item PF_BRUSH, PF_PATTERN, PF_GRADIENT

Lets the user select a brush/pattern/gradient whose name is returned as a
string. The default brush/pattern/gradient-name can be preset.

=item PF_CUSTOM

Example:

  [PF_CUSTOM, "direction", "Direction to fade(0-8)", 4, sub {
    my $btnTable = new Gtk2::Table(3,3,1);
    $btnTable->set_border_width(6);
    my $btn = new Gtk2::RadioButton;
    my ($u_direction, @buttons);
    for (my $x=0;$x<3;$x++) {
      for (my $y=0;$y<3;$y++) {
	my $dir = $x*3 + $y;
	$buttons[$dir] = $btn = Gtk2::RadioButton->new_from_widget($btn);
	$btn->set_mode(0);
	$btn->signal_connect("clicked", sub { $u_direction = $_[1]; }, $dir);
	$btn->show;
	$btnTable->attach_defaults($btn, $x, $x+1, $y, $y+1);
	my $pixmap = Gtk2::Image->new_from_pixmap(
	  Gtk2::Gdk::Pixmap->colormap_create_from_xpm_d(
	    undef, $btn->get_colormap,
	    $btn->style->bg('normal'), @{$arr[$dir]}
	  )
	);
	$pixmap->show;
	$btn->add($pixmap);
      }
    }
    $btnTable->show;
    ($btnTable, sub { $buttons[$_[0]]->clicked }, sub { $u_direction });
  },],

C<PF_CUSTOM> is for those of you requiring some non-standard-widget. You
supply a reference to code returning three values as the extra argument:

=over 2

=item C<widget>

Gtk2 widget that should be used.

=item C<settor>

Function that takes a single argument, the new value for the widget
(the widget should be updated accordingly).

=item C<gettor>

Function returning the current value of the widget.

=back

The value set and returned must be a string. For an example of this,
see C<examples/example-no-fu>.

=item PF_FILE

This represents a file system object. It usually is a file, but can be
anything (directory, link). It might not even exist at all.

=item PF_TEXT

Similar to PF_STRING, but the entry widget is much larger and has Load,
Save, and Edit (in external editor) buttons.

=back

=head1 EMBEDDED POD DOCUMENTATION

Gimp::Fu uses the Gimp::Pod module to display the full text
of the POD sections that are embedded in your scripts (see L<perlpod> for
an explanation of the POD documentation format) when the user hits the
"Help" button in the dialog box. More importantly, various sections of the
POD can be used instead of hardcoding strings in the call to C<register>.

Most of the mentioned arguments have default values (see
L</"THE REGISTER FUNCTION">) that are used when the arguments are
undefined or false, making the register call itself much shorter.

=head1 MISCELLANEOUS FUNCTIONS

=over 2

=item C<save_image(img,options_and_path)>

This is the internal function used to save images, which does more than
C<gimp_file_save>.

The C<img> is the image you want to save (which might get changed during
the operation!), C<options_and_path> denotes the filename and optional
options. If there are no options, C<save_image> tries to deduce the filetype
from the extension. The syntax for options is

 [IMAGETYPE[OPTIONS...]:]filespec

IMAGETYPE is one of GIF, JPG, JPEG, PNM or PNG, options include

 options valid for all images
 +F	flatten the image (default depends on the image)
 -F	do not flatten the image

 options for GIF and PNG images
 +I	do save as interlaced
 -I	do not save as interlaced (default)

 options for GIF animations (use with -F)
 +L	save as looping animation
 -L	save as non-looping animation (default)
 -Dn	default frame delay (default is 0)
 -Pn	frame disposal method: 0=don't care, 1 = combine, 2 = replace

 options for PNG images
 -Cn	use compression level n
 -E	Do not skip ancillary chunks (default)
 +E	Skip ancillary chunks

 options for JPEG images
 -Qn	use quality "n" to save file (JPEG only)
 -S	do not smooth (default)
 +S	smooth before saving

some examples:

 test.jpg		save the image as a simple jpeg
 JPG:test.jpg		the same
 JPG-Q70:test.jpg	the same but force a quality of 70
 GIF-I-F:test.jpg	save a gif image(!) named test.jpg
			non-interlaced and without flattening

=back

=head1 AUTHOR

Marc Lehmann <pcg@goof.com>

=head1 SEE ALSO

perl(1), L<Gimp>.
