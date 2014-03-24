package Gimp;

use strict 'vars';
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK $AUTOLOAD %EXPORT_TAGS @EXPORT_FAIL
            $interface_pkg $interface_type
            @PREFIXES $_PROT_VERSION
            @gimp_gui_functions $function $basename $spawn_opts
            $in_quit $in_run $in_net $in_init $in_query $no_SIG
            $help $verbose $host $in_top);
use subs qw(init end lock unlock canonicalize_color);

BEGIN {
   $VERSION = 2.3001;
   eval {
      require XSLoader;
      XSLoader::load Gimp $VERSION;
   } or do {
      require DynaLoader;
      @ISA = qw(DynaLoader);
      bootstrap Gimp $VERSION;
   }
}

my @_param = qw(
	PDB_BOUNDARY	PDB_CHANNEL	PDB_COLOR	PDB_DISPLAY	PDB_DRAWABLE
	PDB_END		PDB_FLOAT	PDB_IMAGE	PDB_INT32	PDB_FLOATARRAY
	PDB_INT16	PDB_PARASITE	PDB_STRING	PDB_PATH	PDB_INT16ARRAY
	PDB_INT8	PDB_INT8ARRAY	PDB_LAYER	PDB_REGION	PDB_STRINGARRAY
	PDB_SELECTION	PDB_STATUS	PDB_INT32ARRAY
);

# constants that, in some earlier version, were autoloaded
my @_consts = (@_param,
#ENUM_NAME#
'NEGATIVE_CONVOL',       'ABSOLUTE_CONVOL',       'NORMAL_CONVOL',         'DODGE',                 
'BURN',                  'PDB_SUCCESS',           'PDB_CANCEL',            'PDB_CALLING_ERROR',     
'PDB_PASS_THROUGH',      'PDB_EXECUTION_ERROR',   'SHARPEN_CONVOLVE',      'BLUR_CONVOLVE',         
'PAINT_INCREMENTAL',     'PAINT_CONSTANT',        'ORIENTATION_HORIZONTAL','ORIENTATION_UNKNOWN',   
'ORIENTATION_VERTICAL',  'OFFSET_TRANSPARENT',    'OFFSET_BACKGROUND',     'GRADIENT_SPIRAL_CLOCKWISE',      
'GRADIENT_CONICAL_ASYMMETRIC',    'GRADIENT_SPIRAL_ANTICLOCKWISE',  'GRADIENT_SHAPEBURST_DIMPLED',    'GRADIENT_CONICAL_SYMMETRIC',     
'GRADIENT_BILINEAR',              'GRADIENT_LINEAR',                'GRADIENT_RADIAL',                'GRADIENT_SHAPEBURST_ANGULAR',    
'GRADIENT_SHAPEBURST_SPHERICAL',  'GRADIENT_SQUARE',                'PDB_COLOR',             'PDB_END',               
'PDB_LAYER',             'PDB_INT8ARRAY',         'PDB_PATH',              'PDB_INT16',             
'PDB_FLOAT',             'PDB_DISPLAY',           'PDB_STRING',            'PDB_INT16ARRAY',        
'PDB_IMAGE',             'PDB_INT32',             'PDB_SELECTION',         'PDB_STATUS',            
'PDB_STRINGARRAY',       'PDB_CHANNEL',           'PDB_FLOATARRAY',        'PDB_INT32ARRAY',        
'PDB_DRAWABLE',          'PDB_BOUNDARY',          'PDB_REGION',            'PDB_PARASITE',          
'PDB_INT8',              'HISTOGRAM_BLUE',        'HISTOGRAM_VALUE',       'HISTOGRAM_GREEN',             
'HISTOGRAM_ALPHA',       'HISTOGRAM_RED',         'SHADOWS',               'HIGHLIGHTS',            
'MIDTONES',              'POINTS',                'PIXELS',                'STACK_TRACE_ALWAYS',    
'STACK_TRACE_NEVER',     'STACK_TRACE_QUERY',     'GRAIN_EXTRACT_MODE',    'COLOR_ERASE_MODE',      
'COLOR_MODE',            'LIGHTEN_ONLY_MODE',     'DIFFERENCE_MODE',       'DODGE_MODE',            
'BEHIND_MODE',           'ADDITION_MODE',         'MULTIPLY_MODE',         'GRAIN_MERGE_MODE',      
'SOFTLIGHT_MODE',        'HUE_MODE',              'SCREEN_MODE',           'BURN_MODE',             
'NORMAL_MODE',           'OVERLAY_MODE',          'HARDLIGHT_MODE',        'DARKEN_ONLY_MODE',      
'SUBTRACT_MODE',         'SATURATION_MODE',       'DISSOLVE_MODE',         'VALUE_MODE',            
'DIVIDE_MODE',           'IMAGE_CLONE',           'PATTERN_CLONE',         'UNIT_PIXEL',            
'UNIT_PICA',             'UNIT_END',              'UNIT_POINT',            'UNIT_MM',               
'UNIT_INCH',             'RGB_IMAGE',             'INDEXEDA_IMAGE',        'INDEXED_IMAGE',         
'GRAYA_IMAGE',           'RGBA_IMAGE',            'GRAY_IMAGE',            'MESSAGE_BOX',           
'ERROR_CONSOLE',         'CONSOLE',               'BRUSH_SOFT',            'BRUSH_HARD',            
'PLUGIN',                'TEMPORARY',             'EXTENSION',             'INTERNAL',              
'INDEXED',               'GRAY',                  'RGB',                   'CYAN_HUES',             
'ALL_HUES',              'BLUE_HUES',             'MAGENTA_HUES',          'GREEN_HUES',            
'YELLOW_HUES',           'RED_HUES',              'WEB_PALETTE',           'REUSE_PALETTE',         
'MONO_PALETTE',          'CUSTOM_PALETTE',        'MAKE_PALETTE',          'FSLOWBLEED_DITHER',     
'NO_DITHER',             'FIXED_DITHER',          'FS_DITHER',             'PATTERN_BUCKET_FILL',   
'FG_BUCKET_FILL',        'BG_BUCKET_FILL',        'INDEXED_CHANNEL',       'BLUE_CHANNEL',          
'ALPHA_CHANNEL',         'GRAY_CHANNEL',          'RED_CHANNEL',           'GREEN_CHANNEL',         
'RUN_NONINTERACTIVE',    'RUN_WITH_LAST_VALS',    'RUN_INTERACTIVE',       'REPEAT_NONE',           
'REPEAT_TRIANGULAR',     'REPEAT_SAWTOOTH',       'MASK_DISCARD',          'MASK_APPLY',            
'ADD_WHITE_MASK',        'ADD_SELECTION_MASK',    'ADD_ALPHA_TRANSFER_MASK', 'ADD_BLACK_MASK',        'ADD_ALPHA_MASK',        
'ADD_COPY_MASK',         'TRANSPARENT_FILL',      'BACKGROUND_FILL',       'FOREGROUND_FILL',       
'WHITE_FILL',            'PATTERN_FILL',          'CHANNEL_OP_SUBTRACT',   'CHANNEL_OP_REPLACE',    
'CHANNEL_OP_INTERSECT',  'CHANNEL_OP_ADD',        'FLATTEN_IMAGE',         'EXPAND_AS_NECESSARY',   
'CLIP_TO_BOTTOM_LAYER',  'CLIP_TO_IMAGE',         'CUSTOM_MODE',           'FG_TRANSPARENT_MODE',   
'FG_BG_HSV_MODE',        'FG_BG_RGB_MODE',        
#ENUM_NAME#
	'PDB_CALLING_ERROR',		'PDB_EXECUTION_ERROR',		'PDB_PASS_THROUGH',
        'PDB_SUCCESS',			'PARASITE_PERSISTENT',		'PARASITE_ATTACH_PARENT',
        'PARASITE_PARENT_PERSISTENT',	'PARASITE_ATTACH_GRANDPARENT',	'PARASITE_GRANDPARENT_PERSISTENT',
        'PARASITE_UNDOABLE',		'PARASITE_PARENT_UNDOABLE',	'PARASITE_GRANDPARENT_UNDOABLE',
	'TRACE_NONE',	'TRACE_CALL',	'TRACE_TYPE',	'TRACE_NAME',	'TRACE_DESC',	'TRACE_ALL',
	'COMPRESSION_NONE',		'COMPRESSION_LZW',		'COMPRESSION_PACKBITS',
        'WRAP',				'SMEAR',			'BLACK',

	'EXPORT_CAN_HANDLE_RGB',	'EXPORT_CAN_HANDLE_GRAY',	'EXPORT_CAN_HANDLE_INDEXED',
        'EXPORT_CAN_HANDLE_BITMAP',     'EXPORT_CAN_HANDLE_ALPHA',	'EXPORT_CAN_HANDLE_LAYERS',	
	'EXPORT_CAN_HANDLE_LAYERS_AS_ANIMATION', 'EXPORT_CAN_HANDLE_LAYER_MASKS', 'EXPORT_NEEDS_ALPHA',
	'EXPORT_CANCEL',		'EXPORT_IGNORE',		'EXPORT_EXPORT', 
	'ROTATE_90', 			'ROTATE_270',			'ROTATE_180',
	'TRUE', 			'FALSE'
);

#ENUM_DEFS#
sub NEGATIVE_CONVOL       (){ 2} sub ABSOLUTE_CONVOL       (){ 1} sub NORMAL_CONVOL         (){ 0}
sub DODGE                 (){ 0} sub BURN                  (){ 1} sub PDB_SUCCESS           (){ 3}
sub PDB_CANCEL            (){ 4} sub PDB_CALLING_ERROR     (){ 1} sub PDB_PASS_THROUGH      (){ 2}
sub PDB_EXECUTION_ERROR   (){ 0} sub SHARPEN_CONVOLVE      (){ 1} sub BLUR_CONVOLVE         (){ 0}
sub PAINT_INCREMENTAL     (){ 1} sub PAINT_CONSTANT        (){ 0}
sub ORIENTATION_HORIZONTAL(){ 0} sub ORIENTATION_UNKNOWN   (){ 2} sub ORIENTATION_VERTICAL  (){ 1}
sub OFFSET_TRANSPARENT    (){ 1} sub OFFSET_BACKGROUND     (){ 0} sub GRADIENT_SPIRAL_CLOCKWISE      (){ 9}
sub GRADIENT_CONICAL_ASYMMETRIC    (){ 5} sub GRADIENT_SPIRAL_ANTICLOCKWISE  (){10} sub GRADIENT_SHAPEBURST_DIMPLED    (){ 8}
sub GRADIENT_CONICAL_SYMMETRIC     (){ 4} sub GRADIENT_BILINEAR              (){ 1} sub GRADIENT_LINEAR                (){ 0}
sub GRADIENT_RADIAL                (){ 2} sub GRADIENT_SHAPEBURST_ANGULAR    (){ 6} sub GRADIENT_SHAPEBURST_SPHERICAL  (){ 7}
sub SQUARE                (){ 3} sub PDB_COLOR             (){10} sub PDB_END               (){22}
sub PDB_LAYER             (){14} sub PDB_INT8ARRAY         (){ 7} sub PDB_PATH              (){19}
sub PDB_INT16             (){ 1} sub PDB_FLOAT             (){ 3} sub PDB_DISPLAY           (){12}
sub PDB_STRING            (){ 4} sub PDB_INT16ARRAY        (){ 6} sub PDB_IMAGE             (){13}
sub PDB_INT32             (){ 0} sub PDB_SELECTION         (){17} sub PDB_STATUS            (){21}
sub PDB_STRINGARRAY       (){ 9} sub PDB_CHANNEL           (){15} sub PDB_FLOATARRAY        (){ 8}
sub PDB_INT32ARRAY        (){ 5} sub PDB_DRAWABLE          (){16} sub PDB_BOUNDARY          (){18}
sub PDB_REGION            (){11} sub PDB_PARASITE          (){20} sub PDB_INT8              (){ 2}
sub HISTOGRAM_BLUE        (){ 3} sub HISTOGRAM_VALUE       (){ 0} sub HISTOGRAM_GREEN       (){ 2}
sub HISTOGRAM_ALPHA       (){ 4} sub HISTOGRAM_RED         (){ 1} sub SHADOWS               (){ 0}
sub HIGHLIGHTS            (){ 2} sub MIDTONES              (){ 1} sub POINTS                (){ 1}
sub PIXELS                (){ 0} sub STACK_TRACE_ALWAYS    (){ 2} sub STACK_TRACE_NEVER     (){ 0}
sub STACK_TRACE_QUERY     (){ 1} sub GRAIN_EXTRACT_MODE    (){20} sub COLOR_ERASE_MODE      (){22}
sub COLOR_MODE            (){13} sub LIGHTEN_ONLY_MODE     (){10} sub DIFFERENCE_MODE       (){ 6}
sub DODGE_MODE            (){16} sub BEHIND_MODE           (){ 2} sub ADDITION_MODE         (){ 7}
sub MULTIPLY_MODE         (){ 3} sub GRAIN_MERGE_MODE      (){21} sub SOFTLIGHT_MODE        (){19}
sub HUE_MODE              (){11} sub SCREEN_MODE           (){ 4} sub BURN_MODE             (){17}
sub NORMAL_MODE           (){ 0} sub OVERLAY_MODE          (){ 5} sub HARDLIGHT_MODE        (){18}
sub DARKEN_ONLY_MODE      (){ 9} sub SUBTRACT_MODE         (){ 8} sub SATURATION_MODE       (){12}
sub DISSOLVE_MODE         (){ 1} sub VALUE_MODE            (){14} sub DIVIDE_MODE           (){15}
sub IMAGE_CLONE           (){ 0} sub PATTERN_CLONE         (){ 1} sub UNIT_PIXEL            (){ 0}
sub UNIT_PICA             (){ 4} sub UNIT_END              (){ 5} sub UNIT_POINT            (){ 3}
sub UNIT_MM               (){ 2} sub UNIT_INCH             (){ 1} sub RGB_IMAGE             (){ 0}
sub INDEXEDA_IMAGE        (){ 5} sub INDEXED_IMAGE         (){ 4} sub GRAYA_IMAGE           (){ 3}
sub RGBA_IMAGE            (){ 1} sub GRAY_IMAGE            (){ 2} sub MESSAGE_BOX           (){ 0}
sub ERROR_CONSOLE         (){ 2} sub CONSOLE               (){ 1} sub BRUSH_SOFT            (){ 1}
sub BRUSH_HARD            (){ 0} sub PLUGIN                (){ 1} sub TEMPORARY             (){ 3}
sub EXTENSION             (){ 2} sub INTERNAL              (){ 0} sub INDEXED               (){ 2}
sub GRAY                  (){ 1} sub RGB                   (){ 0} sub CYAN_HUES             (){ 4}
sub ALL_HUES              (){ 0} sub BLUE_HUES             (){ 5} sub MAGENTA_HUES          (){ 6}
sub GREEN_HUES            (){ 3} sub YELLOW_HUES           (){ 2} sub RED_HUES              (){ 1}
sub WEB_PALETTE           (){ 2} sub REUSE_PALETTE         (){ 1} sub MONO_PALETTE          (){ 3}
sub CUSTOM_PALETTE        (){ 4} sub MAKE_PALETTE          (){ 0} sub FSLOWBLEED_DITHER     (){ 2}
sub NO_DITHER             (){ 0} sub FIXED_DITHER          (){ 3} sub FS_DITHER             (){ 1}
sub PATTERN_BUCKET_FILL   (){ 2} sub FG_BUCKET_FILL        (){ 0} sub BG_BUCKET_FILL        (){ 1}
sub INDEXED_CHANNEL       (){ 4} sub BLUE_CHANNEL          (){ 2} sub ALPHA_CHANNEL         (){ 5}
sub GRAY_CHANNEL          (){ 3} sub RED_CHANNEL           (){ 0} sub GREEN_CHANNEL         (){ 1}
sub RUN_NONINTERACTIVE    (){ 1} sub RUN_WITH_LAST_VALS    (){ 2} sub RUN_INTERACTIVE       (){ 0}
sub REPEAT_NONE           (){ 0} sub REPEAT_TRIANGULAR     (){ 2} sub REPEAT_SAWTOOTH       (){ 1}
sub MASK_DISCARD          (){ 1} sub MASK_APPLY            (){ 0} sub ADD_WHITE_MASK        (){ 0}
sub ADD_ALPHA_TRANSFER_MASK (){3 }
sub ADD_SELECTION_MASK    (){ 4} sub ADD_BLACK_MASK        (){ 1} sub ADD_ALPHA_MASK        (){ 2}
sub ADD_COPY_MASK         (){ 5} sub TRANSPARENT_FILL      (){ 3} sub BACKGROUND_FILL       (){ 1}
sub FOREGROUND_FILL       (){ 0} sub WHITE_FILL            (){ 2} sub NO_FILL               (){ 4}
sub CHANNEL_OP_SUBTRACT   (){ 1} sub CHANNEL_OP_REPLACE    (){ 2} sub CHANNEL_OP_INTERSECT  (){ 3}
sub CHANNEL_OP_ADD        (){ 0} sub FLATTEN_IMAGE         (){ 3} sub EXPAND_AS_NECESSARY   (){ 0}
sub CLIP_TO_BOTTOM_LAYER  (){ 2} sub CLIP_TO_IMAGE         (){ 1} sub CUSTOM_MODE           (){ 3}
sub FG_TRANSPARENT_MODE   (){ 2} sub FG_BG_HSV_MODE        (){ 1} sub FG_BG_RGB_MODE        (){ 0}
sub ROTATE_90             (){ 0} sub ROTATE_270            (){ 2} sub ROTATE_180            (){ 1}
#ENUM_DEFS#

sub WRAP		(){ 0 }
sub SMEAR		(){ 1 }
sub BLACK		(){ 2 }

# defined in Gimp.xs for some reason
#sub EXPORT_CAN_HANDLE_RGB                   (){1}
#sub EXPORT_CAN_HANDLE_GRAY                  (){2}
#sub EXPORT_CAN_HANDLE_INDEXED               (){4}
#sub EXPORT_CAN_HANDLE_BITMAP                (){8}
#sub EXPORT_CAN_HANDLE_ALPHA	            (){16}
#sub EXPORT_CAN_HANDLE_LAYERS	            (){32}
#sub EXPORT_CAN_HANDLE_LAYERS_AS_ANIMATION   (){64}
#sub EXPORT_CAN_HANDLE_LAYER_MASKS           (){128}
#sub EXPORT_NEEDS_ALPHA                      (){256}
	
# file_tiff_*
sub COMPRESSION_NONE		(){ 0 }
sub COMPRESSION_LZW		(){ 1 }
sub COMPRESSION_PACKBITS	(){ 2 }

# True/False for PDB
sub TRUE			(){ 1 }
sub FALSE			(){ 0 }

use Gimp::ColorDB;
use Carp qw(croak);

my @_procs = ('main', '__', 'N_');
#my @_default = (@_procs, ':consts' ,':_auto2');
my @_default = (@_procs, ':consts');
my @POLLUTE_CLASSES;

# we really abuse the import facility..
sub import($;@) {
   my $pkg = shift;
   my $up = caller;
   my @export;

   @_=@_default unless @_;
   
   for(map { $_ eq ":DEFAULT" ? @_default : $_ } @_) {
      if ($_ eq ":auto") {
         push(@export,@_consts,@_procs);
         *{"$up\::AUTOLOAD"} = sub {
            croak "Cannot call '$AUTOLOAD' at this time" unless initialized();
            my ($class,$name) = $AUTOLOAD =~ /^(.*)::(.*?)$/;
            *{$AUTOLOAD} = sub { unshift @_, 'Gimp'; $AUTOLOAD = "Gimp::$name"; goto &AUTOLOAD };
            #*{$AUTOLOAD} = sub { Gimp->$name(@_) }; # old version
            goto &$AUTOLOAD;
         };
      } elsif ($_ eq ":_auto2") {
         push(@export,@_consts,@_procs);
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
         push(@export,@_consts);
      } elsif ($_ eq ":param") {
         push(@export,@_param);
      } elsif (/^interface=(\S+)$/) {
         croak __"interface=... tag is no longer supported\n";
      } elsif ($_=~/spawn_options=(\S+)/) {
         $spawn_opts = $1;
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

($basename = $0) =~ s/^.*[\\\/]//;

$spawn_opts = "";

# extra check for Gimp::Feature::import
$in_query=0 unless defined $in_query;
$in_top=$in_quit=$in_run=$in_net=$in_init=0;
($function)=$0=~/([^\/\\]+)$/;

$verbose=0 unless defined $verbose;
# $verbose=1;

$interface_type = "net";
if (@ARGV) {
   if ($ARGV[0] eq "-gimp") {
      $interface_type = "lib";
      # ignore other parameters completely
   } else {
      while(@ARGV) {
         $_=shift(@ARGV);
         if (/^-h$|^--?help$|^-\?$/) {
            $help=1;
            print __"Usage: $0 [gimp-args..] [interface-args..] [script-args..]
           gimp-arguments are
           -gimp <anything>           used internally only
           -h | -help | --help | -?   print some help
           -v | --verbose             be more verbose in what you do
           --host|--tcp HOST[:PORT]   connect to HOST (optionally using PORT)
                                      (for more info, see Gimp::Net(3))
";
         } elsif (/^-v$|^--verbose$/) {
            $verbose++;
         } elsif (/^--host$|^--tcp$/) {
            $host=shift(@ARGV);
         } else {
            unshift(@ARGV,$_);
            last;
         }
      }
   }
}

my @log;

sub format_msg {
   $_=shift;
   "$_->[0]: $_->[2] ".($_->[1] ? "($_->[1])":"");
}

sub _initialized_callback {
   # load the compatibility module on older versions
   if ($interface_pkg eq "Gimp::Lib") {
      # this must match @max_gimp_version in Gimp::Compat
      my @compat_gimp_version = (1,3);
      if ((Gimp->major_version < $compat_gimp_version[0])
          || (Gimp->major_version == $compat_gimp_version[0]
              && Gimp->minor_version < $compat_gimp_version[1])) {
         require Gimp::Compat;
         $compat_gimp_version[0] == $Gimp::Compat::max_gimp_version[0]
            && $compat_gimp_version[1] == $Gimp::Compat::max_gimp_version[1]
               or die "FATAL: Gimp::Compat version mismatch\n";
      }
   }
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
   print STDERR format_msg($log[-1]),"\n" if !($in_query || $in_init || $in_quit) || $verbose;
   _initialized_callback if initialized();
}

sub die_msg {
   logger(message => substr($_[0],0,-1), fatal => 1, function => 'ERROR');
}

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

##############################################################################

if ($interface_type=~/^lib$/i) {
   $interface_pkg="Gimp::Lib";
} elsif ($interface_type=~/^net$/i) {
   $interface_pkg="Gimp::Net";
} else {
   croak __"interface '$interface_type' unsupported.";
}

eval "require $interface_pkg" or croak "$@";
$interface_pkg->import;

# create some common aliases
for(qw(gimp_procedural_db_proc_exists gimp_call_procedure set_trace initialized)) {
   *$_ = \&{"$interface_pkg\::$_"};
}

*init  = \&{"$interface_pkg\::gimp_init"};
*end   = \&{"$interface_pkg\::gimp_end" };
*lock  = \&{"$interface_pkg\::lock" };
*unlock= \&{"$interface_pkg\::unlock" };

my %ignore_function = (DESTROY => 1);

@PREFIXES=("gimp_", "");

@gimp_gui_functions = qw(
   gimp_progress_init
   gimp_progress_update
   gimp_displays_flush
   gimp_display_new
   gimp_display_delete
);

sub ignore_functions(@) {
   @ignore_function{@_}++;
}

sub AUTOLOAD {
  my ($class,$name) = $AUTOLOAD =~ /^(.*)::(.*?)$/;
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
	shift unless ref $_[0];
	unshift @_, $sub;
	#goto &gimp_call_procedure; # does not work, PERLBUG! #FIXME
	my @r = eval { gimp_call_procedure (@_) };
	die $@ if $@; wantarray ? @r : $r[0];
      };
      goto &$AUTOLOAD;
    }
  }
  croak __"function/macro \"$name\" not found in $class";
}

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
_pseudoclass qw(Selection 	gimp_selection_);
_pseudoclass qw(Vectors 	gimp_vectors_);
_pseudoclass qw(Channel		gimp_channel_ gimp_selection_), @drawable_prefixes;
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
_pseudoclass qw(GimpDrawable	gimp_drawable_);
_pseudoclass qw(PixelRgn	gimp_pixel_rgn_);
_pseudoclass qw(Tile		gimp_tile_);

# Classes without GIMP-Object
_pseudoclass qw(Palette		gimp_palette_);
_pseudoclass qw(Context         gimp_context_);
_pseudoclass qw(Brushes		gimp_brush_ gimp_brushes_);
_pseudoclass qw(Brush		gimp_brush_);
_pseudoclass qw(Edit		gimp_edit_);
_pseudoclass qw(Gradients	gimp_gradients_);
_pseudoclass qw(Patterns	gimp_patterns_);
_pseudoclass qw(Pattern	        gimp_pattern_);

package Gimp::Tile;

unshift (@Tile::ISA, "Gimp::Tile");

package Gimp::PixelRgn;

push(@PixelRgn::ISA, "Gimp::PixelRgn");

sub new($$$$$$$$) {
   shift;
   init Gimp::PixelRgn(@_);
}

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

package Gimp::run_mode;

# I guess I now use almost every perl feature available ;)

use overload fallback => 1,
             '0+'     => sub { ${$_[0]} };

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

The default (unless '' is specified) is C<'main', ':consts', '__'>.
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
Use either a plain pdb (scheme-like) interface or an object-oriented
syntax, i.e. C<gimp_image_new(600,300,RGB)> is the same as C<new
Image(600,300,RGB)>

=item *
Networked plug-ins look/behave the same as those running from within gimp.  

=item *
Gimp::Fu will start GIMP for you, if it cannot connect to an existing
GIMP process.

=item *
You can access the pixel-data functions using piddles (see L<PDL>) giving
the same level of control as a C plug-in, with a data language wrapper.

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
wishes. Behind the scenes, GIMP has running a perl plugin (as described
above) that is constantly running, and waits for connections from
perl scripts. Your script, when it uses GIMP procedures (and Gimp-Perl
functions), will actually be communicating with the perl server running
under GIMP.

The architecture may be visualised like this:

 perlscript <-> Gimp::Net <-> Perl-Server <-> Gimp::Lib <-> GIMP

This has certain consequences; native GIMP objects like images and layers
obviously persist between Perl method calls, but C<libgimp> entities such
as C<GimpDrawable>, with the perl interface C<Gimp::PixelRgn>, require
special handling. Currently they do not work when used over C<Gimp::Net>.

=head1 OUTLINE OF A GIMP PLUG-IN

All plug-ins (with the exception of those using C<Gimp::init>) I<must>
contain a call to C<Gimp::main>.

The return code should be immediately handed out to exit:

 exit main;		# Gimp::main is exported by default.

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
gimp_install_procedure.

=item Gimp::on_net

Run when called from a network interface (from the Perl-Server or from
running it standalone).

=item Gimp::on_lib

Run only when called interactively from within Gimp.

=item Gimp::on_run

Run when anything calls it (network or lib).

=back

=head1 CALLING GIMP FUNCTIONS

There are two different flavours of gimp-functions. Functions from the
B<PDB> (the Procedural DataBase), and functions from B<libgimp> (the
C-language interface library).

You can get a listing and description of every PDB function by starting
the B<DB Browser> extension in GIMP's B<Xtns> menu (but remember to change 
 "-" (dashes) to  "_" (underscores)).

B<libgimp> functions can't be traced (and won't be traceable in the
foreseeable future).

To call pdb functions (or equivalent libgimp functions), just treat them like
normal perl (this requires the use of the C<:auto> import tag - see
the import C<:auto> note for non-OO limitation; see below for another
possibility!):

 gimp_palette_set_foreground([20,5,7]);
 gimp_palette_set_background("cornsilk");

If you don't use the C<:auto> import tag, you can call all Gimp functions
using OO-Syntax:

 Gimp->gimp_palette_set_foreground([20,5,7]);
 Gimp->palette_set_background("cornsilk");
 Palette->set_foreground('#1230f0');

As you can see, you can also drop part of the name prefixes with this
syntax, so its actually shorter to write and hopefully clearer to read.

=head1 SPECIAL FUNCTIONS

In this section, you can find descriptions of special functions, functions
having different calling conventions/semantics than might be expected
or otherwise interesting functions. All of these functions must either
be imported explicitly or called using a namespace override (C<Gimp::>),
not as Methods (C<Gimp-E<gt>>).

=over 4

=item main(), Gimp::main()

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

=item Gimp::init([connection-argument]), Gimp::end()

These is an alternative interface that replaces the call to Gimp::main
and the net callback. At the moment it only works for the Net interface
(L<Gimp::Net>), and not as a native plug-in. Here's an example:

 use Gimp;
 
 Gimp::init;
 <do something with GIMP>

The optional argument to init has the same format as the GIMP_HOST variable
described in L<Gimp::Net>. Calling C<Gimp::end> is optional.  This is used
in the process of testing the module ('make test').

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
with C<perldoc -m Gimp>, at the end of the file (the default database is
similar, but not identical to the X11 default rgb.txt)

=item Gimp::initialized()

this function returns true whenever it is safe to call gimp functions. This is
usually only the case after gimp_main or gimp_init have been called.

=item Gimp::register_callback(gimp_function_name, perl_function)

Using this function you can overwrite the standard Gimp behaviour of
calling a perl subroutine of the same name as the GIMP function.

The first argument is the name of a registered gimp function that you want
to overwrite ('perl_fu_make_something'), and the second argument can be
either a name of the corresponding perl sub ('Elsewhere::make_something')
or a code reference (\&my_make).

=item Gimp::canonicalize_colour/Gimp::canonicalize_color

Take in a color specifier in a variety of different formats, and return 
a valid gimp color specifier, consisting of 3 or 4 numbers in the range 
between 0 and 1.0.

For example: 

 $color = canonicalize_colour ("#ff00bb");
 $color = canonicalize_colour ([255,255,34]);
 $color = canonicalize_colour ([255,255,34,255]);
 $color = canonicalize_colour ([1.0,1.0,0.32]);
 $color = canonicalize_colour ('red');

Note that bounds checking is excessively lax; this assumes relatively good input

=back

=head1 SPECIAL METHODS

This chapter describes methods that behave differently than you might
expect, or methods uniquely implemented in perl (that is, not in the
PDB). All of these must be invoked using the method syntax (C<Gimp-E<gt>>
or C<$object-E<gt>>).

=over 4

=item gimp_install_procedure(name, blurb, help, author, copyright, date, menu_path, image_types, type, [params], [return_vals])

Mostly same as gimp_install_procedure from the C library. The
parameters and return values for the functions are specified as an
array ref containing either integers or array-refs with three elements,
[PARAM_TYPE, \"NAME\", \"DESCRIPTION\"].
 
=item gimp_progress_init(message,[])

Initializes a progress bar. In networked modules this is a no-op.

=item gimp_progress_update(percentage)

Updates the progress bar. No-op in networked modules.

=item gimp_tile_*, gimp_pixel_rgn_*, gimp_drawable_get

With these functions you can access the raw pixel data of drawables. They
are documented in L<Gimp::Pixel>, to keep this manual page short.

=item gimp_call_procedure(procname, arguments...)

This function is actually used to implement the fancy stuff. Its your basic
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
PixelRgn->new and similar functions.

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
       $image = new Image (width,height,type)
       $image = image_new Display (width,height,type)

=head1 DEBUGGING AIDS

How to debug your scripts:

=over 4

=item $Gimp::verbose

If set to true, will make Gimp say what it's doing on STDOUT. It will
also stop L<Gimp::Net>'s normal behaviour of the server-side closing
STDIN, STDOUT and STDERR. If you want it to be set during loading Gimp.pm,
make sure to do so in a C<BEGIN> block.

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

=item DISPLAY, IMAGE, LAYER, CHANNEL, DRAWABLE, SELECTION, VECTORS

these will be mapped to corresponding objects (IMAGE => Gimp::Image). In
trace output you will see small integers (the image/layer/etc..-ID)

=item PARASITE

represented as an array ref [name, flags, data], where name and data
should be perl strings and flags is the numerical flag value.

=item REGION, BOUNDARY, PATH, STATUS

Not yet supported.

=back

=head1 AUTHOR

Marc Lehmann <pcg@goof.com> (pre-2.0)

Seth Burgess <sjburge@gimp.org> (2.0+)

Ed J (with oversight and guidance from Kevin Cozens) (2.3+)

=head1 SEE ALSO

perl(1), gimp(1), L<Gimp::OO>, L<Gimp::Data>, L<Gimp::Pixel>,
L<Gimp::Util>, L<Gimp::UI>, L<Gimp::Feature>, L<Gimp::Net>,
L<Gimp::Compat>, L<Gimp::Config>, L<Gimp::Lib>, and  L<Gimp::Module> .

=cut

1;
