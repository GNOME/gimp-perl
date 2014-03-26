use strict;
use Test::More;

our ($dir, $DEBUG);
BEGIN {
#  $Gimp::verbose = 1;
  $DEBUG = 0;
  require 't/gimpsetup.pl';
}
BEGIN { use_ok('Gimp', qw(:auto canonicalise_color net_init=spawn/)); }
#Gimp::set_trace(TRACE_ALL);

my %CONST_DATA = (
  'ADD_COPY_MASK' => 5,
  'FG_TRANSPARENT_MODE' => 2,
  'BRUSH_SOFT' => 1,
  'BRUSH_GENERATED_SQUARE' => 1,
  'PATTERN_BUCKET_FILL' => 2,
  'CHANNEL_OP_REPLACE' => 2,
  'GRAY_CHANNEL' => 3,
  'PATTERN_CLONE' => 1,
  'FS_DITHER' => 1,
  'CUSTOM_PALETTE' => 4,
  'ABSOLUTE_CONVOL' => 1,
  'SHARPEN_CONVOLVE' => 1,
  'DESATURATE_LUMINOSITY' => 1,
  'DODGE' => 0,
  'TRANSPARENT_FILL' => 3,
  'FOREGROUND_EXTRACT_SIOX' => 0,
  'GRADIENT_SEGMENT_HSV_CCW' => 1,
  'GRADIENT_SEGMENT_SINE' => 2,
  'GRADIENT_SHAPEBURST_ANGULAR' => 6,
  'GRID_ON_OFF_DASH' => 2,
  'HISTOGRAM_BLUE' => 3,
  'MAGENTA_HUES' => 6,
  'ICON_TYPE_INLINE_PIXBUF' => 1,
  'RGB' => 0,
  'GRAY_IMAGE' => 2,
  'INK_BLOB_TYPE_CIRCLE' => 0,
  'INTERPOLATION_LINEAR' => 1,
  'GRAIN_EXTRACT_MODE' => 20,
  'MASK_DISCARD' => 1,
  'CLIP_TO_IMAGE' => 1,
  'ERROR_CONSOLE' => 2,
  'OFFSET_BACKGROUND' => 0,
  'ORIENTATION_VERTICAL' => 1,
  'PDB_ERROR_HANDLER_PLUGIN' => 1,
  'EXTENSION' => 2,
  'PDB_SUCCESS' => 3,
  'PAINT_CONSTANT' => 0,
  'PROGRESS_COMMAND_PULSE' => 4,
  'REPEAT_SAWTOOTH' => 1,
  'ROTATE_180' => 1,
  'RUN_WITH_LAST_VALS' => 2,
  'SELECT_CRITERION_S' => 5,
  'POINTS' => 1,
  'STACK_TRACE_NEVER' => 0,
  'TEXT_DIRECTION_RTL' => 1,
  'TEXT_HINT_STYLE_MEDIUM' => 2,
  'TEXT_JUSTIFY_FILL' => 3,
  'MIDTONES' => 1,
  'TRANSFORM_FORWARD' => 0,
  'TRANSFORM_RESIZE_CROP' => 2,
  'USER_DIRECTORY_PICTURES' => 4,
  'VECTORS_STROKE_TYPE_BEZIER' => 0,
);

{
no strict 'refs';
map { is(&{$_}, $CONST_DATA{$_}, "const $_ correct"); } sort keys %CONST_DATA;
}

is_deeply(
  canonicalise_color('DarkRed'),
  [ map {$_/255} 139, 0, 0, ],
  "canonicalise_color"
);

Gimp::Net::server_quit;
Gimp::Net::server_wait;

done_testing;
