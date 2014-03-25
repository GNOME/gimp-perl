use strict;
use Test::More;

#BEGIN { $Gimp::verbose = 1; }
BEGIN { use_ok('Gimp', qw(:auto canonicalise_color)); }
require 't/gimpsetup.pl';
#Gimp::set_trace(TRACE_ALL);
Gimp::init("spawn/");

my %CONST_DATA = (
  PDB_FLOAT => 3,
  PDB_PATH => 19,
  ABSOLUTE_CONVOL => 1,
  PDB_PASS_THROUGH => 2,
  GRADIENT_SPIRAL_ANTICLOCKWISE => 10,
  HISTOGRAM_BLUE => 3,
  STACK_TRACE_QUERY => 1,
  DISSOLVE_MODE => 1,
  UNIT_POINT => 3,
  MAGENTA_HUES => 6,
  RUN_INTERACTIVE => 0,
  CLIP_TO_IMAGE => 1,
  COMPRESSION_LZW => 1,
  TRACE_ALL => 255,
  ROTATE_90 => 0,
  TRUE => 1,
  BLACK => 2,
  EXPORT_CAN_HANDLE_GRAY => 2,
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
