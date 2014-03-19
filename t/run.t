use strict;
use Test::More tests => 15;
#BEGIN { $Gimp::verbose = 1; }
#Gimp::set_trace(TRACE_ALL);
use Gimp qw(:auto);

our $dir;
our $DEBUG = 0;
require 't/gimpsetup.pl';

Gimp::init("spawn/");

ok((my $i = new Image(10,10,RGB)), 'OO Syntax for new image');
ok(
  (my $l = $i->layer_new(10,10,RGBA_IMAGE,"new layer",100,VALUE_MODE)),
  'Different OO syntax for creating a layer',
);
ok(!Gimp->image_insert_layer($l,0,0), 'Yet another OO syntax');
is("new layer", $l->get_name, 'layer name');
ok(
  !$l->paintbrush(50,[1,1,2,2,5,3,7,4,2,8],PAINT_CONSTANT,0), 
  'some painting with variable length arrays, default value',
);
ok(
  !$l->paintbrush(30,4,[5,5,8,1],PAINT_CONSTANT,0),
  'paint without default value',
);
ok(
  !Plugin->sharpen(RUN_NONINTERACTIVE,$i,$l,10),
  'call external plugin through Plugin->, use explicit RUN_NONINTERACTIVE',
);
ok(!$l->sharpen(10), 'call with maximum fu magic');
ok(!Gimp->plug_in_sharpen($i,$l,10), 'call plugin using default');

# exercise COLORARRAY - read only as can't find proc that takes as input
my @palettes = Gimp->palettes_get_list("Default");
my @colors = Gimp::Palette->get_colors($palettes[0]);
#require Data::Dumper;warn Data::Dumper::Dumper(scalar @colors), "\n";
cmp_ok(scalar(@colors), '==', 23, 'colorarray correct size');
cmp_ok(scalar(@{ $colors[0] }), '==', 4, 'colorarray 1st el is correct size');

# exercise VECTORS
my $tl = $i->text_layer_new("hi", "Arial", 8, 3);
$i->insert_layer($tl, 0, 0);
my $vectors = $tl->vectors_new_from_text_layer;
cmp_ok(ref($vectors), 'eq', 'Gimp::Vectors', 'vectors object returned');
my $vectorstring = $vectors->export_to_string; # takes VECTORS as input - QED
like($vectorstring, qr/<path id="hi"/, 'vector string plausible');

ok(!$i->delete, 'remove image');
