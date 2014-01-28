use Config;
use strict;
use File::Temp;
use Test::More tests => 22;
use IO::All;

BEGIN { use_ok('Gimp', qw(:auto)); }
our %cfg;
require_ok './config.pl';

my $plugins = $cfg{gimpplugindir} . '/plug-ins';
ok(-d $plugins, 'plugins dir exists');
ok(-x "$plugins/script-fu", 'script-fu executable');

my $dir = File::Temp->newdir;
my $perlserver = "$dir/Perl-Server.pl";
my $s = io("Perl-Server")->all or die "unable to read the Perl-Server";
$s =~ s/^(#!).*?(\n)/$Config{startperl}$2/;
ok(io($perlserver)->print($s), 'wrote Perl-Server');
ok(chmod(0700, $perlserver), 'chmod Perl-Server');
ok(symlink("$plugins/script-fu", "$dir/script-fu"), 'symlink script-fu');
ok(symlink("$plugins/sharpen", "$dir/sharpen"), 'symlink sharpen');
ok(io("$dir/gimprc")->print("(plug-in-path \"$dir\")\n"), 'output gimprc');

$ENV{GIMP2_DIRECTORY} = $dir;

Gimp::init("spawn/");

ok((my $i = new Image(10,10,RGB)), 'OO Syntax for new image');
ok(
  (my $l = $i->layer_new(10,10,RGBA_IMAGE,"new layer",100,VALUE_MODE)),
  'Different OO syntax for creating a layer',
);
ok(!Gimp->image_add_layer($l,0), 'Yet another OO syntax');
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

# exercise COLORARRAY
my @palettes = Gimp->palettes_get_list("Default");
my @colors = Gimp::Palette->get_colors($palettes[0]);
#require Data::Dumper;warn Data::Dumper::Dumper(scalar @colors), "\n";
cmp_ok(scalar(@colors), '==', 23, 'colorarray correct size');
cmp_ok(scalar(@{ $colors[0] }), '==', 4, 'colorarray 1st el is correct size');

# exercise VECTORS
my $tl = $i->text_layer_new("hi", "Arial", 8, 3);
$i->insert_layer($tl, 0, 0);
my $vector = $tl->vectors_new_from_text_layer;
my $vectorstring = $tl->vectors_export_to_string;
ok($vectorstring =~ /^<\?xml/, 'vector string plausible');

ok(!$i->delete, 'remove image');
