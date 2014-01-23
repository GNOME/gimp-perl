use Config;
use strict;
use File::Temp;
use Test::More tests => 19;
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
ok (io("$dir/gimprc")->print(<<EOF), 'output gimprc');
(show-tips no)
(script-fu-path "")
(plug-in-path "$dir")
EOF
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
ok(!$i->delete, 'remove image');
