use strict;
use Test::More tests => 7;
#BEGIN { $Gimp::verbose = 1; }
use Gimp qw(:auto);
use Config;

our $dir;
our $DEBUG = 0;
require 't/gimpsetup.pl';

my $plugin = "$dir/test_perl_filter";
die "write $plugin: $!" unless io($plugin)->print($Config{startperl}.<<'EOF');

use Gimp qw(:auto __ N_);
use Gimp::Fu;

sub test_perl_filter {
  my ($i, $drawable, $text) = @_;
  my $tl = $i->text_layer_new("hi", "Arial", 8, 3);
  $i->insert_layer($tl, 0, 0);
  $tl->set_name('text layer');
  return $image;
}

register	"test_perl_filter",
		"exercise gimp-perl for a filter",
		"exercise gimp-perl for a filter",
		"boilerplate id",
		"boilerplate id",
		"20140310",
		N_"<Image>/Filters",
		"*",
	[
	  [PF_STRING, "text", "Text to put in layer", "hello"],
	],
	\&test_perl_filter;

exit main;
EOF
die "chmod $plugin: $!" unless chmod 0700, $plugin;

Gimp::init("spawn/");

ok((my $i = Gimp::Image->new(10,10,RGB)), 'new image');
ok(
  (my $l0 = $i->layer_new(10,10,RGBA_IMAGE,"new layer",100,VALUE_MODE)),
  'make layer',
);
ok(!$i->insert_layer($l0,0,0), 'insert layer');
ok(!$i->test_perl_filter(undef, 'text value'), 'call filter'); # 1st param drawable
my ($tl) = $i->get_layers;
is('text layer', $tl->get_name, 'layer name');

ok(!$i->delete, 'remove image');
