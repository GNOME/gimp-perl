use strict;
use Test::More;
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

sub test_return_text {
  my ($text) = @_;
  return $text;
}

sub test_return_colour {
  my ($colour) = @_;
  return $colour;
}

# returns a value despite such not being declared
# previously, excess returns were a fatal error, but none were ever returned
# now not an error
sub test_perl_filter {
  my ($i, $drawable, $text) = @_;
  my $tl = $i->text_layer_new("hi", "Arial", 8, 3);
  $i->insert_layer($tl, 0, 0);
  $tl->set_name($text);
  return $image;
}

register	"test_return_text",
		"exercise gimp-perl filter returning text",
		"exercise gimp-perl filter returning text",
		"boilerplate id",
		"boilerplate id",
		"20140310",
		N_"<None>",
		"*",
	[
	  [PF_STRING, "text", "Input text", 'default' ],
	],
	[
	  [PF_STRING, "text", "Output text", ],
	],
	\&test_return_text;

register	"test_return_colour",
		"exercise gimp-perl filter returning color",
		"exercise gimp-perl filter returning color",
		"boilerplate id",
		"boilerplate id",
		"20140310",
		N_"<None>",
		"*",
	[
	  [PF_COLOR, "colour", "Input colour", [ 5, 5, 5 ], ],
	],
	[
	  [PF_COLOR, "colour", "Output colour", ],
	],
	\&test_return_colour;

register	"test_perl_filter",
		"exercise gimp-perl for a filter",
		"exercise gimp-perl for a filter",
		"boilerplate id",
		"boilerplate id",
		"20140310",
		N_"<Image>/Filters",
		"*",
	[
	  [PF_STRING, "text", "Text to name layer", "hello"],
	],
	\&test_perl_filter;

exit main;
EOF
die "chmod $plugin: $!" unless chmod 0700, $plugin;

#Gimp::set_trace(TRACE_ALL);
Gimp::init("spawn/");

ok((my $i = Gimp::Image->new(10,10,RGB)), 'new image');
ok(
  (my $l0 = $i->layer_new(10,10,RGBA_IMAGE,"new layer",100,VALUE_MODE)),
  'make layer',
);
ok(!$i->insert_layer($l0,0,0), 'insert layer');
ok(!$i->test_perl_filter(undef, 'value'), 'call filter'); # 1st param drawable
my ($tl) = $i->get_layers;
is('value', $tl->get_name, 'layer name');
is(Gimp::Plugin->test_return_text('text'), 'text', 'call return text');
is(Gimp::Plugin->test_return_text(undef), 'default', 'test default on plugin');
ok((my $c = Gimp::Plugin->test_return_colour([6, 6, 6])), 'return colour');

ok(!$i->delete, 'remove image');

done_testing;
