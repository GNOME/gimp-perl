use strict;
use Test::More;
#BEGIN { $Gimp::verbose = 1; }
#Gimp::set_trace(TRACE_ALL);
use Gimp qw(:auto);
use Config;

our $dir;
our $myplugins;
our $DEBUG = 0;
require 't/gimpsetup.pl';

my $plugin = "$myplugins/test_perl_filter";
write_plugin($DEBUG, $plugin, $Config{startperl}.
  "\nBEGIN { \$Gimp::verbose = ".int($Gimp::verbose).'; }'.<<'EOF');

use strict;
use Gimp qw(:auto __ N_);
use Gimp::Fu;

sub boilerplate_params {
  my ($testing, $menuloc) = @_;
  (
    ("exercise gimp-perl filter testing $testing") x 2,
    ("boilerplate id") x 2,
    "20140310",
    N_$menuloc,
    "*",
  );
}

# & to dodge annoying prototype preventing use of boilerplate_params!
&register(
  "test_dies",
  boilerplate_params('exceptions', '<None>'),
  [ [ PF_STRING, "text", "Input text", 'default' ], ],
  sub { die $_[0]."\n" }
);

&register(
  "test_pf_adjustment",
  boilerplate_params('returning text', '<None>'),
  [ [ PF_ADJUSTMENT, "input", "input", [100, 2, 1000, 1, 10, 0, 1]  ], ],
  [ [ PF_INT32, "num", "Output number", ], ],
  sub { @_ }
);

&register(
  "test_return_text",
  boilerplate_params('returning text', '<None>'),
  [ [ PF_STRING, "text", "Input text", 'default' ], ],
  [ [ PF_STRING, "text", "Output text", ], ],
  sub { @_ }
);

&register(
  "test_return_colour",
  boilerplate_params('returning color', '<None>'),
  [ [ PF_COLOR, "colour", "Input colour", [ 5, 5, 5 ], ], ],
  [ [ PF_COLOR, "colour", "Output colour", ], ],
  sub { @_ }
);

&register(
  "test_return_int32array",
  boilerplate_params('returning array', '<None>'),
  [],
  [
    [ PDB_INT32ARRAY, "array1", "Output array1", ],
    [ PDB_INT32ARRAY, "array2", "Output array1", ],
  ],
  sub { ([1, 2], [3, 4]) }
);

&register(
  "test_no_params",
  boilerplate_params('no params', '<None>'),
  [],
  [
    [ PF_INT32, "int", "Output int", ],
  ],
  sub { 1 }
);

&register(
  "test_perl_filter",
  boilerplate_params('filter', '<Image>/Filters'),
  [ [PF_STRING, "text", "Text to name layer", "hello"], ],
  sub {
    # returns a value despite such not being declared
    # previously, excess returns were a fatal error, but none were ever returned
    # now not an error
    my ($i, $drawable, $text) = @_;
    my $tl = $i->text_layer_new("hi", "Arial", 8, 3);
    $i->insert_layer($tl, 0, 0);
    $tl->set_name($text);
    return $i;
  }
);

exit main;
EOF

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
is(Gimp::Plugin->test_return_text('text'), 'text', 'return text');
my $incolour = [6, 6, 6, 1];
is_deeply(
  Gimp::Plugin->test_return_colour($incolour),
  Gimp::canonicalize_color($incolour),
  'return colour'
);
my $send_text = 'exception';
eval { Gimp::Plugin->test_dies($send_text); };
is($@, "$send_text\n", 'exception returned correctly');
eval { is(Gimp::Plugin->test_pf_adjustment('text'), 'text', 'adj'); };
like($@, qr/INT32/, 'pf_adjustment dies on non-INT32');
is(Gimp::Plugin->test_pf_adjustment(17), 17, 'adj return');
is(Gimp::Plugin->test_pf_adjustment(undef), 100, 'adj default');
is(Gimp::Plugin->test_no_params, 1, 'no params');
is_deeply(
  [ Gimp::Plugin->test_return_int32array ],
  [ [1, 2], [3, 4] ],
  'return array'
);

done_testing;
