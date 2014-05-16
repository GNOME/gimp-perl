use strict;
use Test::More;
our ($dir, $DEBUG);
my $tpf_name;
BEGIN {
#  $Gimp::verbose = 1;
  $DEBUG = 0;
  require 't/gimpsetup.pl';
  use Config;
  $tpf_name = "test_perl_filter";
  write_plugin($DEBUG, $tpf_name, $Config{startperl}.
    "\nBEGIN { \$Gimp::verbose = ".int($Gimp::verbose||0).'; }'.<<'EOF');

use strict;
use Gimp;
use Gimp::Fu;

sub boilerplate_params {
  my ($testing, $menuloc, $imagetypes) = @_;
  (
    ("exercise gimp-perl filter testing $testing") x 2,
    ("boilerplate id") x 2,
    "20140310",
    N_$menuloc,
    $imagetypes // "*",
  );
}

# & to dodge annoying prototype preventing use of boilerplate_params!
&register(
  "test_dies",
  boilerplate_params('exceptions', '<None>'),
  [ [ PF_STRING, "text", "Input text", 'default' ], ],
  sub { die $_[0] }
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
  "test_return_str_not_int",
  boilerplate_params('param exception on return val', '<None>'),
  [],
  [
    [ PF_INT16, "int", "Output int", ],
  ],
  sub { 'verbiage' }
);

&register(
  "test_float_in",
  boilerplate_params('in param float', '<None>'),
  [
    [ PF_FLOAT, "float", "Input float", ],
  ],
  [],
  sub { }
);

&register(
  "test_return_image",
  boilerplate_params('return image', '<None>'),
  [],
  [
    [ PF_IMAGE, "image", "Output image", ],
  ],
  sub { 1 }
);

&register(
  "test_return_toomany",
  boilerplate_params('return toomany', '<None>'),
  [],
  [
    [ PF_IMAGE, "image", "Output image", ],
  ],
  sub { (1, 2) }
);

&register(
  "test_return_toofew",
  boilerplate_params('return toofew', '<None>'),
  [],
  [
    [ PF_IMAGE, "image", "Output image", ],
  ],
  sub { }
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
  "test_create_return_image",
  boilerplate_params('retval addition', '<Image>/File/Create/x1', ''),
  [],
  [
  ],
  sub { Gimp::Image->new(20, 20, RGB) }
);

&register(
  "test_create_return_int_image",
  boilerplate_params('retval addition', '<Image>/File/Create/x1', ''),
  [],
  [
    [ PF_INT32, "int", "Output int", ],
  ],
  sub { (Gimp::Image->new(20, 20, RGB), 2) }
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
    return;
  }
);

exit main;
EOF
}
use Gimp qw(:DEFAULT net_init=spawn/);

ok((my $i = Gimp::Image->new(10,10,RGB)), 'new image');
ok(
  (my $l0 = $i->layer_new(10,10,RGBA_IMAGE,"new layer",100,VALUE_MODE)),
  'make layer',
);
ok(!$i->insert_layer($l0,0,0), 'insert layer');
ok(!$i->test_perl_filter(undef, 'value'), 'call filter'); # 1st param drawable
my ($tl) = $i->get_layers;
is($tl->get_name, 'value', 'layer name');
is(Gimp::Plugin->test_return_text('text'), 'text', 'return text');
my $incolour = [6, 6, 6, 1];
is_deeply(
  Gimp::Plugin->test_return_colour($incolour),
  Gimp::canonicalize_color($incolour),
  'return colour'
);
my $send_text = 'exception';
eval { Gimp::Plugin->test_dies($send_text."\n"); };
is($@, "$send_text\n", 'exception with newline correct');
eval { Gimp::Plugin->test_dies($send_text); };
like($@, qr/$send_text.*$tpf_name/, 'exception net w/o newline correct');
eval { Gimp::Image->new; };
my $dot_t_file = __FILE__;
$dot_t_file =~ s#.*/##;
like($@, qr/$dot_t_file/, 'exception from GIMP proc');
eval { Gimp::Plugin->test_return_str_not_int; };
like($@, qr/Expected a number/, 'exception handling on bad return value');
eval { Gimp::Plugin->test_float_in('notfloat'); };
like($@, qr/Expected a number/, 'exception handling on bad input value');
ok(!Gimp::Plugin->test_float_in(0.75), 'float input');
eval { Gimp::Plugin->test_pf_adjustment('text'); };
like($@, qr/Expected a number/, 'pf_adjustment dies on non-INT32');
is(Gimp::Plugin->test_pf_adjustment(17), 17, 'adj return');
is(Gimp::Plugin->test_no_params, 1, 'no params');
is_deeply(
  [ Gimp::Plugin->test_return_int32array ],
  [ [1, 2], [3, 4] ],
  'return array'
);
is(ref(Gimp::Plugin->test_return_image), 'Gimp::Image', 'image ref');
eval { Gimp::Plugin->test_return_toomany; };
like($@, qr/too many/, 'too many return values is error');
eval { Gimp::Plugin->test_return_toofew; };
like($@, qr/too few/, 'too few return values is error');
isa_ok(Gimp::Plugin->test_create_return_image, 'Gimp::Image', 'add image ret');
is_deeply(
  [ map { ref($_) || $_ } Gimp::Plugin->test_create_return_int_image ],
  [ 'Gimp::Image', 2 ],
  'add image ret when other ret there'
);
# if enable next line, brings up script dialog
# color one works, font doesn't - speculate is due to being in "batch mode"
#Gimp::Plugin->test_dialogs(RUN_INTERACTIVE, [0,0,0], "Arial", 150, );

Gimp::Net::server_quit;
Gimp::Net::server_wait;

done_testing;
