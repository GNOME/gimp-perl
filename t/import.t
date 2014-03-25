use strict;
use Test::More;
#BEGIN { $Gimp::verbose = 1; }
use Gimp qw(:auto);
#Gimp::set_trace(TRACE_ALL);

our $dir;
our $DEBUG = 0;
require 't/gimpsetup.pl';

Gimp::init("spawn/");

eval { Image->new(10,10,RGB); };
ok($@, 'polluting version should fail');

Gimp->import(':pollute');
ok(Image->new(10,10,RGB), 'polluting version should now work');

Gimp::Net::server_quit;
Gimp::Net::server_wait;

done_testing;
