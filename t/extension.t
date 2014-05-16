use strict;
use Test::More;
our ($dir, $DEBUG);
my $tpf_name;
BEGIN {
#  $Gimp::verbose = 1;
  $DEBUG = 0;
  require 't/gimpsetup.pl';
  use Config;
  $tpf_name = "test_perl_extension";
  write_plugin($DEBUG, $tpf_name, $Config{startperl}.
    "\nBEGIN { \$Gimp::verbose = ".int($Gimp::verbose||0).'; }'.<<'EOF');

use strict;
use Gimp;
use Gimp::Extension;

podregister {
  $num + 1;
};

exit main;
__END__

=head1 NAME

extension_test - test Gimp::Extension

=head1 SYNOPSIS

<Image>/Filters/Languages/Perl/Test

=head1 DESCRIPTION

Description.

=head1 PARAMETERS

 [&Gimp::PDB_INT32, "run_mode", "Interactive:0=yes,1=no"],
 [&Gimp::PDB_INT32, "num", "internal flags (must be 0)"],

=head1 RETURN VALUES

 [&Gimp::PDB_INT32, "retnum", "Number returned"],

=head1 AUTHOR

Author.

=head1 DATE

1999-12-02

=head1 LICENSE

Same terms as Gimp-Perl.
EOF
}
use Gimp "net_init=spawn/";

is(Gimp::Plugin->extension_test(Gimp::RUN_NONINTERACTIVE, 7), 8, 'return val');

Gimp::Net::server_quit;
Gimp::Net::server_wait;

done_testing;
