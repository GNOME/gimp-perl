use strict;
use warnings;
use ExtUtils::MakeMaker;

my $pkg = ExtUtils::Depends->new(qw(Gimp Alien::Gimp Gtk2 PDL));
$pkg->add_typemaps("../typemap");
$pkg->set_inc('-I.. -Ddatadir=""');

WriteMakefile(
  'NAME' => 'Gimp::Net',
  'VERSION_FROM' => 'Net.pm',
  NO_MYMETA => 1,
  NO_META => 1,
  $pkg->get_makefile_vars,
);
