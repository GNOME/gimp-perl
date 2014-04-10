use ExtUtils::MakeMaker;
use ExtUtils::Depends;

require '../config.pl';

my $pkg = new ExtUtils::Depends Gimp, Gtk2;
$pkg->set_inc($cfg{GIMP_CFLAGS});
$pkg->set_libs($cfg{GIMP_LIBS});
$pkg->add_pm('UI.pm' => '$(INST_LIBDIR)/UI.pm');
$pkg->add_typemaps("$topdir/typemap");

WriteMakefile(
  'NAME'	=> 'Gimp::UI',
  'VERSION_FROM' => '../Gimp.pm',
  $pkg->get_makefile_vars,
  NO_MYMETA	=> 1,
  NO_META	=> 1,
);
