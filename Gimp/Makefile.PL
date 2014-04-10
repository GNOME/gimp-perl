use ExtUtils::MakeMaker;
use ExtUtils::Depends;
require PDL::Core::Dev;

require '../config.pl';

my $pkg = new ExtUtils::Depends Gimp;
$pkg->set_inc($cfg{GIMP_CFLAGS}.' '.&PDL::Core::Dev::PDL_INCLUDE);
$pkg->set_libs("$cfg{GIMP_LIBS} $cfg{INTLLIBS}");
$pkg->add_typemaps("$topdir/typemap", &PDL::Core::Dev::PDL_TYPEMAP);
$pkg->add_pm(map { ($_ => "\$(INST_LIB)/Gimp/$_") } glob '*.pm');

WriteMakefile(
    'NAME'	=> 'Gimp::Lib',
    'VERSION_FROM' => '../Gimp.pm',
    $pkg->get_makefile_vars,
    NO_MYMETA	=> 1,
    NO_META	=> 1,
);
