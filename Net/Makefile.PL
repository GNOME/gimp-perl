use ExtUtils::MakeMaker;

require '../config.pl';

WriteMakefile(
  'NAME' => 'Gimp::Net',
  'VERSION_FROM' => '../Gimp.pm',
  'INC' => $cfg{GIMP_CFLAGS},
  'TYPEMAPS' => ["$topdir/typemap"],
  NO_MYMETA => 1,
  NO_META => 1,
);
