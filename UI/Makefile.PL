use ExtUtils::MakeMaker;
use ExtUtils::Depends;

do '../config.pl';

my $pkg = new ExtUtils::Depends Gimp, Glib, Gtk2;

sub MY::const_config {
   my $self = shift;
   $self->{LDDLFLAGS}="$GIMP_LIBS $LDFLAGS $LIBS $self->{LDDLFLAGS}";
   package MY;
   $self->SUPER::const_config(@_);
}

sub MY::const_loadlibs {
   my $self = shift;
   if ($IN_GIMP) {
      $self->{LD_RUN_PATH} = join (":",
         $libdir,
         split /:/,$self->{LD_RUN_PATH}
      );
   }
   package MY;
   $self->SUPER::const_loadlibs(@_);
}

sub MY::postamble {
   <<"EOF";
clean ::
	test -f Makefile || mv -f Makefile.old Makefile
EOF
}

$GIMP_INC = "-I../../.. $GIMP_INC" if $IN_GIMP;

$pkg->set_inc ("$INC1 $GIMP_INC $CPPFLAGS $pdl_inc $CFLAGS");
$pkg->set_libs ("$GIMP_LIBS");
$pkg->add_pm (
              'UI.pm'		=> '$(INST_LIBDIR)/UI.pm',
              'basewidget.pm'	=> '$(INST_LIBDIR)/basewidget.pm',
             );
# obsoleted in 2.0.  I'm not sure if this needs to be added to set_inc 
# or not.
# $pkg->add_define ("$DEFINE1 $DEFS");
$pkg->add_typemaps ("$topdir/typemap", @pdl_typemaps);

WriteMakefile(
    'NAME'	=> 'Gimp::UI',
    'VERSION_FROM' => '../Gimp.pm',
    'macro'	=> { libdir => $libdir, exec_prefix => $exec_prefix, prefix => $prefix },
    $pkg->get_makefile_vars (),
);
