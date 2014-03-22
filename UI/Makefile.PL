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

$GIMP_INC = "-I../../.. $GIMP_INC" if $IN_GIMP;

# change add_define to add them in here instead
$pkg->set_inc ("$DEFINE1 $DEFS $INC1 $GIMP_INC $CPPFLAGS $pdl_inc $CFLAGS");
$pkg->set_libs ("$GIMP_LIBS");
$pkg->add_pm (
              'UI.pm'		=> '$(INST_LIBDIR)/UI.pm',
             );
$pkg->add_typemaps ("$topdir/typemap", @pdl_typemaps);

WriteMakefile(
    'NAME'	=> 'Gimp::UI',
    'VERSION_FROM' => '../Gimp.pm',
    'macro'	=> { libdir => $libdir, exec_prefix => $exec_prefix, prefix => $prefix },
    $pkg->get_makefile_vars (),
);
