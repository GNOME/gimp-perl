# this is ugly, but it makes Gimp installable from within CPAN

use Cwd 'abs_path';

# make $topdir be where the gimp-perl sources start
$topdir = ".";
$topdir .= "/.." while ! -f "$topdir/MANIFEST";
$topdir = abs_path $topdir;

use ExtUtils::PkgConfig;
%gimpcfg = ExtUtils::PkgConfig->find("gimp-2.0");
%gtkcfg  = ExtUtils::PkgConfig->find("gtk+-2.0");
%glibcfg = ExtUtils::PkgConfig->find("glib-2.0");

$^W=0;

# pull in the libs/includes/etc from gimp and glib
$gimppath = ExtUtils::PkgConfig->variable("gimp-2.0", "exec_prefix") . "/bin/";
$gimplibdir = ExtUtils::PkgConfig->variable("gimp-2.0", "gimplibdir");

# need this for setting library variables below
$pluginlibs = `$gimppath/gimptool-2.0 --libs`;
chomp $pluginlibs;

# Get gimp's version and append to make binname
$gimpbinname = ExtUtils::PkgConfig->modversion("gimp-2.0");
$gimpbinname =~ s/^(\d\.\d).*/$1/; # strip off minor versions
$gimpversion = $gimpbinname; # capture the x.y version number
$gimpbinname = "gimp-" . $gimpbinname;

die "Need GIMP version at least 2.8.0\n" unless $gimpversion >= 2.8;

%cfg = (
   GIMP         => $gimppath . $gimpbinname,
   GIMPTOOL     => $gimppath . "gimptool-2.0",
   _GIMP_INC        => $gimpcfg{"cflags"},
   _GIMP_INC_NOUI   => $gimpcfg{"cflags"},
   _GIMP_LIBS       => $pluginlibs,
   _GIMP_LIBS_NOUI  => $gimpcfg{"libs"},

   GTK_CFLAGS       => $gtkcfg{"cflags"},

   GLIB_CFLAGS      => $glibcfg{"cflags"},
   GLIB_LIBS        => $glibcfg{"libs"},

   gimpplugindir    => $gimplibdir,

   _EXTENSIVE_TESTS => q[1],

   pdl_inc      => '',
   pdl_typemaps     => '',
   INC1         => '',
   DEFINE1      => '',

   LIBS         => q[],
   INTLLIBS     => q[],
);

sub expand {
   my $cfg = shift;
   my $count = 5;
   $cfg =~ s%\$\(top_builddir\)%$topdir/../../%g;
   while($cfg=~/\$\{/ and $count--) {
      while(($k,$v)=each %cfg) {
         $cfg=~s/\$\{$k\}/$v/g;
      }
   }
   $cfg;
}


# the next line should no longer be necessary, but...
$cfg{_CFLAGS} =~ s/\B-Wall\b//g; # remove -Wall from cflags and pray...

while (($k,$v)=each(%cfg)) {
   $k=~s/^_//;
   $$k=$v;
}

$GIMPTOOL       = expand($GIMPTOOL);
$INTLLIBS   = expand($INTLLIBS);

chomp($gimpplugindir  = `$GIMPTOOL --gimpplugindir`);
$GIMP           = expand($GIMP);

$GIMP_INC   =~ s%\$topdir%$topdir%g;
$GIMP_INC_NOUI  =~ s%\$topdir%$topdir%g;
$GIMP_LIBS  =~ s%\$topdir%$topdir%g;
$GIMP_LIBS_NOUI =~ s%\$topdir%$topdir%g;

# $...1 variables should be put in front of the corresponding MakeMaker values.
$INC1    = "-I$topdir";
$DEFINE1 = " -Ddatadir=\"\\\"".expand($datadir)."\\\"\"";

eval "use PDL";
if (!$@) {
   require PDL::Version;
   if ($PDL::Version::VERSION > 1.99) {
      require PDL::Core::Dev;
      if (!$@) {
         $PDL=1;
      } else {
         $do_config_msg && print <<EOF;

ERROR:   PDL::Core::Dev module not found ($@),
         this is an error in your PDL installation.

EOF
      }
   } else {
      $do_config_msg && print <<EOF;

WARNING: PDL version $PDL::Version::VERSION is installed. Gimp was only
         tested with 2.0 and higher.  In case of problems its advisable to
         upgrade PDL to at least version 2.

EOF
   }
} else {
   $do_config_msg && print <<EOF;

WARNING: unable to use PDL (the perl data language). This means that
         normal pixel access is non-functional. Unless you plan to use
         Tile/PixelRgn functions together with PDL, this is harmless. The
         plug-ins using PDL, however, will NOT WORK and you can NO LONGER
         install PDL later. You can get PDL from any CPAN mirror.

EOF
}

$do_config_msg && print "checking for PDL support... ", $PDL ? "yes":"no","\n";
if ($PDL) {
   $do_config_msg && print "checking for PDL include path... ",&PDL::Core::Dev::PDL_INCLUDE,"\n";
   $do_config_msg && print "checking for PDL typemap... ",&PDL::Core::Dev::PDL_TYPEMAP,"\n";

   $pdl_inc      = $pdl_inc = &PDL::Core::Dev::PDL_INCLUDE;
   $pdl_typemaps = "@{[@pdl_typemaps = &PDL::Core::Dev::PDL_TYPEMAP]}";
   $DEFINE1 .= " -DHAVE_PDL=1";
} else {
   @pdl_typemaps = "$topdir/typemap.pdl";
}

for(keys %cfg) {
   ($k=$_)=~s/^_//;
   $cfg{$_}=$$k;
}

sub MY::makefile {
   my $self = shift;
   return $self->MM::makefile(@_);
}

1;
