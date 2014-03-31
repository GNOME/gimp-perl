use Cwd 'abs_path';
use ExtUtils::PkgConfig;

# make $topdir be where the gimp-perl sources start
$topdir = ".";
$topdir .= "/.." while ! -f "$topdir/MANIFEST";
$topdir = abs_path $topdir;

my %gimpcfg = ExtUtils::PkgConfig->find("gimp-2.0");
my $gimppath = ExtUtils::PkgConfig->variable("gimp-2.0", "exec_prefix")."/bin/";
my $gimptool = expand($gimppath . "gimptool-2.0");
my ($plugindir, $pluginlibs) = split /\n/, `$gimptool --gimpplugindir --libs`;

my $gimpbinname = ExtUtils::PkgConfig->modversion("gimp-2.0");
$gimpbinname =~ s/^(\d\.\d).*/$1/; # strip off minor versions
die "Need GIMP version at least 2.8.0\n" unless $gimpbinname >= 2.8;

eval "use PDL";
$PDL= !$@;
require PDL::Core::Dev;

%cfg = (
  GIMP => expand($gimppath . "gimp-" . $gimpbinname),
  GIMPTOOL => $gimptool,
  GIMP_LIBS => exp_topdir($pluginlibs),
  GIMP_LIBS_NOUI => exp_topdir($gimpcfg{"libs"}),
  gimpplugindir => $plugindir,
  pdl_typemaps => [$PDL ? &PDL::Core::Dev::PDL_TYPEMAP : "$topdir/typemap.pdl"],
  GIMP_CFLAGS => " -I$topdir -Ddatadir=\"\\\"".expand($datadir)."\\\"\" "
    . ($PDL ? "-DHAVE_PDL=1 " . &PDL::Core::Dev::PDL_INCLUDE : '')
    . ' ' . (add_ingimp(exp_topdir($gimpcfg{"cflags"}))) . ' ',
  INTLLIBS => expand(q[]),
);

sub expand {
  my $var = shift;
  return '' unless defined $var;
  my $count = 5;
  $var =~ s%\$\(top_builddir\)%$topdir/../../%g;
  while($var=~/\$\{/ and $count--) {
    $var=~s/\$\{$k\}/$v/g while ($k,$v) = each %cfg;
  }
  $var;
}

sub exp_topdir { local $_ = shift; s%\$topdir%$topdir%g; $_ }
sub add_ingimp { local $_ = shift; $_ = "-I$topdir/../.. $_"  if $IN_GIMP; $_ }

1;
