require 5.008;
use ExtUtils::Depends;
use Data::Dumper qw(Dumper);
use ExtUtils::MakeMaker;
use IO::All;

our %cfg;
require './config.pl';

sub MY::postamble {
   my $self=shift;
   <<EOF;
install-po:
	cd po && \$(MAKE) install

install ::
	cd examples && \$(MAKE) install

# generate tags file
tags: .
	ctags --lang=c `find . -name '*.xs' -print`

# set \$VERSION in all modules
setver:
	\$(PERL) -pi -e 's/^(\\s*(?:our)?\\s*\\\$\$VERSION\\s*=\\s*).*;/\$\${1}"\$(VERSION)";/' Gimp/*.pm UI/*.pm Net/*.pm

EOF
}

my $cfg = io("Gimp/Config.pm.in")->all or die "Gimp/Config.pm.in: $!\n";
$cfg =~ s/#CFG#/Data::Dumper->Dump([\%cfg], ['*Gimp::Config'])/e;
io("Gimp/Config.pm")->print($cfg) or die "Gimp/Config.pm: $!\n";

my $pkg = new ExtUtils::Depends Gimp, Gtk2;
$pkg->set_inc($cfg{GIMP_CFLAGS});
$pkg->add_pm(map { ($_ => "\$(INST_LIB)/$_") } qw(Gimp.pm));
$pkg->save_config('Gimp/IFiles.pm');

WriteMakefile(
  dist	=> {
    PREOP	=> 'make setver && chmod -R u=rwX,go=rX . ;',
    COMPRESS	=> 'gzip -9v',
    SUFFIX	=> '.gz',
  },
  MIN_PERL_VERSION => '5.014001',
  PREREQ_PM	=> {
    Gtk2		=> 1.0,
    PDL			=> 2.0,
    Data::Dumper	=> 2.0,
    IO::All		=> 0,
    ExtUtils::Depends	=> 0,
    ExtUtils::PkgConfig	=> 0,
    Gtk2::Ex::PodViewer	=> 0,
    IO::Scalar		=> 0,
    Web::Scraper	=> 0,
    HTML::Entities	=> 0,
  },
  META_MERGE => {
    "meta-spec" => { version => 2 },
    resources => {
      repository => {
	type => 'git',
	url => 'git://git.gnome.org/gimp-perl',
	web => 'https://git.gnome.org/browse/gimp-perl/',
      },
    },
  },
  DIR	=> [ qw/Gimp Net UI examples/ ],
  NAME	=> 'Gimp',
  VERSION_FROM => 'Gimp.pm',
  EXE_FILES => [ map { "utils/$_" } qw(gimpdoc xcftopnm embedxpm scm2perl) ],
  realclean	=> { FILES => "Makefile" },
  clean	=> { FILES => "Makefile.old stamp-h Gimp/Config.pm Gimp/IFiles.pm" },
  $pkg->get_makefile_vars,
);
