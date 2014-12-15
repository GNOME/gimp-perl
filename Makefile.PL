require 5.008;
use ExtUtils::Depends;
use ExtUtils::MakeMaker;

sub MY::postamble {
   my $self=shift;
   <<'EOF';
install-po:
	cd po && \$(MAKE) install

install ::
	cd examples && \$(MAKE) install

# generate tags file
tags: .
	ctags --lang=c `find . -name '*.xs' -print`

# set \$VERSION in all modules
setver:
	$(PERL) -pi -e 's/^(\s*(?:our)?\s*\$$VERSION\s*=\s*)[^a-z]*;/$${1}"$(VERSION)";/' Gimp/*.pm UI/*.pm Net/*.pm

EOF
}

my $pkg = ExtUtils::Depends->new(qw(Gimp Alien::Gimp Gtk2 PDL));
$pkg->add_pm(map { ($_ => "\$(INST_LIB)/$_") } qw(Gimp.pm));
$pkg->add_typemaps("typemap");
$pkg->save_config('Gimp/IFiles.pm');

WriteMakefile(
  NAME	=> 'Gimp',
  VERSION_FROM => 'Gimp.pm',
  dist	=> { PREOP => 'make setver && chmod -R u=rwX,go=rX . ;' },
  MIN_PERL_VERSION => '5.014001',
  PREREQ_PM	=> {
    Gtk2		=> 1.0,
    'PDL'		=> '2.007_04',
    Data::Dumper	=> 2.0,
    IO::All		=> 0,
    ExtUtils::Depends	=> '0.402',
    'Alien::Gimp'	=> '0.01',
    Gtk2::Ex::PodViewer	=> 0,
    IO::Scalar		=> 0,
    Web::Scraper	=> 0,
    HTML::Entities	=> 0,
  },
  TEST_REQUIRES	=> {
    'Test::More'	=> '0.88',
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
  EXE_FILES => [ map { "utils/$_" } qw(gimpdoc xcftopnm embedxpm scm2perl) ],
  clean	=> { FILES => "Gimp/IFiles.pm" },
  $pkg->get_makefile_vars,
);
