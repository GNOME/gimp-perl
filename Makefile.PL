require 5.008;

$|=1;

# run the config.pl to setup environment
do './config.pl' or die $@;

# GTK isn't absolutely required, but for most people it is highly
# desirable, so make a nice big notice saying so.

eval "use Gtk2;"; $GTK = $@ eq "";
$GTK or print <<EOF;

NOTICE:  unable to use the Perl-Gtk2 interface. Many features (like
         Gimp::Fu) rely on this extension. You can build without it (and
         many scripts will be disabled or run with reduced functionality
         only), but it's better to install it.  If you do install it,
	 version 1.0 or higher is required; you can get it from
	 http://gtk2-perl.sourceforge.net/ or any CPAN mirror.

EOF

use ExtUtils::MakeMaker;

sub MY::install {
   my $self=shift;
   package MY;
   my $install = $self->SUPER::install(@_);
   <<'EOF' . $install;
install :: install-writable

install-writable ::
	@for dir in \
	   $(INSTALLPRIVLIB) \
	   $(INSTALLARCHLIB) \
	   $(INSTALLBIN) \
	   $(INSTALLSCRIPT) \
	   $(INSTALLMAN1DIR) \
	   $(INSTALLMAN3DIR) ; \
	do \
	   $(MKPATH) "$$dir"; \
	   if test -d "$$dir" && test -w "$$dir" ; then : ; else \
	      echo "ERROR: installation directory $$dir" ; \
	      echo "       is not writable, not installing gimp-perl" ; \
	      exit 1 ; \
	   fi ; \
	done ;

install-po:
	cd po && \$(MAKE) install

EOF
}

sub MY::postamble {
   my $self=shift;
   my $GT = "$GIMPTOOL --install-admin-bin";
   my $UT = "$GIMPTOOL --uninstall-admin-bin";
   <<EOF;

objclean :: clean
maintainer-clean :: realclean
distclean :: realclean
check :: test

force_uninstall_from_perldirs ::
	\$(PERL) -MExtUtils::Install -e 'uninstall(\$\$ARGV[0],1,0)' @{[$self->catfile('$(PERL_ARCHLIB)','auto','$(FULLEXT)','.packlist')]}

force_uninstall_from_sitedirs ::
	\$(PERL) -MExtUtils::Install -e 'uninstall(\$\$ARGV[0],1,0)' @{[$self->catfile('$(SITEARCHEXP)','auto','$(FULLEXT)','.packlist')]}

# generate tags file
tags: .
	ctags --lang=c `find . -name '*.xs' -print`

# set \$VERSION in all modules
setver:
	\$(PERL) -pi -e 's/^(\\s*\\\$\$VERSION\\s*=\\s*).*\$\$/\$\${1}\$(VERSION);/' *.pm Gimp/*.pm UI/*.pm Net/*.pm

EOF
}

print "writing Gimp/Config.pm... ";
{
   sub conf_eval {
      my $v = expand($cfg{$_[0]});
      $v =~ s/([\\\]])/\\$1/g;
      $v;
   }
   local $/,*FH;
   open FH,"<Gimp/Config.pm.in" or die "Gimp/Config.pm.in: $!\n";
   my $cfg = <FH>;
   $cfg =~ s/#CFG#/join "",
                        map sprintf("   %-20s => q[%s],\n",$_,conf_eval $_),
                            keys %cfg/e;
   open FH,">Gimp/Config.pm" or die "Gimp/Config.pm: $!\n";
   print(FH $cfg)>0 or die "Gimp/Config.pm: $!\n";
}
print "ok\n";

@EXE_FILES = qw(utils/gimpdoc utils/xcftopnm utils/embedxpm);
push @EXE_FILES, qw(utils/scm2perl) if $PRD;

WriteMakefile(
    'dist'	=> {
	            PREOP	=> 'make setver && chmod -R u=rwX,go=rX . ;',
	            COMPRESS	=> 'gzip -9v',
	            SUFFIX	=> '.gz',
	           },
    'PREREQ_PM'	=> {
		    Gtk2		=> 1.0,
		    PDL			=> 0,
		    Data::Dumper	=> 2.0,
		    IO::All		=> 0,
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
    'DIR'	=> [ qw/Gimp Net UI examples/ ],
    'NAME'	=> 'Gimp',
    'VERSION_FROM' => 'Gimp.pm',
    'INSTALLBIN'=> $bindir,
    'LDFROM'	=> expand("\$(OBJECT) $LIBS $INTLLIBS"),
    'INC'	=> "$INC1 $GIMP_INC_NOUI $GTK_CFLAGS $CPPFLAGS $CFLAGS",
    'DEFINE'	=> "$DEFINE1 $DEFS",
    'EXE_FILES'	=> \@EXE_FILES,
    'macro'	=> \%cfg,
    'realclean'	=> { FILES => "config.status config.cache config.log config.h Gimp/Config.pm Makefile" },
    'clean'	=> { FILES => "Makefile.old stamp-h Gimp/Config.pm" },
);
