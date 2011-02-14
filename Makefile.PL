require 5.008;

use Config;
require ExtUtils::PkgConfig;
require ExtUtils::Depends;

$|=1;


if ($ARGV[0] ne "--writemakefile") {
   # convert any prefix= to --prefix=
   for(@ARGV) {
      s/^prefix=/--prefix=/i;
   }
   
   # run the config.pl to setup enviornment
   do './config.pl';
   
   # Set it up to do lots of tests
   $EXTENSIVE_TESTS = 1;
   $ENV{'EXTENSIVE_TESTS'} = $EXTENSIVE_TESTS;
  
   # get compile/link args
   $ENV{CC}=$Config{cc};
   $ENV{LD}=$Config{ld};
   $ENV{prefix}=$Config{prefix};
   $ENV{PERL}=$Config{perlpath};
   $ENV{MAKEFILE_PL}=$0;
}

@otherdeps = qw(
); 

# list of plugins using gtk directly (Gtk absolutely required)
@gtk_pins =
   qw(
     dataurl 
     colorhtml 
     fade-alpha
   );

# list of standard plugins
@pins =
   qw(
      animate_cells 
      blended2 blowinout bricks burst  
      centerguide  
      ditherize dots dust 
      font_table frame_filter frame_reshuffle 
      glowing_steel goldenmean guidegrid guide_remove guides_to_selection
      image_tile innerbevel  
      layerfuncs mirrorsplit 
      perlotine prep4gif 
      repdup roundsel 
      scratches sethspin stamps 
      tex-to-float translogo 
      randomart1 randomblends 
      selective_sharpen
      warp-sharp webify windify 
      xachlego xachshadow xachvision
      yinyang 
   );
# list of plugins using PDL

@pdl_pins =
   qw(
      gouge 
      redeye
      pixelmap 
      view3d 
   );  

# plugins that are useful code examples, but not even arguably useful 
# gimp plugins due to silliness or duplication

@uninstalled_pins = 
  qw(
     example-fu
     example-net
     map_to_gradient 
  );

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

require ExtUtils::MakeMaker;
import ExtUtils::MakeMaker;

push @pins, @pdl_pins if $PDL;
push @pins, @gtk_pins if $GTK;

@INC = grep /^\//,@INC; # comment out as well?

sub MY::install {
   my $self=shift;
   package MY;
   my $install = $self->SUPER::install(@_);
   ($install =~ s/\b(un)?install\b/really-$1install/g) or return $install;
   '
install ::
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
	      echo ; \
	      echo "ERROR: installation directory \"$$dir\"" ; \
	      echo "       is not writable, not installing gimp-perl" ; \
	      echo ; \
	      exit ; \
	   fi ; \
	done ; \
	$(MAKE) really-install install-plugins'."

install-po:
	cd po && \$(MAKE) install
        
".$install;
}

sub MY::postamble {
   my $self=shift;
   my $install_program = $cfg{INSTALL};
   $install_program =~ s{^\.\./}{../../};
   my $GT = "$GIMPTOOL --install-admin-bin";
   my $UT = "$GIMPTOOL --uninstall-admin-bin";
   my $GT2 = '';
   my $postamble="

objclean :: clean
maintainer-clean :: realclean
distclean :: realclean
check :: test

MY_FIXIN = \$(PERL) -I\$(PERL_ARCHLIB) -I\$(PERL_LIB) -MExtUtils::MakeMaker -e 'MY->fixin(\@ARGV)'

all ::

clean ::
	test -f Makefile || mv -f Makefile.old Makefile
	\$(RM_RF) inst-temp

install-plugins ::
	\$(RM_RF) inst-temp
	\$(MKPATH) inst-temp
	cd inst-temp ; \\
	\$(UMASK_NULL) ; \\
	\$(CP) ".join(' ',map("'../examples/$_'",sort @pins))." ../Perl-Server . ; \\
	\$(CHMOD) 755 * ; \\
	\$(MY_FIXIN) * ; \\
	for plugin in * ; do \\
	   $GT \"\$\$plugin\" $GT2 ; \\
	done
	\$(RM_RF) inst-temp

uninstall :: force_uninstall_from_\$(INSTALLDIRS)dirs
	for plugin in Perl-Server @pins; do \\
           $UT $GT2\$\$plugin; \\
        done

force_uninstall_from_perldirs ::
	\$(PERL) -MExtUtils::Install -e 'uninstall(\$\$ARGV[0],1,0)' ".$self->catfile('$(PERL_ARCHLIB)','auto','$(FULLEXT)','.packlist')."

force_uninstall_from_sitedirs ::
	\$(PERL) -MExtUtils::Install -e 'uninstall(\$\$ARGV[0],1,0)' ".$self->catfile('$(SITEARCHEXP)','auto','$(FULLEXT)','.packlist')."

# generate tags file
tags: .
	ctags --lang=c `find . -name '*.xs' -print`

# set \$VERSION in all modules
setver:
	\$(PERL) -pi -e 's/^(\\s*\\\$\$VERSION\\s*=\\s*).*\$\$/\$\${1}\$(VERSION);/' *.pm Gimp/*.pm UI/*.pm Net/*.pm

gimpenums.c: makeenums.pl
	perl makeenums.pl arrays ~/cvs/gimp/libgimp*/*.h >gimpenums.c

";

   $postamble;
}

@DIRS= qw/Gimp Net UI/;

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

@EXE_FILES = qw(gimpdoc xcftopnm embedxpm);
push @EXE_FILES, qw(scm2perl) if $PRD;

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
		   },
    'DIR'	=> [@DIRS],
    'NAME'	=> 'Gimp',
    'VERSION_FROM' => 'Gimp.pm',
    'INSTALLBIN'=> $bindir,
    'PM'	=> {
    		    'Gimp.pm'		=> '$(INST_LIBDIR)/Gimp.pm',
    		    'Gimp/Data.pm'	=> '$(INST_LIBDIR)/Gimp/Data.pm',
    		    'Gimp/Fu.pm'	=> '$(INST_LIBDIR)/Gimp/Fu.pm',
    		    'Gimp/Lib.pm'	=> '$(INST_LIBDIR)/Gimp/Lib.pm',
    		    'Gimp/Util.pm'	=> '$(INST_LIBDIR)/Gimp/Util.pm',
    		    'Gimp/Feature.pm'	=> '$(INST_LIBDIR)/Gimp/Feature.pm',
    		    'Gimp/Module.pm'	=> '$(INST_LIBDIR)/Gimp/Module.pm',
    		    'Gimp/Config.pm'	=> '$(INST_LIBDIR)/Gimp/Config.pm',
    		   },
    'LDFROM'	=> expand("\$(OBJECT) $LIBS $INTLLIBS"),
    'INC'	=> "$INC1 $GIMP_INC_NOUI $CPPFLAGS $CFLAGS",
    'DEFINE'	=> "$DEFINE1 $DEFS",
    'EXE_FILES'	=> \@EXE_FILES,
    'macro'	=> \%cfg,
    'realclean'	=> { FILES => "config.status config.cache config.log config.h Gimp/Config.pm Makefile" },
    'clean'	=> { FILES => "Makefile.old stamp-h" },
);

print <<EOF;

Hopefully, Gimp is now correctly configured. you can now enter "make",
"make test" and "make install".

EOF

__END__
# write an empty makefile (the last chance to stop)
# and warn the user.
sub emergency_stop {
   print STDERR "WARNING: *** perl extension will not be built ($_[0]) ***\n\n";
   open MAKEFILE,">Makefile" or die "unable to create Makefile: $!";
   print MAKEFILE <<EOF;
all install check:

clean mostlyclean objclean:

distclean maintainer-clean realclean clobber: clean
	\$(RM_F) Makefile config.cache config.log
        \$(RM_F) config.h config.status stamp-h Makefile.old Gimp/Config.pm
        \$(RM_RF) test-dir inst-temp

EOF
   close MAKEFILE;
   exit;
}
