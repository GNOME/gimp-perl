require 5.008;

$|=1;

do '../config.pl' or die $@;

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
      Perl-Server
      animate_cells
      blended2 blowinout bricks burst
      centerguide
      ditherize dots dust
      frame_filter frame_reshuffle
      glowing_steel goldenmean guidegrid guides_to_selection
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

use ExtUtils::MakeMaker;

push @pins, @pdl_pins if $PDL;
push @pins, @gtk_pins if $GTK;

sub MY::install {
   my $self=shift;
   package MY;
   my $install = $self->SUPER::install(@_);
   <<'EOF' . $install;
install :: install-plugins

EOF
}

sub basename {
  my $dest = shift;
  $dest =~ s#.*/##;
  $dest;
}

sub plugin_target {
  my $plugin = shift;
  my $dest = '$(INST_PLUGINS)/'.basename($plugin);
  <<EOF;
$dest : $plugin \$(FIRST_MAKEFILE) \$(INST_PLUGINS)\$(DFSEP).exists
	\$(NOECHO) \$(RM_F) $dest
	\$(CP) $plugin $dest
	\$(FIXIN) $dest
	-\$(NOECHO) \$(CHMOD) \$(PERM_RWX) $dest

EOF
}

sub MY::postamble {
   my $self=shift;
   my $GT = "$GIMPTOOL --install-admin-bin";
   my $UT = "$GIMPTOOL --uninstall-admin-bin";
   <<EOF;

INST_PLUGINS = ../blib/plugins

\$(INST_PLUGINS)\$(DFSEP).exists :: Makefile.PL
	\$(NOECHO) \$(MKPATH) \$(INST_PLUGINS)
	\$(NOECHO) \$(CHMOD) \$(PERM_DIR) \$(INST_PLUGINS)
	\$(NOECHO) \$(TOUCH) \$(INST_PLUGINS)\$(DFSEP).exists

@{[ join '', map { plugin_target($_) } @pins ]}

pure_all :: pure_plugins

pure_plugins : @{[ map { "\$(INST_PLUGINS)/".basename($_) } @pins ]}

install-plugins :: pure_plugins
	cd \$(INST_PLUGINS) ; for p in * ; do $GT "\$\$p" ; done

uninstall ::
	cd \$(INST_PLUGINS); for plugin in *; do $UT "\$\$plugin"; done

EOF
}

WriteMakefile(
    'NAME'	=> 'Gimp',
    'VERSION_FROM' => '../Gimp.pm',
    'clean'	=> { FILES => "Makefile.old" },
);
