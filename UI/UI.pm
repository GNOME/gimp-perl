package Gimp::UI;

use Gimp ('__');
use Gimp::Fu;
use Gtk2;
use IO::All;
use List::Util qw(min);
use strict;

our (@ISA, $VERSION);
BEGIN {
   $VERSION = 2.3002;
   eval {
      require XSLoader;
      XSLoader::load Gimp::UI $VERSION;
   } or do {
      require DynaLoader;
      @ISA = qw(DynaLoader);
      bootstrap Gimp::UI $VERSION;
   };
}

# shows the properties of a glib object
#d# just to debug
sub info {
   my ($idx, $obj) = @_;
   my %seen;
   return if $seen{$idx}++;
   print "\n$idx\n";
   for ($obj->list_properties) {
      printf "%-16s %-24s %-24s %s\n", $_->{name}, $_->{type}, (join ":", @{$_->{flags}}), $_->{descr};
   }
}

@Gimp::UI::ImageMenu::ISA   =qw(Gimp::UI);
@Gimp::UI::LayerMenu::ISA   =qw(Gimp::UI);
@Gimp::UI::ChannelMenu::ISA =qw(Gimp::UI);
@Gimp::UI::DrawableMenu::ISA=qw(Gimp::UI);

sub image_name {
   my $name = $_[0]->get_filename;
   $name.="-".${$_[0]} if $name eq "Untitled";
   $name;
}

sub Gimp::UI::ImageMenu::_items {
  map [[$_],$_,image_name($_)],
      Gimp::Image->list;
}
sub Gimp::UI::LayerMenu::_items {
  map { my $i = $_; map [[$i,$_],$_,image_name($i)."/".$_->drawable_get_name],$i->get_layers }
      Gimp::Image->list;
}

sub Gimp::UI::ChannelMenu::_items {
  map { my $i = $_; map [[$i,$_],$_,image_name($i)."/".$_->drawable_get_name],$i->get_channels }
      Gimp::Image->list;
}

sub Gimp::UI::DrawableMenu::_items {
  map { my $i = $_; map [[$i,$_],$_,image_name($i)."/".$_->drawable_get_name],($i->get_layers, $i->get_channels) }
      Gimp::Image->list;
}

sub new($$$$) {
   my ($class,$constraint,$active,$var)=@_;
   my (@items)=$class->_items;
   my $menu = new Gtk2::Menu;
   for(@items) {
      my ($constraints,$result,$name)=@$_;
      next unless $constraint->(@{$constraints});
      my $item = new Gtk2::MenuItem $name;
      $item->signal_connect(activate => sub { $$var=$result });
      $menu->append($item);
   }
   if (@items) {
      $$var=$items[0]->[1];
   } else {
      my $item = new Gtk2::MenuItem __"(none)";
      $menu->append($item);
      $$var=undef;
   }
   $menu->show_all;
   $menu;
}

package Gimp::UI::PreviewSelect;

# Parent widget that handles a generic preview selection.
#
# pure virtual methods (must be implemented by child):
#                  ->get_list
#                  ->get_title
#                  ->get_pixbuf
#TODO: Add preview (or portion of preview) directly to button

use Gtk2::SimpleList;

our @ISA = 'Glib::Object';

Glib::Type->register (
   'Gtk2::Button', __PACKAGE__,
   signals => {},
   properties => [
     Glib::ParamSpec->string  ('active',
                               'Active',
                               'The active child',
                               '',
                               [qw/readable writable/]),
        ],
);

sub SET_PROPERTY {
        my ($self, $pspec, $newval) = @_;

        if ($pspec->get_name eq 'active') {
          $self->{active} = $newval;
	  $self->set_label($newval);
        }
}

sub GET_PROPERTY {
        my ($self, $pspec) = @_;
        if ($pspec->get_name eq 'active') {
                return $self->{active};
        }
}

sub INIT_INSTANCE {
       my ($self) = @_;
       $self->signal_connect("clicked", \&preview_dialog);
       my $lbl = new Gtk2::Label $self->get("active");
       $self->add($lbl);
}

sub preview_dialog {
   my ($self) = @_;

   my $w = new Gtk2::Dialog;
   $w->set_title($self->get_title);
   $w->set_default_size(400,300);
   $w->action_area->set_border_width(2);
   $w->action_area->set_homogeneous(0);

   (my $h=new Gtk2::HBox 0,0)->show;
   $w->vbox->pack_start($h,1,1,0);

   (my $s=new Gtk2::ScrolledWindow undef,undef)->show;
   $s->set_policy(-automatic, -automatic);
   $s->set_size_request(200,300);
   $h->pack_start($s,1,1,0);

   my $datalist = new Gtk2::SimpleList (
       'Name' => 'text',
       'Preview' => 'pixbuf',
       );

   for(sort $self->get_list) {
     my $listname = $_;
     my $pixbuf = $self->new_pixbuf($listname);
     push @{$datalist->{data}}, [ $listname, $pixbuf ];
   }

   $datalist->get_selection->set_mode('single');
   $datalist->get_selection->unselect_all;

   $s->add($datalist);
   $datalist->show;

   my $hbbox = new Gtk2::HButtonBox;
   $hbbox->set_spacing(2);
   $w->action_area->pack_end($hbbox,0,0,0);
   show $hbbox;

   my $button = new Gtk2::Button->new_from_stock('gtk-cancel');
   signal_connect $button clicked => sub {hide $w};
   $hbbox->pack_start($button,0,0,0);
   can_default $button 1;
   show $button;

   $button = new Gtk2::Button->new_from_stock('gtk-ok');
   signal_connect $button clicked => sub {
     my @sel = $datalist->get_selected_indices;
     my @row =  $datalist->{data}[$sel[0]];

     # this no longer works, so use a scalar instead (muppet's suggestion)
     # $self->set( 'active', $row[0][0] );

     $self->set( active => scalar($row[0][0]) );
     hide $w;
     };
   $hbbox->pack_start($button,0,0,0);
   can_default $button 1;
   grab_default $button;
   show $button;

   show $w;
}

package Gimp::UI::PatternSelect;

our @ISA = 'Glib::Object';

Glib::Type->register (
   'Gimp::UI::PreviewSelect', __PACKAGE__,
   signals => {},
   properties => [],
);

sub get_title { Gimp::__"Pattern Selection Dialog" }
sub get_list { Gimp::Patterns->get_list("") }

sub new_pixbuf {
   my ($w,$h,$bpp,$mask)=Gimp::Pattern->get_pixels ($_);
   my $has_alpha = ($bpp==2 || $bpp==4);

   if ($bpp==1) {
      my @graydat = unpack "C*", $mask;
      my @rgbdat;
      foreach (@graydat) {
	 push @rgbdat, $_; push @rgbdat, $_; push @rgbdat, $_;
      }
      $mask = pack "C*", @rgbdat;
   } elsif($bpp == 3) {
      $mask = pack "C*", @{$mask};
   } elsif($bpp == 4) {
      $mask = pack "C*", @{$mask}[0..2];
   }

   # TODO: Add code/test for handling GRAYA; don't have any GRAYA to test
   # with currently though.

   Gtk2::Gdk::Pixbuf->new_from_data(
      $mask,'rgb', $has_alpha?1:0, 8, $w, $h, $has_alpha?$w*4:$w*3
   );
}

package Gimp::UI::BrushSelect;

our @ISA = 'Glib::Object';

Glib::Type->register (
   'Gimp::UI::PreviewSelect', __PACKAGE__,
   signals => {},
   properties => [],
);

sub get_title { Gimp::__"Brush Selection Dialog" }
sub get_list { Gimp::Brushes->get_list("") }

sub new_pixbuf {
   my ($w,$h,$mask_bpp,$mask,$color_bpp,$color_data) = Gimp::Brush->get_pixels($_);

   my @rgbdat;

   # color bitmaps seem broken from gimp's side, but I'm leaving it in here.
   # if you notice this and care, let me know and I may fix the gimp side.
   if ($color_bpp == 3)
   {
      @rgbdat = @{$color_data} ;
   }
   elsif ($color_bpp == 0)
   {
     my @graydat = @{$mask};
     foreach (@graydat)
     {
        $_ = 255 - $_;
        push @rgbdat, $_; push @rgbdat, $_; push @rgbdat, $_;
     }
   }

   my $display = pack "C*", @rgbdat;
   Gtk2::Gdk::Pixbuf->new_from_data($display,'rgb',0,8,$w,$h,$w*3);
}

package Gimp::UI::GradientSelect;

our @ISA = 'Glib::Object';

Glib::Type->register (
   'Gimp::UI::PreviewSelect', __PACKAGE__,
   signals => {},
   properties => [],
);

sub get_title { Gimp::__"Gradient Selection Dialog" }
sub get_list { Gimp::Gradients->get_list("") }

sub new_pixbuf {
   use POSIX;
   my @grad_row = map { $_ = abs(ceil($_*255 - 0.5)) }
                   Gimp::Gradient->get_uniform_samples ($_,100,0);

# make it 16 tall; there's bound to be a better way to do this? (its slow)
   push @grad_row, @grad_row, @grad_row, @grad_row,
        @grad_row, @grad_row, @grad_row, @grad_row,
        @grad_row, @grad_row, @grad_row, @grad_row,
        @grad_row, @grad_row, @grad_row, @grad_row;

   my $pb = Gtk2::Gdk::Pixbuf->new_from_data(
      pack "C*", @grad_row,'rgb',1,8,100,8,100*4
   );
}


package Gimp::UI;

sub _new_adjustment {
   my @adj = eval { @{$_[1]} };

   $adj[2] ||= ($adj[1] - $adj[0]) * 0.01;
   $adj[3] ||= ($adj[1] - $adj[0]) * 0.01;
   $adj[4] ||= 0;

   new Gtk2::Adjustment $_[0], @adj;
}

# find a suitable value for the "digits" value
sub _find_digits {
   my $adj = shift;
   my $digits = log ($adj->step_increment || 1) / log(0.1);
   $digits > 0 ? int $digits + 0.9 : 0;
}

sub help_window(\$$$) {
   my ($helpwin, $title, $help) = @_;
   unless ($$helpwin) {
      $$helpwin = new Gtk2::Dialog;
      $$helpwin->set_title(sprintf __"Help for %s", $title);
      $$helpwin->action_area->set_border_width (2);
      my $tophelp = new Gtk2::Label $help;
      $tophelp->set_alignment(0.5,0.5);
      $$helpwin->vbox->pack_start($tophelp,0,1,3);
      my $sw = new Gtk2::ScrolledWindow undef,undef;
      $sw->set_policy (-automatic, -automatic);
      $sw->set_size_request(500,600);
      require Gtk2::Ex::PodViewer;
      my $pv = new Gtk2::Ex::PodViewer;
      require FindBin;
      $pv->load("$FindBin::RealBin/$FindBin::RealScript");
      $pv->show;
      $sw->add($pv);
      $$helpwin->vbox->add($sw);
      my $button = Gtk2::Button->new_from_stock('gtk-ok');
      signal_connect $button clicked => sub { hide $$helpwin };
      $$helpwin->action_area->add ($button);
      $$helpwin->signal_connect (destroy => sub { undef $$helpwin });
   }
   $$helpwin->show_all;
}

sub _instrument {
  return unless $Gimp::verbose;
  my $obj = shift;
  my $class = ref $obj;
  my %sig2done;
  map {
    my $c = $_;
    map {
#warn "$c:$_->{signal_name}\n";
      my $s = $_->{signal_name};
      $obj->signal_connect(
	$s => sub { warn "SIG:$s(@_)\n";0 }
      ) unless $sig2done{$s};
      $sig2done{$s} = 1;
    } Glib::Type->list_signals($c);
  } Glib::Type->list_ancestors($class);
}

# function($name,$desc,$default,$extra,$value) returns $widget,\&setval,\&getval
my %PF2INFO = (
  &PF_STRING => sub {
    my $e = new Gtk2::Entry;
    ($e, sub { set_text $e $_[0] // "" }, sub { get_text $e });
  },
  &PF_SLIDER => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my $adj = _new_adjustment ($value, $extra);
    my $a = new Gtk2::HScale $adj;
    $a->set_digits (_find_digits $adj);
    $a->set_size_request(120,-1);
    ($a, sub { $adj->set_value($_[0]) }, sub { $adj->get_value });
  },
  &PF_SPINNER => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my $adj = _new_adjustment ($value, $extra);
    my $a = new Gtk2::SpinButton $adj, 1, 0;
    $a->set_digits (_find_digits $adj);
    ($a, sub { $adj->set_value($_[0]) }, sub { $adj->get_value });
  },
);
%PF2INFO = (
  %PF2INFO,
  &PF_FLOAT => $PF2INFO{&PF_STRING},
  &PF_FILE => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my ($a, $set, $get) = $PF2INFO{&PF_STRING}->();
    my $s = $a;
    $a = new Gtk2::HBox 0,5;
    $a->add ($s);
    my $b = new Gtk2::Button __"Browse";
    $a->add ($b);
    my $f = new Gtk2::FileSelection $desc;
    $b->signal_connect(clicked => sub { $f->set_filename ($s->get_text); $f->show_all });
    $f->ok_button->signal_connect(clicked => sub { $f->hide; $s->set_text($f->get_filename) });
    $f->cancel_button->signal_connect(clicked => sub { $f->hide });
    ($a, $set, $get);
  },
  &PF_ADJUSTMENT => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my (@x)=@$default;
    $value=shift @x;
    $PF2INFO{pop(@x) ? &PF_SPINNER : &PF_SLIDER}->(
      $name, $desc, $default, [@x], $value,
    );
  },
  &PF_INT8 => sub { $PF2INFO{&PF_SLIDER}->(@_[0..2], [ 0, 255, 1 ], $_[4]); },
  &PF_INT16 => sub {
    my $max = 1 << 15;
    $PF2INFO{&PF_SPINNER}->(@_[0..2], [ -$max, $max - 1, 1 ], $_[4]);
  },
  &PF_INT32 => sub {
    my $max = 1 << 31;
    $PF2INFO{&PF_SPINNER}->(@_[0..2], [ -$max, $max - 1, 1 ], $_[4]);
  },
  &PF_FONT => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    $default = 'Sans' unless $default;
    $value = $default unless $value;
    my ($a, $s, $g);
    if ($Gimp::interface_pkg ne 'Gimp::Net') {
      $a = new Gimp::UI::FontSelectButton $desc, $default;
      $s = sub { $a->set_font($_[0]) };
      $g = sub { $a->get_font };
#      _instrument($a);
    } else {
      # no GIMP ui available, use Gtk2 equivalent
      my $fs = new Gtk2::FontSelectionDialog sprintf __"Font Selection Dialog (%s)", $desc;
      my $val;
      my $l = new Gtk2::Label "!error!";
      my $setval = sub {
	$val = shift;
	$val =~ s#\s*(Bold)?\s*(Italic)?\s*\d+$##; # vim highlighter
	unless (defined $val && $fs->set_font_name ("$val 10")) {
	  warn sprintf __"Illegal default font description: %s\n", $val
	    if defined $val;
	  $val = $default;
	  $fs->set_font_name ("$val 10");
	}
	$l->set (label => " $val ");
      };
      $fs->ok_button->signal_connect (clicked => sub {$setval->($fs->get_font_name); $fs->hide});
      $fs->cancel_button->signal_connect (clicked => sub {$fs->hide});
      $s = $setval;
      $g = sub { $val };
      $a = new Gtk2::Button;
      $a->add ($l);
      $a->signal_connect (clicked => sub { show $fs });
    }
    ($a, $s, $g);
  },
  &PF_COLOR => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    $default = [0.8,0.6,0.1] unless defined $default;
    $default = Gimp::canonicalize_color($default);
    my $b = new Gimp::UI::ColorButton $desc, 90, 14, $default, 'small-checks';
#    _instrument($b);
    ($b, sub {
      $b->set_color (defined $_[0] ? Gimp::canonicalize_color $_[0] : [0.8,0.6,0.1])
    }, sub { $b->get_color });
  },
  &PF_TOGGLE => sub {
    my $a = new Gtk2::CheckButton;
    ($a, sub{ $a->set (active => $_[0] ? 1 : 0)}, sub{ $a->get("active") });
  },
  &PF_RADIO => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my $b = new Gtk2::HBox 0,5;
    my ($r,$prev);
    my $prev_sub = sub { $r = $_[0] };
    while (@$extra) {
      my $label = shift @$extra;
      my $value = shift @$extra;
      my $radio = new Gtk2::RadioButton undef, $label;
      $radio->set_group ($prev) if $prev;
      $b->pack_start ($radio, 1, 0, 5);
      $radio->signal_connect (clicked => sub { $r = $value });
      my $prev_sub_my = $prev_sub;
      $prev_sub = sub { $radio->set_active ($_[0] eq $value); &$prev_sub_my };
      $prev = $radio;
    }
    $a = new Gtk2::Frame;
    $a->add($b);
    ($a, $prev_sub, sub { $r });
  },
  &PF_IMAGE => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my $res;
    $a=new Gtk2::HBox (0,5);
    my $b=new Gtk2::OptionMenu;
    $b->set_menu(new Gimp::UI::ImageMenu(sub {1},-1,\$res));
    $a->pack_start ($b,1,1,0);
    ($a, sub { }, sub { $res });
#           my $c = new Gtk2::Button "Load";
#           signal_connect $c "clicked", sub {$res = 2; main_quit Gtk2};
##           $g->attach($c,1,2,$res,$res+1,{},{},4,2);
#           $a->pack_start ($c,1,1,0);
  },
  &PF_LAYER => sub {
    my $res;
    my $a=new Gtk2::OptionMenu;
    $a->set_menu(new Gimp::UI::LayerMenu(sub {1},-1,\$res));
    ($a, sub { }, sub { $res });
  },
  &PF_CHANNEL => sub {
    my $res;
    my $a=new Gtk2::OptionMenu;
    $a->set_menu(new Gimp::UI::ChannelMenu(sub {1},-1,\$res));
    ($a, sub { }, sub { $res });
  },
  &PF_DRAWABLE => sub {
    my $res=13;
    $a=new Gtk2::OptionMenu;
    $a->set_menu(new Gimp::UI::DrawableMenu(sub {1},-1,\$res));
    ($a, sub { }, sub { $res });
  },
  &PF_PATTERN => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my $a=new Gimp::UI::PatternSelect;
    ($a,
      sub { $a->set('active', $value // (Gimp::Context->get_pattern)[0]) },
      sub { $a->get('active') });
  },
  &PF_BRUSH => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my $a=new Gimp::UI::BrushSelect;
    ($a,
      sub{ $a->set('active', $value // (Gimp::Context->get_brush)[0]) },
      sub{ $a->get('active') });
  },
  &PF_GRADIENT => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    $a=new Gimp::UI::GradientSelect;
    ($a,
      sub { $a->set('active', $value // (Gimp::Gradients->get_list(""))[0]) },
      sub { $a->get('active') });
  },
  &PF_CUSTOM => sub { goto &{$_[3]}; },
  &PF_TEXT => sub {
    my ($name,$desc,$default,$extra,$value) = @_;
    my $a = new Gtk2::Frame;
    my $h = new Gtk2::VBox 0,5;
    $a->add($h);
    my $b = new Gtk2::TextBuffer;
    my $e = new_with_buffer Gtk2::TextView $b;
    $e->set_size_request(300,200);
    $e->set_wrap_mode('GTK_WRAP_WORD');
    $e->set_editable (1);
    $h->add ($e);
    my $buttons = new Gtk2::HBox 1,5;
    $h->add ($buttons);
    my $sv = sub { $b->set_text ($_[0]); };
    my $gv = sub {$b->get_text ($b->get_start_iter, $b->get_end_iter, 0);};
    my $load = Gtk2::Button->new_from_stock('gtk-open');
    my $save = Gtk2::Button->new_from_stock('gtk-save');
    my $edit = Gtk2::Button->new_from_stock('gimp-edit');
    $buttons->add ($load);
    $buttons->add ($save);
    $buttons->add ($edit);
    $edit->signal_connect (clicked => sub {
      my $tmp = Gimp->temp_name("txt");
      io($tmp)->utf8->print(&$gv);
      system 'gedit', $tmp;
      $sv->(io($tmp)->utf8->all);
    });
    my $filename = (eval { Gimp->directory } || ".") . "/";
    my $f = new Gtk2::FileSelection sprintf __"Fileselector for %s", $name;
    $f->set_filename ($filename);
    $f->cancel_button->signal_connect (clicked => sub { $f->hide });
    my $lf = sub { $f->hide; $sv->(io($f->get_filename)->utf8->all); };
    my $sf = sub { $f->hide; io($f->get_filename)->utf8->print(&$gv); };
    my $lshandle;
    $load->signal_connect (clicked => sub {
      $f->set_title(sprintf __"Load %s", $name);
      $f->ok_button->signal_handler_disconnect($lshandle) if $lshandle;
      $lshandle=$f->ok_button->signal_connect (clicked => $lf);
      $f->show_all;
    });
    $save->signal_connect (clicked => sub {
      $f->set_title(sprintf __"Save %s", $name);
      $f->ok_button->signal_handler_disconnect($lshandle) if $lshandle;
      $lshandle=$f->ok_button->signal_connect (clicked => $sf);
      $f->show_all;
    });
    ($a, $sv, $gv);
  },
);

sub interact($$$$@) {
  warn __PACKAGE__ . "::interact(@_)" if $Gimp::verbose;
  my $function = shift;
  my $blurb = shift;
  my $help = shift;
  my @params = @{+shift};
  my $menupath = shift;
  my (@getvals, @setvals, @lastvals, @defaults);
  my ($button, $box, $bot, $g);
  my $helpwin;
  my $res = 0;
  my @res;

  Gimp::gtk_init;
  my $exception_text;
  my $exception = sub { $exception_text = $_[0]; Gtk2->main_quit; };
  Glib->install_exception_handler($exception);
  Glib::Log->set_handler(
    'GLib-GObject', [
      qw(G_LOG_FATAL_MASK G_LOG_LEVEL_CRITICAL G_LOG_LEVEL_ERROR
      G_LOG_FLAG_FATAL G_LOG_LEVEL_WARNING)
    ], $exception
  );

  my $t = new Gtk2::Tooltips;
  my $w = new Gtk2::Dialog;

  for(;;) {
    my $title = $menupath;
    $title =~ s#.*/##; $title =~ s#[_\.]##g;
    set_title $w "Perl-Fu: $title";
    $w->set_border_width(3); # sets border on inside because it's a window
    $w->action_area->set_spacing(2);
    $w->action_area->set_homogeneous(0);
    signal_connect $w destroy => sub { main_quit Gtk2 };
    my $topblurb = new Gtk2::Label $blurb;
    $topblurb->set_alignment(0.5,0.5);
    $w->vbox->pack_start($topblurb,0,1,3);
    $g = new Gtk2::Table scalar @params,2,0;
    $g->set(border_width => 4);
    for(@params) {
      my ($type,$name,$desc,$default,$extra)=@$_;
      my ($value)=shift;
      $value=$default unless defined $value;
      die sprintf __"Unsupported argumenttype %s for %s\n", $type, $name
	unless $PF2INFO{$type};
      my ($a, $sv, $gv) = $PF2INFO{$type}->(
	$name,$desc,$default,$extra,$value
      );
      push @setvals, $sv;
      push @getvals, $gv;
      push @lastvals, $value;
      push @defaults, $default;
      $sv->($value);
      my $label = new Gtk2::Label "$desc: ";
      $label->set_alignment(1.0,0.5);
      $g->attach($label, 0, 1, $res, $res+1, ["expand","fill"], ["expand","fill"], 4, 2);
      set_tip $t $a,$desc;
      my $halign = new Gtk2::HBox 0,0;
      $halign->pack_start($a,0,0,0);
      $g->attach($halign, 1, 2, $res, $res+1, ["expand","fill"], ["expand","fill"], 4, 2);
      $res++;
    }
    my $sw = new Gtk2::ScrolledWindow undef,undef;
    $sw->set_policy (-automatic, -automatic);
    $sw->add_with_viewport($g);
    $w->vbox->add($sw);

    my $hbbox = new Gtk2::HButtonBox;
    $hbbox->set_spacing (4);
    $w->action_area->pack_end ($hbbox, 0, 0, 2);

    $button = new Gtk2::Button->new_from_stock('gtk-help');
    signal_connect $button clicked => sub { help_window ($helpwin, $title, $help) };
#     can_default $button 1;
    $hbbox->pack_start($button, 0, 0, 0);

    $button = new Gtk2::Button->new_from_stock('gimp-reset');
    signal_connect $button clicked => sub {
      for my $i (0..$#defaults) {
	$setvals[$i]->($defaults[$i]);
      }
    };
    $hbbox->pack_start ($button, 0, 0, 0);
    #  set_tip $t $button,__"Reset all values to their default";

    $button = new Gtk2::Button->new_from_stock('gtk-cancel');
    signal_connect $button clicked => sub { hide $w; main_quit Gtk2 };
    $hbbox->pack_start ($button, 0, 0, 0);
    can_default $button 1;

    $button = new Gtk2::Button->new_from_stock('gtk-ok');
    signal_connect $button clicked => sub { $res = 1; hide $w; main_quit Gtk2 };
    $hbbox->pack_start ($button, 0, 0, 0);
    can_default $button 1;
    grab_default $button;

    $res=0;

    show_all $w;
    $sw->set_size_request(
      min(0.75*$sw->get_screen->get_width, $g->size_request->width + 30),
      min(0.6*$sw->get_screen->get_height, $g->size_request->height + 5)
    );
    main Gtk2;
    die $exception_text if $exception_text;

    if ($res == 0) {
      @res = ();
      last;
    }
    @_ = map {&$_} @getvals;
    if ($res == 1) {
      @res = (1,@_);
      last;
    }
#     Gimp->file_load(&Gimp::RUN_INTERACTIVE,"","");
  }
  @getvals = @setvals = @lastvals = ();
  @res;
}

1;
__END__

=head1 NAME

Gimp::UI - Programming interface to libgimpui, plus Gtk widgets for other
parameter types.

=head1 SYNOPSIS

  use Gimp::UI;

=head1 DESCRIPTION

If you use L<Gimp::Fu> in your script, a GUI will be taken care of
for you. However, for an example of implementing your own UI, see
C<examples/example-no-fu>.

=over 4

 $option_menu = new Gimp::UI::ImageMenu;
 $option_menu = new Gimp::UI::LayerMenu;
 $option_menu = new Gimp::UI::ChannelMenu;
 $option_menu = new Gimp::UI::DrawableMenu (constraint_func, active_element, \var);

 $button = new Gimp::UI::PatternSelect;
 $button = new Gimp::UI::BrushSelect;
 $button = new Gimp::UI::GradientSelect;

=back

=head1 AUTHOR

Marc Lehmann <pcg@goof.com>, Seth Burgess <sjburges@gimp.org>

=head1 SEE ALSO

perl(1), L<Gimp>.
