package Gimp::UI;

use Gimp ('__');
use Gimp::Fu;
use base 'DynaLoader';

no warnings "all";

=head1 NAME

Gimp::UI - interface to libgimpui, and more!

=head1 SYNOPSIS

  use Gimp::UI;

=head1 DESCRIPTION

The libgimpwidgets api has improved considerably in 1.4 (mostly due to
it being based on gobjects), but the resulting widgets still are not
full-featured gobjects, so a lot of manual workaround is neccessary. Most
of the API has been converted.

=over 4

 $option_menu = new Gimp::UI::ImageMenu
 $option_menu = new Gimp::UI::LayerMenu
 $option_menu = new Gimp::UI::ChannelMenu
 $option_menu = new Gimp::UI::DrawableMenu (constraint_func, active_element, \var);
 
 $button = new Gimp::UI::PatternSelect;
 $button = new Gimp::UI::BrushSelect;
 $button = new Gimp::UI::GradientSelec;

=back

=cut

# <sjburges@gimp.org> removed the camel logo from scripts

$VERSION = 2.0;

if (eval { require Gtk2; import Gtk2 (); 1 }) {
   local $/;

   require XSLoader;
   XSLoader::load Gimp::UI $VERSION;

   eval <DATA>;
   die $@ if $@;
   close DATA;
}

1;

# All Gtk-dependent functions are put below
__DATA__
#line 58 "..../Gimp/UI.pm"

# shows the properties of a glib object
#d# just to debug
sub info {
   my ($idx, $obj) = @_;
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
      Gimp->image_list ();
}
sub Gimp::UI::LayerMenu::_items {
  map { my $i = $_; map [[$i,$_],$_,image_name($i)."/".$_->drawable_get_name],$i->get_layers }
      Gimp->image_list ();
}

sub Gimp::UI::ChannelMenu::_items {
  map { my $i = $_; map [[$i,$_],$_,image_name($i)."/".$_->drawable_get_name],$i->get_channels }
      Gimp->image_list ();
}

sub Gimp::UI::DrawableMenu::_items {
  map { my $i = $_; map [[$i,$_],$_,image_name($i)."/".$_->drawable_get_name],($i->get_layers, $i->get_channels) }
      Gimp->image_list ();
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
# virtual methods (must be implemented by child): 
#                  ->get_list 
#                  ->get_title
#                  ->get_pixbuf

use Gimp '__';

use Gtk2::SimpleList;

*new = \&Glib::Object::new;

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

   $datalist = new Gtk2::SimpleList (
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
 
   $button = new Gtk2::Button->new_from_stock('gtk-cancel');
   signal_connect $button clicked => sub {hide $w};
   $hbbox->pack_start($button,0,0,0);
   can_default $button 1;
   show $button;

   my $button = new Gtk2::Button->new_from_stock('gtk-ok');
   signal_connect $button clicked => sub {
     @sel = $datalist->get_selected_indices;
     @row =  @{$datalist->{data}}[$sel[0]];

     $self->set( active => $row[0][0] );   
     hide $w;
     };
   $hbbox->pack_start($button,0,0,0);
   can_default $button 1;
   grab_default $button;
   show $button;

   show $w;
}

package Gimp::UI::PatternSelect;

use Gimp '__';

*new = \&Glib::Object::new;

Glib::Type->register (
   'Gimp::UI::PreviewSelect', __PACKAGE__,
   signals => {},
   properties => [],
);

sub INIT_INSTANCE {
       my ($self) = @_;
       my $lbl = new Gtk2::Label $self->get("active");
       $self->add($lbl);
}

sub get_title { __"Pattern Selection Dialog" }
sub get_list { Gimp->patterns_get_list("") }

sub new_pixbuf {
   my ($name,$w,$h,$bpp,$mask)=Gimp->patterns_get_pattern_data ($_);
   my $has_alpha = ($bpp==2 || $bpp==4);
     
   if ($bpp==1)
     {
        my @graydat = unpack "C*", $mask;
        my @rgbdat;
        
        foreach (@graydat)
          {
             push @rgbdat, $_; push @rgbdat, $_; push @rgbdat, $_;
          }

        $mask = pack "C*", @rgbdat;
     }
   # TODO: Add code/test for handling GRAYA; don't have any GRAYA to test 
   # with currently though.
        
   $pb = Gtk2::Gdk::Pixbuf->new_from_data($mask,'rgb',
                                          $has_alpha?1:0,
                                          8,$w,$h,
                                          $has_alpha?$w*4:$w*3);
   $pb;
}

package Gimp::UI::BrushSelect;

use Gimp '__';

*new = \&Glib::Object::new;

Glib::Type->register (
   'Gimp::UI::PreviewSelect', __PACKAGE__,
   signals => {},
   properties => [],
);

sub INIT_INSTANCE {
       my ($self) = @_;
       my $lbl = new Gtk2::Label $self->get("active");
       $self->add($lbl);
}


sub get_title { __"Brush Selection Dialog" }
sub get_list { Gimp->brushes_list }

package Gimp::UI::GradientSelect;

use Gimp '__';

*new = \&Glib::Object::new;

Glib::Type->register (
   'Gimp::UI::PreviewSelect', __PACKAGE__,
   signals => {},
   properties => [],
);

sub INIT_INSTANCE {
       my ($self) = @_;
       my $lbl = new Gtk2::Label $self->get("active");
       $self->add($lbl);
}

sub get_title { __"Gradient Selection Dialog" }
sub get_list { Gimp->gradients_get_list("") }

sub new_pixbuf {
   use POSIX;
   my ($name,$grad_data)=Gimp->gradients_get_gradient_data ($_,100,0);
   my (@grad_row) = @{$grad_data}; 
   @grad_row = map { $_ = abs(ceil($_*255 - 0.5)) } @grad_row;

# make it 16 tall; somewhat ugly...
   push @grad_row, @grad_row;
   push @grad_row, @grad_row;
   push @grad_row, @grad_row;
   push @grad_row, @grad_row;

   $mask = pack "C*", @grad_row;

   $pb = Gtk2::Gdk::Pixbuf->new_from_data($mask,'rgb',1,8,100,8,100*4);
   $pb;
}


package Gimp::UI;

# Seth Burgess <sjburges@gimp.org> Removed the camel logo from all 
# scripts; doesn't add anything to the interface, and isn't that attractive.

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
   my ($helpwin, $blurb, $help) = @_;
   unless ($$helpwin) {
      $$helpwin = new Gtk2::Dialog;
      $$helpwin->set_title(sprintf __"Help for %s", $Gimp::function);
      $$helpwin->action_area->set_border_width (2);

      my $b = new Gtk2::TextBuffer;
      my $e = new_with_buffer Gtk2::TextView $b;
      $e->set_editable (0);
      #$b->set_word_wrap (1);

      my $cs = new Gtk2::ScrolledWindow undef,undef;
      $cs->set_policy (-automatic, -automatic);
      $cs->add ($e);
      $$helpwin->vbox->add ($cs);
      #$b->set_text ($font, $b->style->fg(-normal),undef,__"BLURB:\n\n$blurb\n\nHELP:\n\n$help");
      $b->set_text (sprintf __"BLURB:\n\n%s\n\nHELP:\n\n%s", $blurb, $help);
      #d#$b->set_usize($font->string_width('M')*80,($font->ascent+$font->descent)*26);

      my $button = Gtk2::Button->new_from_stock('gtk-ok');
      signal_connect $button clicked => sub { hide $$helpwin };
      $$helpwin->action_area->add ($button);
      
      $$helpwin->signal_connect (destroy => sub { undef $$helpwin });

      require Gimp::Pod;
      my $pod = new Gimp::Pod;
      my $text = $pod->format;
      if ($text) {
         $b->insert ($b->get_end_iter, __"\n\nEMBEDDED POD DOCUMENTATION:\n\n");
         $b->insert ($b->get_end_iter, $text);
      }
   }

   $$helpwin->show_all;
}
sub interact($$$$@) {
   my $function = shift;
   my $blurb = shift;
   my $help = shift;
   my @types = @{+shift};
   my (@getvals, @setvals, @lastvals, @defaults);
   my ($button, $box, $bot, $g);
   my $helpwin;
   my $res = 0;
   my @res;

   Gimp::gtk_init;

   my $t = new Gtk2::Tooltips;
   my $w = new Gtk2::Dialog;
   my $accel = new Gtk2::AccelGroup;

   for(;;) {
     #$accel->attach($w);#d#

     set_title $w $Gimp::function;
     $w->action_area->set_border_width(2);
     $w->action_area->set_homogeneous(0);

     my $h = new Gtk2::HBox 0,2;
     $h->add(new Gtk2::Label Gimp::wrap_text($blurb,40));
     $w->vbox->pack_start($h,1,1,0);
     realize $w;

     $g = new Gtk2::Table scalar@types,2,0;
     $g->set(border_width => 4);
     $w->vbox->pack_start($g,1,1,0);

     for(@types) {
        my ($label,$a);
        my ($type,$name,$desc,$default,$extra)=@$_;
        my ($value)=shift;
        
        local *new_PF_STRING = sub {
           my $e = new Gtk2::Entry;
           push @setvals, sub { set_text $e defined $_[0] ? $_[0] : "" };
           #select_region $e 0,1;
           push @getvals, sub { get_text $e };
           $a=$e;
        };

        if ($type == PF_ADJUSTMENT) { # support for scm2perl
           my (@x)=@$default;
           $default=shift @x;
           $type = pop(@x) ? PF_SPINNER : PF_SLIDER;
           $extra=[@x];
        }
        
        $value=$default unless defined $value;
        # massage label text a small bit (works only for english)
        $label="$name: ";
        $label =~ y/_/ /; $label =~ s/^(\w)/\U$1/g;
        
        if ($type == PF_INT8		# perl just maps
        || $type == PF_INT16		# all this crap
        || $type == PF_INT32		# into the scalar
        || $type == PF_FLOAT		# domain.
        || $type == PF_STRING) {	# I love it
           &new_PF_STRING;
           
        } elsif ($type == PF_FONT) {
           my $fs = new Gtk2::FontSelectionDialog sprintf __"Font Selection Dialog (%s)", $desc;
           my $def = __"Helvetica 34";
           my $val;
           
           my $l = new Gtk2::Label "!error!";
           my $setval = sub {
              $val = $_[0];
              unless (defined $val && $fs->set_font_name ($val)) {
                 warn sprintf __"Illegal default font description for $function: %s\n", $val
                    if defined $val;
                 $val = $def;
                 $fs->set_font_name ($val);
              }
              
              $l->set (label => $val);
           };
           
           $fs->ok_button->signal_connect (clicked => sub {$setval->($fs->get_font_name); $fs->hide});
           $fs->cancel_button->signal_connect (clicked => sub {$fs->hide});
           
           push @setvals, $setval;
           push @getvals, sub { $val };
           
           $a = new Gtk2::Button;
           $a->add ($l);
           $a->signal_connect (clicked => sub { show $fs });
           
        } elsif ($type == PF_SPINNER) {
           my $adj = _new_adjustment ($value, $extra);
           $a = new Gtk2::SpinButton $adj, 1, 0;
           $a->set_digits (_find_digits $adj);
           push @setvals, sub { $adj->set_value($_[0]) };
           push @getvals, sub { $adj->get_value };
           
        } elsif ($type == PF_SLIDER) {
           my $adj = _new_adjustment ($value, $extra);
           $a = new Gtk2::HScale $adj;
           $a->set_digits (_find_digits $adj);
           push @setvals, sub { $adj->set_value($_[0]) };
           push @getvals, sub { $adj->get_value };
           
        } elsif ($type == PF_COLOR) {
           $a = new Gtk2::HBox 0,5;

           $default = [0.8,0.6,0.1] unless defined $default;

           my $b = new Gimp::UI::ColorButton $name, 90, 18, $default, 'small-checks';
     
           $a->pack_start ($b, 1, 1, 0);

           push @setvals, sub { $b->set_color (defined $_[0] ? Gimp::canonicalize_color $_[0] : [0.8,0.6,0.1]) };
           push @getvals, sub { $b->get_color };
           set_tip $t $b,$desc;
           
           my $c = new Gtk2::Button __"FG";
           signal_connect $c clicked => sub {
             $b->set_color (Gimp::Palette->get_foreground);
           };
           set_tip $t $c,__"get current foreground colour from the gimp";
           $a->pack_start ($c,1,1,0);
           
           my $d = new Gtk2::Button __"BG";
           signal_connect $d clicked => sub {
             $b->set_color (Gimp::Palette->get_background);
           };
           set_tip $t $d,__"get current background colour from the gimp";
           $a->pack_start ($d,1,1,0);
           
        } elsif ($type == PF_TOGGLE) {
           $a = new Gtk2::CheckButton $desc;

           push @setvals, sub{ $a->set (active => $_[0] ? 1 : 0)};
           push @getvals, sub{ $a->get("active") };
           
        } elsif ($type == PF_RADIO) {
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
           push @setvals, $prev_sub;
           push @getvals, sub { $r };
           
        } elsif ($type == PF_IMAGE) {
           my $res;
           $a=new Gtk2::HBox (0,5);
           my $b=new Gtk2::OptionMenu;
           $b->set_menu(new Gimp::UI::ImageMenu(sub {1},-1,\$res));
           $a->pack_start ($b,1,1,0);
           push @setvals, sub { };
           push @getvals, sub { $res };
           set_tip $t $b,$desc;
           
#           my $c = new Gtk2::Button "Load";
#           signal_connect $c "clicked", sub {$res = 2; main_quit Gtk2};
##           $g->attach($c,1,2,$res,$res+1,{},{},4,2);
#           $a->pack_start ($c,1,1,0);
#           set_tip $t $c,"Load an image into the Gimp";
           
        } elsif ($type == PF_LAYER) {
           my $res;
           $a=new Gtk2::OptionMenu;
           $a->set_menu(new Gimp::UI::LayerMenu(sub {1},-1,\$res));
           push @setvals, sub { };
           push @getvals, sub { $res };
           
        } elsif ($type == PF_CHANNEL) {
           my $res;
           $a=new Gtk2::OptionMenu;
           $a->set_menu(new Gimp::UI::ChannelMenu(sub {1},-1,\$res));
           push @setvals, sub { };
           push @getvals, sub { $res };
           
        } elsif ($type == PF_DRAWABLE) {
           my $res=13;
           $a=new Gtk2::OptionMenu;
           $a->set_menu(new Gimp::UI::DrawableMenu(sub {1},-1,\$res));
           push @setvals, sub {};
           push @getvals, sub { $res };
           
        } elsif ($type == PF_PATTERN) {
           $a=new Gimp::UI::PatternSelect;
           push @setvals, sub { $a->set('active', 
	       defined $value ? $value : (Gimp->patterns_get_pattern)[0]) };
           push @getvals, sub { $a->get('active') };
           
        } elsif ($type == PF_BRUSH) {
           $a=new Gimp::UI::BrushSelect -active =>  defined $value ? $value : (Gimp->brushes_get_brush)[0];
           push @setvals, sub{ $a->set('active',$_[0]) };
           push @getvals, sub{ $a->get('active') };
           
        } elsif ($type == PF_GRADIENT) {
           $a=new Gimp::UI::GradientSelect;
           push @setvals, sub { $a->set('active',
	       defined $value ? $value : (Gimp->gradients_get_list(""))[0]) };
           push @getvals, sub { $a->get('active') };
           
        } elsif ($type == PF_CUSTOM) {
           my (@widget)=&$extra;
           $a=&{$widget[0]};
           push @setvals, $widget[1];
           push @getvals, $widget[2];
           
        } elsif ($type == PF_FILE) {
           &new_PF_STRING;
           my $s = $a;
           $a = new Gtk2::HBox 0,5;
           $a->add ($s);
           my $b = new Gtk2::Button __"Browse";
           $a->add ($b);
           my $f = new Gtk2::FileSelection $desc;
           $b->signal_connect (clicked => sub { $f->set_filename ($s->get_text); $f->show_all });
           $f->ok_button    ->signal_connect (clicked => sub { $f->hide; $s->set_text ($f->get_filename) });
           $f->cancel_button->signal_connect (clicked => sub { $f->hide });
           
        } elsif ($type == PF_TEXT) {
           $a = new Gtk2::Frame;
           my $h = new Gtk2::VBox 0,5;
           $a->add($h);
           my $b = new Gtk2::TextBuffer;
           my $e = new_with_buffer Gtk2::TextView $b;
           my %e;
           %e = $$extra if ref $extra eq "HASH";

           my $sv = sub { 
              $b->set_text ($_[0]);
           };
           my $gv = sub {
              $b->get_text ($b->get_start_iter, $b->get_end_iter, 0);
           };

           $h->add ($e);
           $e->set_editable (1);

           my $buttons = new Gtk2::HBox 1,5;
           $h->add ($buttons);

           my $load = new Gtk2::Button __"Load"; $buttons->add ($load);
           my $save = new Gtk2::Button __"Save"; $buttons->add ($save);
           my $edit = new Gtk2::Button __"Edit"; $buttons->add ($edit);

           $edit->signal_connect (clicked => sub {
              my $editor = $ENV{EDITOR} || "vi";
              my $tmp = Gimp->temp_name ("txt");
              open TMP,">:utf8", $tmp or die __"FATAL: unable to create $tmp: $!\n";
              print TMP &$gv;
              close TMP;
              $w->hide;
              main_iteration Gtk2;
              system 'xterm', '-T', "$editor: $name", '-e', $editor, $tmp;
              $w->show;
              if (open TMP,"<:utf8", $tmp) {
                 local $/; &$sv(scalar<TMP>); close TMP;
              } else {
                 Gimp->message(sprintf __"unable to read temporary file '%s': %s", $tmp, "$!");
              }
           });

           my $filename = ($e{prefix} || eval { Gimp->directory } || ".") . "/";
           
           my $f = new Gtk2::FileSelection sprintf __"Fileselector for %s", $name;
           $f->set_filename ($filename);
           $f->cancel_button->signal_connect (clicked => sub { $f->hide });
           my $lf = sub {
              $f->hide;
              my $fn = $f->get_filename;
              if (open TMP,"<:utf8", $fn) {
                 local $/; &$sv(scalar<TMP>);
                 close TMP;
              } else {
                 Gimp->message(sprintf __"unable to read '%s': %s", $fn, "$!");
              }
           };
           my $sf = sub {
              $f->hide;
              my $fn = $f->get_filename;
              if (open TMP, ">:utf8", $fn) {
                 print TMP &$gv;
                 close TMP;
              } else {
                 Gimp->message(sprintf __"unable to create '%s': %s", $fn, "$!");
              }
           };
           my $lshandle;
           $load->signal_connect (clicked => sub {
              $f->set_title(sprintf __"Load %s", $name);
              $f->ok_button->signal_disconnect($lshandle) if $lshandle;
              $lshandle=$f->ok_button->signal_connect (clicked => $lf);
              $f->show_all;
           });
           $save->signal_connect (clicked => sub {
              $f->set_title(sprintf __"Save %s", $name);
              $f->ok_button->signal_disconnect($lshandle) if $lshandle;
              $lshandle=$f->ok_button->signal_connect (clicked => $sf);
              $f->show_all;
           });

           push @setvals,$sv;
           push @getvals,$gv;

        } else {
           $label = sprintf __"Unsupported argumenttype %s", $type;
           push @setvals, sub {};
           push @getvals, sub { $value };
        }
        
        push @lastvals, $value;
        push @defaults, $default;
        $setvals[-1]->($value);
        
        $label = new Gtk2::Label $label;
        $label->set_alignment (0,0.5);
        $g->attach ($label, 0, 1, $res, $res+1, [], [], 4, 2);
        $a && do {
           set_tip $t $a,$desc;
           $g->attach ($a, 1, 2, $res, $res+1, ["expand","fill"], ["expand","fill"], 4, 2);
        };
        $res++;
     }
     
     my $v = new Gtk2::HBox 0,4;
     $w->vbox->pack_start ($v, 0, 0, 4);

     my $hbbox = new Gtk2::HButtonBox;
     $hbbox->set_spacing (4);
     $v->pack_end ($hbbox, 0, 0, 2);
     
     $button = new Gtk2::Button->new_from_stock('gimp-reset');
     signal_connect $button clicked => sub {
       for my $i (0..$#defaults) {
         $setvals[$i]->($defaults[$i]);
       }
     };
     $hbbox->pack_start ($button, 0, 0, 0);
     set_tip $t $button,__"Reset all values to their default";
     
     $button = new Gtk2::Button __"Previous";
     signal_connect $button clicked => sub {
       for my $i (0..$#lastvals) {
         $setvals[$i]->($lastvals[$i]);
       }
     };
     $hbbox->pack_start($button,0,0,0);
     set_tip $t $button,__"Restore values to the previous ones";
     
     signal_connect $w destroy => sub { main_quit Gtk2 };

     $hbbox = new Gtk2::HButtonBox;
     $hbbox->set_spacing (2);
     $w->action_area->pack_start ($hbbox, 0, 0, 0);
     show $hbbox;

     $button = new Gtk2::Button->new_from_stock('gtk-help');
     $hbbox->pack_start ($button, 0, 0, 0);
     signal_connect $button clicked => sub { help_window ($helpwin, $blurb, $help) };
     can_default $button 1;
     
     $hbbox = new Gtk2::HButtonBox;
     $hbbox->set_spacing (2);
     $w->action_area->pack_end ($hbbox, 0, 0, 0);
     show $hbbox;

     
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
     main Gtk2;
     
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
   @getvals=
   @setvals=
   @lastvals=();
   @res;
}

1;

=head1 AUTHOR

Marc Lehmann <pcg@goof.com>.

=head1 SEE ALSO

perl(1), L<Gimp>.

=cut
