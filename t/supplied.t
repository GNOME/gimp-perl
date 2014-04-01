use strict;
use Test::More;
our ($dir, $DEBUG);
BEGIN {
#  $Gimp::verbose = 1;
  $DEBUG = 0;
  require 't/gimpsetup.pl';
  # most minimal and elegant would be to symlink sandbox gimp-dir's
  # plug-ins to our blib/plugins dir, but not portable to windows
  my $blibdir = 'blib/plugins';
  my @plugins = grep { !/Perl-Server/ } glob "$blibdir/*";
  map {
    warn "inst $_\n" if $Gimp::verbose;
    write_plugin($DEBUG, $_, io($_)->all);
  } @plugins;
  map { symlink_sysplugin($_) }
    qw(
      noise-rgb noise-solid blur-gauss grid pixelize blur-motion displace
      bump-map checkerboard edge file-png unsharp-mask crop-auto
    );
}
use Gimp qw(:auto), "net_init=spawn/";
#Gimp::set_trace(TRACE_ALL);

sub newimage {
  my $numlayers = shift;
  my $i = Gimp::Image->new(200,200,RGB);
  for my $layernum (1..$numlayers) {
    my $l0 = $i->layer_new(200,200,RGBA_IMAGE,"layer $layernum",100,VALUE_MODE);
    $i->insert_layer($l0,0,0);
  }
  $i;
}

use constant {
  REQ_NONE  => 0,
  REQ_ALPHA => 1 << 0,
  REQ_SEL   => 1 << 1,
  REQ_GUIDE => 1 << 2,
  REQ_DIR   => 1 << 3,
  REQ_LAYER => 1 << 4,
};

my $color1 = [0,0,1.0];
my $color2 = [0.4,0,1.0];
my $black = [0,0,0];
my $white  = [1,1,1];
my $gradient1 = "Burning Paper";
my $width     = 10;
my $height    = 10;

my @testbench = (
["add_glow"               , 2, REQ_ALPHA, [$color1, 5] ],
["animate_cells"          , 3, REQ_ALPHA, [0] ],
["auto_red_eye"           , 1, REQ_NONE , [] ],
["blowinout"              , 1, REQ_NONE , [ 30, 8, "30", 0, 0] ],
["blur_2x2"               , 1, REQ_NONE , [] ],
["brushed_metal"          , 1, REQ_NONE , [40,120,1,$gradient1] ],
["burst"                  , 1, REQ_NONE , [0,0,14,30,50,80,140] ],
["center_guide"           , 1, REQ_NONE , [0] ],
["center_layer"           , 2, REQ_ALPHA, [] ],
["contrast_enhance_2x2"   , 1, REQ_NONE , [] ],
["do_bricks"              , 0, REQ_NONE , ["Leather","unused yet","",[0.5,0.5,0.5],1,8,16,256,256,0] ],
["dots"                   , 1, REQ_NONE , [8,$color1,80,20,16,0,0] ],
["dust"                   , 1, REQ_NONE , [0.0005,0,50] ],
["edge_detect_2x2"        , 1, REQ_NONE , [] ],
["glowing_steel"          , 0, REQ_NONE , ["GET LOST","Bitstream Charter Bold 72",100,$color1,$black,4,0,0] ],
["golden_mean"            , 0, REQ_NONE , [233, 0] ],
["guide_grid"             , 1, REQ_NONE , [24,14,0,0,0] ],
["guide_to_selection"     , 1, REQ_GUIDE, [CHANNEL_OP_REPLACE,0,0] ],
["highlight_edges"        , 1, REQ_ALPHA, [ 10] ],
["inner_bevel"            , 0, REQ_NONE , ["URW Bookman L, Bold 80","INNERBEVEL",$color1,$color2,132,30,7,2] ],
["layer_apply"            , 1, REQ_NONE , ['$d->gauss_rle($P*100+1,1,1)',""] ],
["layer_reorder"          , 3, REQ_ALPHA, [1,""] ],
["make_bevel_logos"       , 1, REQ_ALPHA, [$white,$color1,$color2,45,4,0] ],
["make_trans_logos"       , 1, REQ_ALPHA, [0,$gradient1,$color1] ],
["map_to_gradient"        , 1, REQ_NONE , [$gradient1] ],
["mirror_split"           , 1, REQ_NONE , [0] ],
["perlotine"              , 1, REQ_GUIDE|REQ_DIR, ["foo.html","t","png",0,"",1,0] ],
["pixelgen"               , 0, REQ_NONE , [$width,$height,RGB_IMAGE,'($x*$y*0.01)->slice("*$bpp")'] ],
["pixelmap"               , 1, REQ_NONE , ['($x*$y*0.01)->slice("*$bpp")'] ],
["prep4gif"               , 2, REQ_ALPHA, [64,1,0,1,255] ],
["random_art_1"           , 0, REQ_NONE , [$width,$height,20,10,1,30,0] ],
["random_blends"          , 1, REQ_NONE , [7] ],
["red_eye"                , 1, REQ_NONE , [0] ],
["repdup"                 , 1, REQ_SEL  , [3,50,50] ],
["round_sel"              , 1, REQ_SEL  , [16] ],
["scratches"              , 1, REQ_NONE , [30,70,0.3,15,10] ],
["selective_sharpen"      , 1, REQ_NONE , [5.0,1.0,20] ],
["seth_spin"              , 2, REQ_LAYER, [16,$color1,40,1,1] ],
["stamps"                 , 0, REQ_NONE , [90,$white,$color1,10,5] ],
# ["tex_string_to_float"    , 1, REQ_NONE , ["","I can write \\\\TeX",72,6,4] ],
# ["view3d"                 , 1, REQ_NONE , [0,1,1] ],
["webify"                 , 1, REQ_NONE , [1,1,$white,3,32,1] ],
["windify"                , 1, REQ_NONE , [120,80,30,1] ],
["xach_blocks"            , 1, REQ_NONE , [10,40] ],
["xach_shadows"           , 1, REQ_NONE , [10] ],
["xachvision"             , 1, REQ_NONE , [$color1,25] ],
["yinyang"                , 0, REQ_NONE , [$width,$height,1,0,"","",1] ],
);

for my $test (@testbench) {
  my ($name, $numlays, $flags, $params) = @$test;
  my @actualparams = @$params;
  my $tempdir;
  if ($flags & REQ_DIR) {
    $tempdir = File::Temp->newdir($DEBUG ? (CLEANUP => 0) : ());
    unshift @actualparams, $tempdir.'';
  }
  if ($numlays > 0) {
    my $img = newimage($numlays);
    my $drw = $img->get_active_layer;
    unshift @actualparams, ($img->get_layers)[1] if $flags & REQ_LAYER;
    unshift @actualparams, $img, $drw;
    Gimp::Display->new($img);
    if ($flags & REQ_ALPHA) {
      $drw->add_alpha;
      $img->select_rectangle(CHANNEL_OP_REPLACE,0.1*$height,0.1*$width,0.8*$height,0.8*$width);
      $img->selection_invert;
      $drw->edit_cut;
      $img->selection_none;
    }
    $img->select_rectangle(
      CHANNEL_OP_REPLACE,0.2*$height,0.2*$width,0.6*$height,0.6*$width
    ) if $flags & REQ_SEL;
    map {
      $img->add_hguide($width * $_); $img->add_vguide($height * $_);
    } (0.3, 0.6, 0.9) if $flags & REQ_GUIDE;
  }
  warn "Running $name\n" if $Gimp::verbose;
#use Data::Dumper;warn Dumper(Gimp->procedural_db_proc_info("perl_fu_$name"));
  my $img = eval { Gimp::Plugin->$name(@actualparams); };
  is($@, '', "plugin $name");
  $img->delete if defined $img;
}

Gimp::Net::server_quit;
Gimp::Net::server_wait;

done_testing;
