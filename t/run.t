use Config;
use vars qw($EXTENSIVE_TESTS $GIMPTOOL);

# the most complicated thing is to set up a working gimp environment. its
# difficult at best...

BEGIN {
  $|=1;

  if ($ENV{DISPLAY}) {
     print "1..26\n";
     $count=0;
     $Gimp::host = "spawn/";
  } else {
     print "1..0\n";
     exit;
  }
}

sub ok($;$) {
   print((@_==1 ? shift : $_[0] eq &{$_[1]}) ?
         "ok " : "not ok ", ++$count, "\n");
}

sub skip($$;$) {
   shift() ? print "ok ",++$count," # skip\n" : &ok;
}

END {
   system("rm","-rf",$dir);#d##FIXME#
}

use Cwd;

$dir=cwd."/test-dir";

do './config.pl';
# Test 1
ok(1);

$n=!$EXTENSIVE_TESTS;

# Find the installation path that gimptool would choose
# should look like:
#  /usr/bin/install -c /bin/sh /usr/local/lib/gimp/2.0/plug-ins/sh

# Test 2
skip($n,1,sub {($plugins = `$GIMPTOOL -n --install-admin-bin /bin/sh`) =~ 
                            s|^.*/bin/sh\s+(.+)/sh\r?\n?$|$1|});

# Test 3 (Make sure plug-in directory exists)
skip($n,1,sub {-d $plugins});

# Test 4 (Make sure script-fu exists so we can symlink with it)
skip($n,1,sub {-x "$plugins/script-fu"});

# Test 5 (Check that environment is setup and can use the Gimp module)
use Gimp;
ok(1);

# Test 6 (check that constants are brought in)
ok(RGBA_IMAGE || RGB_IMAGE);

# Test 7
ok(RGB_IMAGE ? 1 : 1); #  check for correct prototype

sub tests {
   my($i,$l);
 # Test 15 (OO Syntax for new image) 
   skip($n,1,sub{0 != ($i=new Image(10,10,RGB))});
 # Test 16 (check its return value)
   skip($n,1,sub {!!ref $i});
 # Test 17 (Different OO syntax for creating a layer)
   skip($n,1,sub{0 != ($l=$i->layer_new(10,10,RGBA_IMAGE,"new layer",100,VALUE_MODE))});
 # Test 18 (check its return value)
   skip($n,1,sub {!!ref $l});
   
 # Test 19 (Yet another OO syntax)
   skip($n,1,sub{Gimp->image_add_layer($l,0) || 1});
 # Test 20 (check that the layer is named what we expect)
   skip($n,"new layer",sub{$l->get_name()});
 # Test 21 (some painting to test variable length arrays, default value) 
   skip($n,1,sub{$l->paintbrush(50,[1,1,2,2,5,3,7,4,2,8],PAINT_CONSTANT,0) || 1});
 # Test 22 (some more to test without default value)
   skip($n,1,sub{$l->paintbrush(30,4,[5,5,8,1],PAINT_CONSTANT,0) || 1});
  
 # Test 23 (call external plugin through Plugin->)
#   skip($n,1,sub{Plugin->sharpen(RUN_NONINTERACTIVE,$i,$l,10) || 1});
# BROKEN: skip for now
   ok(1);
 # Test 24 (call with maximum fu magic)
   skip($n,1,sub{$l->sharpen(10) || 1});
 # Test 25 (call with little Fu magic, using default RUN_NONINTERACTIVE)
   skip($n,1,sub{Gimp->plug_in_sharpen($i,$l,10) || 1});
 # Test 26 (remove image)
   skip($n,1,sub{$i->delete || 1});
}

system("rm","-rf",$dir); #d#FIXME
# Test 8
ok(1,sub {mkdir $dir,0700});

# copy the Perl-Server
{
   local(*X,*Y,$/);
   open X,"<Perl-Server" or die "unable to read the Perl-Server";
   my $s = <X>;
   open Y,">$dir/Perl-Server.pl" or die "unable to write the Perl-Server";
   print Y $Config{startperl},"\n",$s,<X>;
   # Test 9
   ok(1);
}
# Test 10
ok(1,sub { chmod 0700,"$dir/Perl-Server.pl" });

# Test 11 (symlink script-fu)
skip($n,1,sub {symlink "$plugins/script-fu","$dir/script-fu"});
# Test 12 (symlink sharpen)
skip($n,1,sub {symlink "$plugins/sharpen","$dir/sharpen"});

# Test 13 (output gimprc)
ok (
  open RC,">$dir/gimprc" and
  print RC "(show-tips no)\n" and
#  print RC "(gimp_data_dir \"\")\n" and
  print RC "(script-fu-path \"\")\n" and
  print RC "(plug-in-path \"$dir\")\n" and
  close RC
);

$ENV{GIMP2_DIRECTORY}=$dir;
$ENV{PERL5LIB}=cwd."/blib/lib:".cwd."/blib/arch";

if(!$n) {
# Test 14
   skip($n,1);
   Gimp::init;
   tests;
} else {
# Test 14
   skip($n,0);
   tests;
}







