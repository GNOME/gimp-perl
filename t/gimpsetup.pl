# only one "test" result - 'gimp set up' at bottom
# relies on caller having first done:
#    use Test::*; # to make available ok()
#    use Gimp qw(:auto);
#    our $dir;
# if encounters problems, does a die()

use strict;
use Config;
use File::Temp;
use IO::All;

our $DEBUG = 0 unless defined $DEBUG;

our %cfg;
require './config.pl';

my $plugins = $cfg{gimpplugindir} . '/plug-ins';
die "plugins dir: $!" unless -d $plugins;
die "script-fu not executable: $!" unless-x "$plugins/script-fu";

our $dir = File::Temp->newdir($DEBUG ? (CLEANUP => 0) : ());;#
my $perlserver = "$dir/Perl-Server.pl";
my $s = io("Perl-Server")->all or die "unable to read the Perl-Server: $!";
$s =~ s/^(#!).*?(\n)/$Config{startperl}$2/;
die "write Perl-Server: $!" unless io($perlserver)->print($s);
if ($DEBUG) {
  die "chmod Perl-Server: $!" unless chmod(0600, $perlserver);
  my $wrapper = "$dir/perlserver-wrapper";
  die "write $wrapper: $!" unless io($wrapper)->print(<<EOF);
#!/bin/sh
MALLOC_CHECK_=3 G_SLICE=always-malloc valgrind --read-var-info=yes perl $perlserver "\$\@" >../tf 2>&1
EOF
  die "chmod $wrapper: $!" unless chmod 0700, $wrapper;
} else {
  die "chmod Perl-Server: $!" unless chmod(0700, $perlserver);
}
die "symlink script-fu: $!"
  unless symlink("$plugins/script-fu", "$dir/script-fu");
die "symlink sharpen: $!" unless symlink("$plugins/sharpen", "$dir/sharpen");
die "output gimprc: $!"
  unless io("$dir/gimprc")->print("(plug-in-path \"$dir\")\n");
map { die "mkdir $dir/$_: $!" unless mkdir "$dir/$_"; }
  qw(palettes gradients patterns brushes dynamics);

$ENV{GIMP2_DIRECTORY} = $dir;

ok(1, 'gimp set up');

1;
