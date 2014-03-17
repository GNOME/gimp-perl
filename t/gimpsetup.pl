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
my $perlserver = "$dir/Perl-Server";
my $s = io("Perl-Server")->all or die "unable to read the Perl-Server: $!";
$s =~ s/^(#!).*?(\n)/$Config{startperl}$2/;
write_plugin($DEBUG, $perlserver, $s);
die "symlink script-fu: $!"
  unless symlink("$plugins/script-fu", "$dir/script-fu");
die "symlink sharpen: $!" unless symlink("$plugins/sharpen", "$dir/sharpen");
die "output gimprc: $!"
  unless io("$dir/gimprc")->print("(plug-in-path \"$dir\")\n");
map { die "mkdir $dir/$_: $!" unless mkdir "$dir/$_"; }
  qw(palettes gradients patterns brushes dynamics);

$ENV{GIMP2_DIRECTORY} = $dir;

ok(1, 'gimp set up');

sub make_executable {
  my $file = shift;
  my $newfile = "$file.pl";
  die "rename $file $newfile: $!\n" unless rename $file, $newfile;
  die "chmod $newfile: $!\n" unless chmod 0700, $newfile;
}

sub write_plugin {
  my ($debug, $file, $text) = @_;
  # trying to be windows- and unix-compat in how to make things executable
  # $file needs to have no extension on it
  my $wrapper = "$file-wrap";
  die "write $file: $!" unless io($file)->print($text);
  if ($DEBUG) {
    die "write $wrapper: $!" unless io($wrapper)->print(<<EOF);
$Config{startperl}
\$ENV{MALLOC_CHECK_} = '3';
\$ENV{G_SLICE} = 'always-malloc';
my \@args = (qw(valgrind --read-var-info=yes perl), '$file', \@ARGV);
open STDOUT, '>', "valgrind-out.\$\$";
open STDERR, '>&', \*STDOUT;
die "failed to exec \@args: \$!\\n" unless exec \@args;
EOF
    make_executable($wrapper);
  } else {
    make_executable($file);
  }
}

1;
