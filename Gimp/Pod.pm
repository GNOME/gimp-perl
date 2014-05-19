package Gimp::Pod;

use Config;
use Carp qw(croak carp);
use strict;
use FindBin qw($RealBin $RealScript);
use File::Basename;
use base 'Exporter';
use Pod::Simple::SimpleTree;

our @EXPORT = qw(fixup_args make_arg_line);
our $VERSION = 2.3003;

warn "$$-Loading ".__PACKAGE__ if $Gimp::verbose;

sub __ ($) { goto &Gimp::__ }

sub new {
   return unless -f "$RealBin/$RealScript";
   bless { path => "$RealBin/$RealScript", }, $_[0];
}

sub _cache {
   my $self = shift;
   return $self->{doc} if $self->{doc};
   $self->{doc} = Pod::Simple::SimpleTree->new->parse_file($self->{path})->root;
}

sub sections {
  map $_->[2], grep { ref eq 'ARRAY' and $_->[0] eq 'head1' } @{$_[0]->_cache};
}

sub _flatten_para {
   my $para = shift;
   join '', map { ref($_) ? _flatten_para($_) : $_ } @{$para}[2..$#{$para}];
}

sub section {
   my $self = shift;
   warn __PACKAGE__."::section(@_)" if $Gimp::verbose >= 2;
   return unless defined(my $doc = $self->_cache);
   my $i = 2; # skip 'Document' and initial attrs
   $i++ until
      $i >= @$doc or ($doc->[$i]->[0] eq 'head1' and $doc->[$i]->[2] eq $_[0]);
   return if $i >= @$doc;
   my $i2 = ++$i;
   $i2++ until $i2 >= @$doc or $doc->[$i2]->[0] =~ /^head/;
   $i2--;
   my $text = join "\n\n", map { _flatten_para($_) } @{$doc}[$i..$i2];
   warn __PACKAGE__."::section returning '$text'" if $Gimp::verbose >= 2;
   $text;
}

my %IND2SECT = (
   2 => 'DESCRIPTION', 3 => 'AUTHOR', 4 => 'LICENSE',
   5 => 'DATE', 6 => 'SYNOPSIS', 7 => 'IMAGE TYPES',
);
sub _getpod { $_[0] ||= new __PACKAGE__; $_[0]->section($_[1]); }
sub _patchup_eval ($$) {
   my ($label, $text) = @_;
   my @result = eval "package main;\n#line 0 \"$0 $label\"\n" . ($text // '');
   die $@ if $@;
   @result;
}
sub fixup_args {
   my @p = @_;
   my $pod;
   splice @p, 9, 0, [ _patchup_eval 'RETURN VALUES', _getpod($pod, 'RETURN VALUES') ] if @p == 10;
   croak sprintf
      __"register given wrong number of arguments: wanted 11, got %d(%s)",
      scalar(@p),
      join(' ', @p),
      unless @p == 11;
   @p[0,1] = (_getpod($pod,'NAME')//'') =~ /(.*?)\s*-\s*(.*)/ unless $p[0] or $p[1];
   ($p[0]) = File::Basename::fileparse($RealScript, qr/\.[^.]*/) unless $p[0];
   while (my ($k, $v) = each %IND2SECT) { $p[$k] ||= _getpod($pod, $v); }
   $p[8] ||= [ _patchup_eval 'PARAMETERS', _getpod($pod, 'PARAMETERS') ];
   for my $i (0..6, 10) {
      croak "$0: Need arg $i (or POD ".($IND2SECT{$i}//'')." section)" unless $p[$i]
   }
   for my $val (@{$p[8]}, @{$p[9]}) {
      croak __"$p[0]: argument/return '$val->[1]' has illegal type '$val->[0]'"
	unless int($val->[0]) eq $val->[0];
      carp __"$p[0]: argument name '$val->[1]' contains illegal characters, only 0-9, a-z and _ allowed"
	unless $val->[1]=~/^[0-9a-z_]+$/;
   }
   @p;
}

sub make_arg_line {
   my @p = @_;
   return '' unless @{$p[8]};
   die "$0: parameter had empty string\n" if grep { !length $_->[1] } @{$p[8]};
   my $myline = 'my ('.join(',', map { '$'.$_->[1] } @{$p[8]}).') = @_;';
   warn __PACKAGE__."::make_arg_line: $myline" if $Gimp::verbose >= 2;
   $myline;
}

1;
__END__

=head1 NAME

Gimp::Pod - Evaluate pod documentation embedded in scripts.

=head1 SYNOPSIS

  use Gimp::Pod;
  my $pod = Gimp::Pod->new;
  my $synopsis = $pod->section('SYNOPSIS');
  my @sections = $pod->sections;

  my @args = fixup_args(@register_args);

=head1 DESCRIPTION

C<Gimp::Pod> can be used to find and parse embedded pod documentation in
Gimp-Perl scripts, returning formatted text.

=head1 FUNCTIONS

=over 4

=item fixup_args

C<fixup_args> is exported by default. It takes a list of arguments,
and for all the scalar arguments, will (if they are false) extract them
from various parts of the calling script's POD documentation, and then
return the fixed-up list:

=over 4

=item $function

Defaults to the NAME section of the POD, the part B<before> the first
C<->. Falls back to the script's filename.

=item $blurb

Defaults to the NAME section of the POD, the part B<after> the first C<->.

=item $help

Defaults to the DESCRIPTION section of the POD.

=item $author

Defaults to the AUTHOR section of the POD.

=item $copyright

Defaults to the LICENSE section of the POD.

=item $date

Defaults to the DATE section of the POD.

=item $menupath

Defaults to the SYNOPSIS section of the POD.

=item $imagetypes

Defaults to the "IMAGE TYPES" section of the POD.

=item $params

Defaults to the "PARAMETERS" section of the POD, passed to C<eval>, e.g.:

  =head PARAMETERS

    [ PF_COLOR, 'color', 'Colour', 'black' ],
    [ PF_FONT, 'font', 'Font', 'Arial' ],

You don't B<have> to indent it so that POD treats it as verbatim, but
it will be more readable in any POD viewer if you do.

=item $results

Defaults to the "RETURN VALUES" section of the POD, passed to C<eval>.
Not providing the relevant POD section is perfectly valid, so long as
you intend to return no values.

=item $other

Untouched. Must be supplied - will normally be the code reference.

=back

=item make_arg_line

C<make_arg_line> is exported by default. It is used by source filters
in L<Gimp::Fu> and L<Gimp::Extension> to generate the line inserted at
the start of functions passed to C<podregister>. It takes as arguments,
the output of C<fixup_args>, and returns the text to be inserted (possibly
an empty string).

=back

=head1 METHODS

=over 4

=item new

Return a new Gimp::Pod object representing the current script or undef, if
an error occured.

=item section($header)

Return the section with the header C<$header>, or undef if not
found. There is no trailing newline on the returned string.

=item sections

Returns a list of section titles found in the pod.

=back

=head1 AUTHOR

Marc Lehmann <pcg@goof.com>.
Rewritten to eliminate external executables by Ed J.

=head1 SEE ALSO

perl(1), L<Gimp>
