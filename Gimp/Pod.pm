package Gimp::Pod;

use Config;
use strict;
use FindBin qw($RealBin $RealScript);

our $VERSION = 2.3001;

warn "$$-Loading ".__PACKAGE__ if $Gimp::verbose;

{
package Gimp::Pod::Parser;
use base 'Pod::Text';
sub output { shift->{gpp_text} .= join '', @_; }
sub get_text { $_[0]->{gpp_text} }
}

sub new {
   return unless -f "$RealBin/$RealScript";
   bless { path => "$RealBin/$RealScript", }, $_[0];
}

sub _cache {
   my $self = shift;
   return $self->{doc} if $self->{doc};
   my $parser = Gimp::Pod::Parser->new;
   $parser->parse_from_file($self->{path});
   $self->{doc} = $parser->get_text;
}

sub format { $_[0]->_cache; }

sub sections { $_[0]->_cache =~ /^\S.*$/mg; }

sub section {
   my $self = shift;
   warn __PACKAGE__."::section(@_)" if $Gimp::verbose;
   return unless defined(my $doc = $self->_cache);
   ($doc) = $doc =~ /^$_[0]\n(.*?)(?:^[A-Z]|\Z)/sm;
   if ($doc) {
      $doc =~ y/\r//d;
      $doc =~ s/^\s*\n//;
      $doc =~ s/[\s]+$/\n/;
      $doc =~ s/^    //mg;
      chomp $doc;
   }
   warn __PACKAGE__."::section returning '$doc'" if $Gimp::verbose;
   $doc;
}

1;
__END__

=head1 NAME

Gimp::Pod - Evaluate pod documentation embedded in scripts.

=head1 SYNOPSIS

  use Gimp::Pod;
  my $pod = Gimp::Pod->new;
  my $text = $pod->format;
  my $synopsis = $pod->section('SYNOPSIS');
  my @sections = $pod->sections;

=head1 DESCRIPTION

C<Gimp::Pod> can be used to find and parse embedded pod documentation in
Gimp-Perl scripts, returning formatted text.

=head1 METHODS

=over 4

=item new

Return a new Gimp::Pod object representing the current script or undef, if
an error occured.

=item format

Return the embedded pod documentation in text format, or undef if no
documentation can be found.

=item section($header)

Return the section with the header C<$header>, or undef if not
found. There is no trailing newline on the returned string.

=item sections

Returns a list of paragraph titles found in the pod.

=back

=head1 AUTHOR

Marc Lehmann <pcg@goof.com>.
Rewritten to eliminate external executables by Ed J.

=head1 SEE ALSO

perl(1), L<Gimp>
