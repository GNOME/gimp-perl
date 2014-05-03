package Gimp::Extension;

use strict;
use Carp qw(croak carp);
use base 'Exporter';
use Filter::Simple;
use Gimp::Pod;

# manual import
sub __ ($) { goto &Gimp::__ }
sub main { goto &Gimp::main; }

my $podreg_re = qr/(\bpodregister\s*{)/;
FILTER {
   return unless /$podreg_re/;
   my $myline = make_arg_line(fixup_args(('') x 9, 1));
   s/$podreg_re/$1\n$myline/;
   warn __PACKAGE__."::FILTER: found: '$1'" if $Gimp::verbose;
};

our @EXPORT = qw(podregister main);

my @register_params;
Gimp::on_query {
   Gimp->install_procedure(@register_params);
};

sub podregister (&) {
   no strict 'refs';
   my ($function, $blurb, $help, $author, $copyright, $date, $menupath,
       $imagetypes, $params, $results, $code) = fixup_args(('')x9, @_);
   for my $p (@$params,@$results) {
      next unless ref $p;
      croak __"$function: argument/return value '$p->[1]' has illegal type '$p->[0]'"
	unless int($p->[0]) eq $p->[0];
      carp(__"$function: argument name '$p->[1]' contains illegal characters, only 0-9, a-z and _ allowed")
	unless $p->[1]=~/^[0-9a-z_]+$/;
   }
   Gimp::register_callback $function => sub {
      warn "$$-Gimp::Extension sub: $function(@_)" if $Gimp::verbose;
      Gimp::gtk_init;
      Gimp->extension_ack;
      Gimp->extension_enable;
      goto &$code;
   };
   @register_params = (
      $function, $blurb, $help, $author, $copyright, $date, $menupath,
      $imagetypes, Gimp::EXTENSION, $params, $results
   );
}

1;
__END__

=head1 NAME

Gimp::Extension - Easy framework for Gimp-Perl extensions

=head1 SYNOPSIS

  use Gimp;
  use Gimp::Extension;
  podregister {
    # your code
  };
  exit main;
  __END__
  =head1 NAME

  function_name - Short description of the function

  =head1 SYNOPSIS

  <Image>/Filters/Menu/Location...

  =head1 DESCRIPTION

  Longer description of the function...

=head1 DESCRIPTION

This module provides all the infrastructure you need to write Gimp-Perl
extensions.

Your main interface for using C<Gimp::Extension> is the C<podregister>
function. This works in exactly the same way as L<Gimp::Fu/PODREGISTER>,
including declaring/receiving your variables for you.

It is different in that parameters and return values are not added
for you, and your function name will not be changed but passed to GIMP
verbatim.

Another difference is that the C<run_mode> is passed on to your function,
rather than being stripped off as with Gimp::Fu.

Finally, before control is passed to your function, these procedures are called:

  Gimp::gtk_init; # sets up Gtk2, ready for event loop
  Gimp->extension_ack; # GIMP hangs till this is called
  Gimp->extension_enable; # adds an event handler in Glib mainloop for
			  # GIMP messages

Your function will then either proceed as if it were a plugin, or call
the Glib/Gtk2 mainloop:

  Gtk2->main;

Values returned by your function will still be returned to a caller,
as with a plugin.

One benefit of being an extension vs a plugin is that you can keep
running, installing temporary procedures which are called by the user.
When they are called, the procedure you have registered will be
called, possibly accessing your persistent data or at least benefiting
from the fact that you have already started up.

Another benefit is that you can respond to events outside of GIMP,
such as network connections (this is how the Perl-Server is implemented).

Additionally, if no parameters are specified, then the extension will
be started as soon as GIMP starts up.

=head1 AUTHOR

Ed J

=head1 SEE ALSO

perl(1), L<Gimp>, L<Gimp::Fu>.
