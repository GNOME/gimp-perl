=head1 NAME

 Gimp::Compat - compatibility functions for older verasions of Gimp.

=head1 SYNOPSIS

 <loaded automatically on demand>

=head1 DESCRIPTION

Older versions of Gimp (vesion 1.0 at the time of this writing) lack some
very important or useful functions.

This module is providing the most needed replacement functions. If you
happen to miss a function then please create a replacement function and
send it to me ;)

These functions are handled in exactly the same way as PDB-Functions,
i.e. the (hypothetical) function C<gimp_image_xyzzy> can be called as
$image->xyzzy, if the module is available.

=head1 FUNCTIONS

=over 4

=item gimp_text_fontname

=item gimp_get_extents_fontname

=back

=head1 AUTHOR

Various, Dov Grobgeld <dov@imagic.weizmann.ac.il>. The author of the
Gimp-Perl extension (contact him to include new functions) is Marc Lehmann
<pcg@goof.com>

=cut

package      Gimp::Compat;

$VERSION=$Gimp::VERSION;

use Gimp ();

# as a failsafe check, lowest version NOT requiring this module
@max_gimp_version = (1,1);

# The following function is used to convert a xlfd to the array structure
# used by the pre 1.2 functions gimp_text_ext() and gimp_text_get_extent_ext().
	      
sub xlfd_unpack {
    my $fontname = shift;
    my $size_overload = shift;
    my $size_unit_overload = shift;
    # XLFDs fields can contain anything, including minus signs, but we
    # gracefully ignore these weird things here ;)
    my $p = "[^-]*";
    my($foundry,
         $family,
           $weight,
              $slant,
                 $set_width,
                    $pixelsize,
                       $pointsize, 
                          $spacing,
                             $registry,
                                $encoding,
      ) = $fontname=~
     /^-($p)     (?# foundry )
       -($p)     (?# family  )
       -($p)     (?# weight  )
       -($p)     (?# slant   )
       -($p)     (?# set_Width )
       -$p       
       -($p)     (?# pixelsize )
       -($p)     (?# pointsize )
       -$p      
       -$p       
       -($p)     (?# spacing )
       -$p
       -($p)     (?# rgstry )
       -($p)     (?# encdng )
     /x or die "xlfd_unpack: unmatched XLFD '$fontname'\n";

    my $size;
    if ($pixelsize && $pixelsize ne "*") {
	$size = $pixelsize;
    } else {
	$size = 0.1*$pointsize;
    }
    $size = $size_overload if $size_overload;
    my $size_unit = ($pointsize > 0);
    $size_unit = $size_unit if defined $size_unit_overload;
    return ($size, $size_unit, $foundry, $family, $weight, $slant,
	    $set_width, $spacing, $registry, $encoding);
}

sub fun {
   my($major,$minor,$name,$sub)=@_;
   if (Gimp->major_version < $major
       || (Gimp->major_version == $major && Gimp->minor_version < $minor)) {
      print "overwriting Gimp::Lib::$name ($major,$minor)\n";
      *{"Gimp::Lib::$name"}=$sub;
   }
}

fun 1,1,gimp_text_get_extents_fontname,sub {
   my($string, $xlfd_size, $xlfd_unit, $xlfd) = @_;
   
   Gimp->text_get_extents_ext($string, @font_info,
                              xlfd_unpack($xlfd, $xlfd_size, $xlfd_unit));
};
	
fun 1,1,gimp_text_fontname,sub {
   shift if $_[0]->isa('Gimp::Image');
   my ($drw, $x,$y, $string,$border,$antialias, $xlfd_size, $xlfd_unit, $xlfd) = @_;
   
   Gimp->text_ext($drw, $x, $y, $string, $border, $antialias,
                  xlfd_unpack($xlfd, $xlfd_size, $xlfd_unit));
};

1;


