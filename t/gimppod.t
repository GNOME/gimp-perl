use Test::More;
$Gimp::verbose = $Gimp::verbose = 0; # normally done in Gimp.pm
require Gimp::Pod;

my $p = Gimp::Pod->new;
ok($p, 'obj init');
is_deeply(
  [ $p->sections ],
  [ ('NAME', 'SPACE NAME', 'VERBATIM', 'OTHER') ], 'sections'
);
is($p->section('NAME'), 'test - Run some tests', 'sect name');
is(
  $p->section('SPACE NAME'),
  "Some bold text.\n\nSecond para.",
  'sect space-name'
);
is(
  $p->section('VERBATIM'),
  " verbatim\n verbatim2 \n\n new verbatim para",
  'sect verbatim'
);
is($p->section('OTHER'), 'Other text.', 'sect at eof');
is($p->section('NOT THERE'), undef, 'sect not there');

done_testing;
__END__

=head1 NAME

test - Run some tests

=head1 SPACE NAME

Some B<bold> text.

Second para.

=head1 VERBATIM

 verbatim
 verbatim2 

 new verbatim para

=head1 OTHER

Other text.
