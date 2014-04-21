use Test::More;
#$Gimp::verbose = 1;
require Gimp::Pod;

my $p = Gimp::Pod->new;
ok($p, 'obj init');
is_deeply([ $p->sections ], [ qw(NAME VERBATIM OTHER) ], 'sections');
is($p->section('NAME'), 'test - Run some tests', 'sect name');
is($p->section('VERBATIM'), " verbatim\n verbatim2", 'sect verbatim');
is($p->section('OTHER'), 'Other text.', 'sect at eof');

done_testing;
__END__

=head1 NAME

test - Run some tests

=head1 VERBATIM

 verbatim
 verbatim2 

=head1 OTHER

Other text.
