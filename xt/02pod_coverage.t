use Test::More skip_all => "There's a lot of effectively private methods without a leading underscore.";
use Test::Pod::Coverage;

my @modules = qw(XML::Atom::OWL);
pod_coverage_ok($_, "$_ is covered")
	foreach @modules;
done_testing(scalar @modules);

