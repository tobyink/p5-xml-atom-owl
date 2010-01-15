use lib "lib";
use RDF::Trine::Serializer::NTriples;
use LWP::Simple;
use XML::Atom::OWL;

my $u = "http://bblfish.net/work/atom-owl/2006-06-06/examples/example-1.atom";
my $p = XML::Atom::OWL->new(get($u), $u);
$p->consume;

my $s = RDF::Trine::Serializer::NTriples->new;
print $s->serialize_model_to_string($p->graph);