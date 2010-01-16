use lib "lib";
use RDF::Trine::Serializer::NTriples;
use LWP::Simple;
use XML::Atom::OWL;
use File::Slurp qw(slurp);

my $u = "http://bblfish.net/work/atom-owl/2006-06-06/examples/example-2.atom";
my $d = get($u);
my $p = XML::Atom::OWL->new($d, $u);
$p->consume;

my $s = RDF::Trine::Serializer::NTriples->new;
print $s->serialize_model_to_string($p->graph);
