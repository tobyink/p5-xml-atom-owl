use strict;
use warnings;

use inc::Module::Install;

my $dist = 'XML-Atom-OWL';
my $fn   = "lib/$dist.pm"; $fn =~ s#-#/#g;

name                $dist;
perl_version_from   $fn;
version_from        $fn;
abstract_from       $fn;
readme_from         $fn;
author              'Toby Inkster <tobyink@cpan.org>';
license             'perl';

requires            'Carp'               => '1.00';
requires            'common::sense'      => 0;
requires            'DateTime'           => 0;
requires            'Encode'             => 0;
requires            'HTTP::Link::Parser' => '0.04';
requires            'LWP::UserAgent'     => 0;
requires            'MIME::Base64'       => 0;
requires            'RDF::Trine'         => '0.112';
test_requires       'Test::More'         => '0.61';
requires            'Scalar::Util'       => 0;
requires            'URI'                => '1.30';
requires            'URI::URL'           => 0;
requires            'XML::LibXML'        => '1.60';

# install_script 'fingerw';

resources(
	'homepage'   => "http://search.cpan.org/dist/$dist/",
	'repository' => "http://goddamn.co.uk/viewvc/perlmods/$dist/",
	'bugtracker' => "http://rt.cpan.org/Dist/Display.html?Queue=$dist",
	);
	
write_doap_changes;
write_doap_changes_xml;

auto_install;
WriteAll(
	'meta' => 1,
	'sign' => 1,
	);
