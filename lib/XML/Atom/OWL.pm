package XML::Atom::OWL;

use 5.008;
use strict;

use Carp;
use DateTime;
use Encode qw(encode_utf8);
use HTTP::Link::Parser;
use LWP::UserAgent;
use RDF::Trine 0.112;
use URI;
use XML::LibXML qw(:all);

use constant ATOM_NS =>  'http://www.w3.org/2005/Atom';
use constant AWOL_NS =>  'http://bblfish.net/work/atom-owl/2006-06-06/#';
use constant RDF_NS =>   'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
use constant RDF_TYPE => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
use constant XSD_NS =>   'http://www.w3.org/2001/XMLSchema#';

my $VERSION = '0.01';

sub new
{
	my $class   = shift;
	my $content = shift;
	my $baseuri = shift;
	my $options = shift || undef;
	my $store   = shift || undef;
	my $domtree;
	
	unless (defined $content)
	{
		my $ua = LWP::UserAgent->new;
		$ua->agent(sprintf('%s/%s ', __PACKAGE__, $VERSION));
		$ua->default_header("Accept" => "application/atom+xml, application/xml;q=0.1, text/xml;q=0.1");
		my $response = $ua->get($baseuri);
		use Data::Dumper;
		croak "HTTP response not successful\n"
			unless $response->is_success;
		croak "Non-Atom HTTP response\n"
			unless $response->content_type =~ m`^(text/xml)|(application/(atom\+xml|xml))$`;
		$content = $response->decoded_content;
	}

	if (UNIVERSAL::isa($content, 'XML::LibXML::Document'))
	{
		($domtree, $content) = ($content, $content->toString);
	}
	else
	{
		my $xml_parser = XML::LibXML->new;
		$domtree = $xml_parser->parse_string($content);
	}

	$store = RDF::Trine::Store::DBI->temporary_store
		unless defined $store;

	my $self = bless {
		'content'   => $content,
		'baseuri'   => $baseuri,
		'options'   => $options,
		'DOM'       => $domtree,
		'RESULTS'   => RDF::Trine::Model->new($store),
		}, $class;

	return $self;
}

=item $p->uri

Returns the base URI of the document being parsed. This will usually be the
same as the base URI provided to the constructor.

Optionally it may be passed a parameter - an absolute or relative URI - in
which case it returns the same URI which it was passed as a parameter, but
as an absolute URI, resolved relative to the document's base URI.

This seems like two unrelated functions, but if you consider the consequence
of passing a relative URI consisting of a zero-length string, it in fact makes
sense.

=cut

sub uri
{
	my $this  = shift;
	my $param = shift || '';
	my $opts  = shift || {};
	
	if ((ref $opts) =~ /^XML::LibXML/)
	{
		my $x = {'element' => $opts};
		$opts = $x;
	}
	
	if ($param =~ /^([a-z][a-z0-9\+\.\-]*)\:/i)
	{
		# seems to be an absolute URI, so can safely return "as is".
		return $param;
	}
	elsif ($opts->{'require-absolute'})
	{
		return undef;
	}
	
	my $base = $this->{baseuri};
	if ($this->{'options'}->{'xml_base'})
	{
		$base = $opts->{'xml_base'} || $this->{baseuri};
	}
	
	my $url = url $param, $base;
	my $rv  = $url->abs->as_string;

	# This is needed to pass test case 0114.
	while ($rv =~ m!^(http://.*)(\.\./|\.)+(\.\.|\.)?$!i)
	{
		$rv = $1;
	}
	
	return $rv;
}

=item $p->dom

Returns the parsed XML::LibXML::Document.

=cut

sub dom
{
	my $this = shift;
	return $this->{DOM};
}

sub consume
{
	my $self = shift;
	my $root = $self->dom->documentElement;
	
	if ($root->namespaceURI eq ATOM_NS and $root->localname eq 'feed')
	{
		$self->consume_feed($root);
	}
	
	return $self;
}

sub consume_feed
{
	my $self = shift;
	my $feed = shift;
	
	# Feed
	my $feed_identifier;
	{
		my @elems = $feed->getChildrenByTagNameNS(ATOM_NS, 'id');
		if (@elems)
		{
			$feed_identifier = $elems[0]->textContent;
			$self->rdf_triple_literal($elems[0], $feed_identifier, AWOL_NS.'id', $feed_identifier, XSD_NS.'anyURI');
		}
		if (!$feed_identifier)
		{
			$feed_identifier = $self->bnode($feed);
		}
	}
	$self->rdf_triple($feed, $feed_identifier, RDF_TYPE, AWOL_NS.'Feed');

	# Common stuff
	$self->consume_feed_or_entry($feed, $feed_identifier);
	
	# Entries
	{
		my @elems = $feed->getChildrenByTagNameNS(ATOM_NS, 'entry');
		foreach my $e (@elems)
		{
			my $entry_identifier = $self->consume_entry($e);
			$self->rdf_triple($e, $feed_identifier, AWOL_NS.'entry', $entry_identifier);		
		}
	}

	# TODO - generator
	# TODO - icon
	# TODO - logo
	# TODO - subtitle
	
	return $feed_identifier;
}

sub consume_entry
{
	my $self  = shift;
	my $entry = shift;
	
	# Entry
	my $entry_identifier;
	{
		my @elems = $entry->getChildrenByTagNameNS(ATOM_NS, 'id');
		if (@elems)
		{
			$entry_identifier = $elems[0]->textContent;
			$self->rdf_triple_literal($elems[0], $entry_identifier, AWOL_NS.'id', $entry_identifier, XSD_NS.'anyURI');
		}
		if (!$entry_identifier)
		{
			$entry_identifier = $self->bnode($entry);
		}
	}
	$self->rdf_triple($entry, $entry_identifier, RDF_TYPE, AWOL_NS.'Entry');

	# Common stuff
	$self->consume_feed_or_entry($entry, $entry_identifier);
	
	# published
	{
		my @elems = $entry->getChildrenByTagNameNS(ATOM_NS, 'published');
		foreach my $e (@elems)
		{
			$self->rdf_triple_literal($e, $entry_identifier, AWOL_NS.'published', $e->textContent, XSD_NS.'dateTime');
		}
	}

	# TODO - content
	# TODO - source
	# TODO - summary	

	return $entry_identifier;
}

sub consume_feed_or_entry
{
	my $self = shift;
	my $fore = shift;
	my $id   = shift;
	
	# authors and contributors
	foreach my $role (qw(author contributor))
	{
		my @elems = $fore->getChildrenByTagNameNS(ATOM_NS, $role);
		foreach my $e (@elems)
		{
			my $person_identifier = $self->consume_person($e);
			$self->rdf_triple($e, $id, AWOL_NS.$role, $person_identifier);
		}
	}

	# updated
	{
		my @elems = $fore->getChildrenByTagNameNS(ATOM_NS, 'updated');
		foreach my $e (@elems)
		{
			$self->rdf_triple_literal($e, $id, AWOL_NS.'updated', $e->textContent, XSD_NS.'dateTime');
		}
	}

	# link
	{
		my @elems = $fore->getChildrenByTagNameNS(ATOM_NS, 'link');
		foreach my $e (@elems)
		{
			my $link_identifier = $self->consume_link($e, $id);
			$self->rdf_triple($e, $id, AWOL_NS.'link', $link_identifier);
		}
	}

	# TODO - category
	# TODO - rights
	# TODO - title
}

sub consume_person
{
	my $self   = shift;
	my $person = shift;
	
	# Person
	my $person_identifier = $self->bnode($person);
	$self->rdf_triple($person, $person_identifier, RDF_TYPE, AWOL_NS.'Person');
	
	# name
	{
		my @elems = $person->getChildrenByTagNameNS(ATOM_NS, 'name');
		foreach my $e (@elems)
		{
			$self->rdf_triple_literal($e, $person_identifier, AWOL_NS.'name', $e->textContent);
		}
	}

	# uri
	{
		my @elems = $person->getChildrenByTagNameNS(ATOM_NS, 'uri');
		foreach my $e (@elems)
		{
			$self->rdf_triple($e, $person_identifier, AWOL_NS.'url', $e->textContent);
		}
	}

	# email
	{
		my @elems = $person->getChildrenByTagNameNS(ATOM_NS, 'email');
		foreach my $e (@elems)
		{
			$self->rdf_triple($e, $person_identifier, AWOL_NS.'email', 'mailto:'.$e->textContent);
		}
	}

	return $person_identifier;
}


sub consume_link
{
	my $self    = shift;
	my $link    = shift;
	my $subject = shift || undef;
	
	# Link
	my $link_identifier = $self->bnode($link);
	$self->rdf_triple($link, $link_identifier, RDF_TYPE, AWOL_NS.'Link');

	# Destination
	my $destination_identifier = $self->bnode;
	$self->rdf_triple($link, $destination_identifier, RDF_TYPE, AWOL_NS.'Content');
	$self->rdf_triple($link, $link_identifier, AWOL_NS.'to', $destination_identifier);

	# rel
	{
		my $rel = HTTP::Link::Parser::relationship_uri(
			$link->hasAttribute('rel') ? $link->getAttribute('rel') : 'alternate');
		$self->rdf_triple($link, $link_identifier, AWOL_NS.'rel', $rel);
		
		if ($link->hasAttribute('href') and defined $subject)
		{
			my $href = $self->uri($link->getAttribute('href'));
			$self->rdf_triple($link, $subject, $rel, $href);
		}
	}
	
	# href
	if ($link->hasAttribute('href'))
	{
		my $href = $self->uri($link->getAttribute('href'));
		$self->rdf_triple($link, $destination_identifier, AWOL_NS.'src', $href);
	}

	# hreflang
	if ($link->hasAttribute('hreflang'))
	{
		my $hreflang = $link->getAttribute('hreflang');
		$self->rdf_triple_literal($link, $destination_identifier, AWOL_NS.'lang', $hreflang);
	}

	# length
	if ($link->hasAttribute('length'))
	{
		my $length = $link->getAttribute('length');
		$self->rdf_triple_literal($link, $destination_identifier, AWOL_NS.'length', $length, XSD_NS.'integer');
	}

	# type
	if ($link->hasAttribute('type'))
	{
		my $type = $link->getAttribute('type');
		$self->rdf_triple_literal($link, $destination_identifier, AWOL_NS.'type', $type);
	}

	# title: TODO - check this uses AWOL properly.
	if ($link->hasAttribute('title'))
	{
		my $title = $link->getAttribute('title');
		$self->rdf_triple_literal($link, $link_identifier, AWOL_NS.'title', $title);
	}

	return $link_identifier;
}


=item $p->graph() 

This method will return an RDF::Trine::Model object with all
statements of the full graph.

It makes sense to call C<consume> before calling C<graph>. Otherwise
you'll just get an empty graph.

=cut

sub graph
{
	my $this = shift;
	return $this->{RESULTS};
}

sub graphs
{
	my $this = shift;
	return { $this->{'baseuri'} => $this->{RESULTS} };
}

sub rdf_triple
# Function only used internally.
{
	my $this = shift;

	my $suppress_triple = 0;
	if ($this->{'sub'}->[0])
	{
		$suppress_triple = $this->{'sub'}->[0]($this, @_);
	}
	return if $suppress_triple;
	
	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $object    = shift;  # Resource URI or bnode
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# First make sure the object node type is ok.
	my $to;
	if ($object =~ m/^_:(.*)/)
	{
		$to = RDF::Trine::Node::Blank->new($1);
	}
	else
	{
		$to = RDF::Trine::Node::Resource->new($object);
	}

	# Run the common function
	return $this->rdf_triple_common($element, $subject, $predicate, $to, $graph);
}

sub rdf_triple_literal
# Function only used internally.
{
	my $this = shift;

	my $suppress_triple = 0;
	if ($this->{'sub'}->[1])
	{
		$suppress_triple = $this->{'sub'}->[1]($this, @_);
	}
	return if $suppress_triple;

	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $object    = shift;  # Resource Literal
	my $datatype  = shift;  # Datatype URI (possibly undef or '')
	my $language  = shift;  # Language (possibly undef or '')
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# Now we know there's a literal
	my $to;
	
	# Work around bad Unicode handling in RDF::Trine.
	$object = encode_utf8($object);

	if (defined $datatype)
	{
		if ($datatype eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral')
		{
			if ($this->{'options'}->{'use_rtnlx'})
			{
				eval
				{
					require RDF::Trine::Node::Literal::XML;
					$to = RDF::Trine::Node::Literal::XML->new($element->childNodes);
				};
			}
			
			if ( $@ || !defined $to)
			{
				my $orig = $RDF::Trine::Node::Literal::USE_XMLLITERALS;
				$RDF::Trine::Node::Literal::USE_XMLLITERALS = 0;
				$to = RDF::Trine::Node::Literal->new($object, undef, $datatype);
				$RDF::Trine::Node::Literal::USE_XMLLITERALS = $orig;
			}
		}
		else
		{
			$to = RDF::Trine::Node::Literal->new($object, undef, $datatype);
		}
	}
	else
	{
		$to = RDF::Trine::Node::Literal->new($object, $language, undef);
	}

	# Run the common function
	$this->rdf_triple_common($element, $subject, $predicate, $to, $graph);
}

sub rdf_triple_common
# Function only used internally.
{
	my $this      = shift;  # A reference to the RDF::RDFa::Parser object
	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $to        = shift;  # RDF::Trine::Node Resource URI or bnode
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# First, make sure subject and predicates are the right kind of nodes
	my $tp = RDF::Trine::Node::Resource->new($predicate);
	my $ts;
	if ($subject =~ m/^_:(.*)/)
	{
		$ts = RDF::Trine::Node::Blank->new($1);
	}
	else
	{
		$ts = RDF::Trine::Node::Resource->new($subject);
	}

	# If we are configured for it, and graph name can be found, add it.
	if (ref($this->{'options'}->{'named_graphs'}) && ($graph))
	{
		$this->{Graphs}->{$graph}++;
		
		my $tg;
		if ($graph =~ m/^_:(.*)/)
		{
			$tg = RDF::Trine::Node::Blank->new($1);
		}
		else
		{
			$tg = RDF::Trine::Node::Resource->new($graph);
		}

		my $statement = RDF::Trine::Statement::Quad->new($ts, $tp, $to, $tg);
		$this->{RESULTS}->add_statement($statement);
	
		#if ($graph ne $this->{'options'}->{'named_graphs'}->{'default'})
		#{
		#	my $graph_statement = RDF::Trine::Statement::Quad->new($ts, $tp, $to, 
		#		$this->{'options'}->{'named_graphs'}->{'default_trine'});
		#	$this->{RESULTS}->add_statement($graph_statement,
		#		$this->{'options'}->{'named_graphs'}->{'default_trine'});
		#}
	}
	else
	{
		# If no graph name, just add triples
		my $statement = RDF::Trine::Statement->new($ts, $tp, $to);
		$this->{RESULTS}->add_statement($statement);
	}
}

sub bnode
# Function only used internally.
{
	my $this    = shift;
	my $element = shift;
	
	return sprintf('_:RDFaAutoNode%03d', $this->{bnodes}++);
}

sub valid_lang
{
	my $value_to_test = shift;

	return 1 if (defined $value_to_test) && ($value_to_test eq '');
	return 0 unless defined $value_to_test;
	
	# Regex for recognizing RFC 4646 well-formed tags
	# http://www.rfc-editor.org/rfc/rfc4646.txt
	# http://tools.ietf.org/html/draft-ietf-ltru-4646bis-21

	# The structure requires no forward references, so it reverses the order.
	# It uses Java/Perl syntax instead of the old ABNF
	# The uppercase comments are fragments copied from RFC 4646

	# Note: the tool requires that any real "=" or "#" or ";" in the regex be escaped.

	my $alpha      = '[a-z]';      # ALPHA
	my $digit      = '[0-9]';      # DIGIT
	my $alphanum   = '[a-z0-9]';   # ALPHA / DIGIT
	my $x          = 'x';          # private use singleton
	my $singleton  = '[a-wyz]';    # other singleton
	my $s          = '[_-]';       # separator -- lenient parsers will use [_-] -- strict will use [-]

	# Now do the components. The structure is slightly different to allow for capturing the right components.
	# The notation (?:....) is a non-capturing version of (...): so the "?:" can be deleted if someone doesn't care about capturing.

	my $language   = '([a-z]{2,8}) | ([a-z]{2,3} $s [a-z]{3})';
	
	# ABNF (2*3ALPHA) / 4ALPHA / 5*8ALPHA  --- note: because of how | works in regex, don't use $alpha{2,3} | $alpha{4,8} 
	# We don't have to have the general case of extlang, because there can be only one extlang (except for zh-min-nan).

	# Note: extlang invalid in Unicode language tags

	my $script = '[a-z]{4}' ;   # 4ALPHA 

	my $region = '(?: [a-z]{2}|[0-9]{3})' ;    # 2ALPHA / 3DIGIT

	my $variant    = '(?: [a-z0-9]{5,8} | [0-9] [a-z0-9]{3} )' ;  # 5*8alphanum / (DIGIT 3alphanum)

	my $extension  = '(?: [a-wyz] (?: [_-] [a-z0-9]{2,8} )+ )' ; # singleton 1*("-" (2*8alphanum))

	my $privateUse = '(?: x (?: [_-] [a-z0-9]{1,8} )+ )' ; # "x" 1*("-" (1*8alphanum))

	# Define certain grandfathered codes, since otherwise the regex is pretty useless.
	# Since these are limited, this is safe even later changes to the registry --
	# the only oddity is that it might change the type of the tag, and thus
	# the results from the capturing groups.
	# http://www.iana.org/assignments/language-subtag-registry
	# Note that these have to be compared case insensitively, requiring (?i) below.

	my $grandfathered  = '(?:
			  (en [_-] GB [_-] oed)
			| (i [_-] (?: ami | bnn | default | enochian | hak | klingon | lux | mingo | navajo | pwn | tao | tay | tsu ))
			| (no [_-] (?: bok | nyn ))
			| (sgn [_-] (?: BE [_-] (?: fr | nl) | CH [_-] de ))
			| (zh [_-] min [_-] nan)
			)';

	# old:         | zh $s (?: cmn (?: $s Hans | $s Hant )? | gan | min (?: $s nan)? | wuu | yue );
	# For well-formedness, we don't need the ones that would otherwise pass.
	# For validity, they need to be checked.

	# $grandfatheredWellFormed = (?:
	#         art $s lojban
	#     | cel $s gaulish
	#     | zh $s (?: guoyu | hakka | xiang )
	# );

	# Unicode locales: but we are shifting to a compatible form
	# $keyvalue = (?: $alphanum+ \= $alphanum+);
	# $keywords = ($keyvalue (?: \; $keyvalue)*);

	# We separate items that we want to capture as a single group

	my $variantList   = $variant . '(?:' . $s . $variant . ')*' ;     # special for multiples
	my $extensionList = $extension . '(?:' . $s . $extension . ')*' ; # special for multiples

	my $langtag = "
			($language)
			($s ( $script ) )?
			($s ( $region ) )?
			($s ( $variantList ) )?
			($s ( $extensionList ) )?
			($s ( $privateUse ) )?
			";

	# Here is the final breakdown, with capturing groups for each of these components
	# The variants, extensions, grandfathered, and private-use may have interior '-'
	
	my $r = ($value_to_test =~ 
		/^(
			($langtag)
		 | ($privateUse)
		 | ($grandfathered)
		 )$/xi);
	return $r;
}

1;