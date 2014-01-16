package MarpaX::Interface::Inline::Recognizer;

use strict;
use warnings;
use feature qw< say unicode_strings >;
use re '/xsm';

use Moo;
use Carp ();

# A hash that maps terminals IDs to string references or regexps.
has terminals => (
    is => 'ro',
);

# A Marpa::R2::Thin::G instance.
has grammar => (
    is => 'ro',
);

# A hashref that maps rule IDs to arrayrefs which contain a signature and a code reference.
# A signature is an arrayref containing the indices of captured symbols.
has actions => (
    is => 'ro',
);

# An internal recognizer class name.
has recce_class => (
    is => 'ro',
    default => sub { 'Marpa::R2::Thin::R' },
);

sub parse {
    my ($self, $string_reference, $pos, $end) = @_;

    # create a new THIF recognizer
    my $recce = $self->recce_class->new($self->grammar);

    # scan the input string
    my ($values, $positions, $lengths) = $self->_read($string_reference, $pos, $end, $recce);

    # The input has now been parsed, so we evaluate the parse tree

    # Get access to the parse trees
    my $bocage = Marpa::R2::Thin::B->new($recce, $recce->latest_earley_set);
    # discard less important alternatives – keep highest rank only
    my $ordering = Marpa::R2::Thin::O->new($bocage);
    $ordering->high_rank_only_set(1);
    # init the parse tree iterator
    my $tree = Marpa::R2::Thin::T->new($ordering);
    $tree->next or Carp::confess q(No successful parse);
    # TODO check if other parse trees exist
    # init the stack driver
    my $valuator = Marpa::R2::Thin::V->new($tree);


    # tell the $valuator about our actions
    for my $id (keys %{ $self->actions }) {
        $valuator->rule_is_valued_set($id, 1);
    }

    # perform the semantics
    return $self->_value($valuator, $values, $positions, $lengths);
}

sub _read {
    my ($self, $string_reference, $pos, $end, $recce) = @_;

    # set up the start and end bounds
    $pos //= 0;
    $end //= length $$string_reference;

    # set up a container for the token values (positions and lengths)
    my @values    = (undef);
    my @positions = (undef);
    my @lengths   = (undef);

    # do longest expected token-matching
    $recce->start_input;
    POSITION:
    while ($pos < $end) {
        my $len = 0;  # the length of the longest token(s) at this position
        my %matches;  # the values for all longest tokens here

        TOKEN:
        for my $expected_id ($recce->terminals_expected) {
            # fetch a reference to the pattern – \"foo" or qr/foo/.
            my $pattern = $self->terminals->{$expected_id}
                or Carp::confess qq(The recognizer requested the token ID "$expected_id" which does not exist);

            my $substr;  # on success, this will hold the consumed substring

            # case 1: strings
            if (ref $pattern eq 'SCALAR') {
                next TOKEN if length $$pattern < $len;
                $substr = substr $$string_reference, $pos, length $$pattern;
                next TOKEN if not $substr eq $$pattern;
            }
            # case 1: regex
            elsif (ref $pattern eq 'Regexp') {
                pos($$string_reference) = $pos;
                if ($$string_reference =~ /$pattern/gc) {
                    $substr = $1;
                }
                else {
                    next TOKEN;
                }
            }
            # case no reference: an error
            elsif (not length ref $pattern) {
                Carp::confess qq(The pattern is not a reference: "$pattern");
            }
            # case default: anything else is an error,
            else {
                my $ref = ref $pattern;
                Carp::confess qq(Unknown pattern type "$ref");
            }

            # add the matches $substr to the matches. Throw away shorter tokens.
            if ($len < length $substr) {
                $len = length $substr;
                %matches = ();
            }
            $matches{$expected_id} = $substr;
        }

        # At this point all tokens for this position have been matched.
        # Now, store all values permanently,
        # and offer the matches terminals back to the $recce.
        # TODO beause of longest token matching, all tokens at one position have the same value
        while (my ($rule, $value) = each %matches) {
            # register the value, position, and length
            # TODO share values like common keywords if possible
            push @values,    $value;
            push @positions, $pos;
            push @lengths,   $len;

            # associate the index for this token
            # by design, each token has length 1
            $recce->alternative($rule, $#values, 1);
        }

        $recce->earleme_complete;  # TODO handle events

        # update the positional information
        $pos += $len;
    }

    return \@values, \@positions, \@lengths;
}

sub _value {
    my ($self, $valuator, $values, $positions, $lengths) = @_;

    # The valuator stack
    my @stack;
    # TODO add start position tracking

    while (my ($type, @data) = $valuator->step) {
        # A token just copies the associated value onto the stack
        if ($type eq 'MARPA_STEP_TOKEN') {
            my ($id, $value_ix, $stack_ix) = @data;
            $stack[$stack_ix] = $values->[$value_ix];
        }
        # A rule action is implemented by an external closure
        elsif ($type eq 'MARPA_STEP_RULE') {
            my ($id, $start_ix, $end_ix) = @data;
            my $action = $self->actions->{$id}
                or Carp::confess qq(No action was registered for rule "$id");
            my ($signature, $code) = @$action;
            my @indices = map { $_ + $start_ix } @$signature;
            if (@indices and $indices[-1] > $end_ix) {
                Carp::confess q(signature index out of bounds);
            }
            $stack[$start_ix] = $code->(@stack[@indices]);
        }
        # For now, nulling symbols just get "undef" as a value
        # TODO add option to inject garbage instead, for hardening
        elsif ($type eq 'MARPA_STEP_NULLING_SYMBOL') {
            my ($id, $ix) = @data;
            $stack[$ix] = undef;
        }
        # I think I haven't handled "MARPA_STEP_INACTIVE"
        else {
            Carp::confess qq(No implementation for stack command "$type");
        }
    }

    # the commands are structured so that the result is in the bottom stack entry
    return $stack[0];
}

1;