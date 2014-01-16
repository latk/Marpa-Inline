#!/usr/bin/env perl

use strict;
use warnings;

use Test::More tests => 3;
use Marpa::R2;

BEGIN {
    use_ok 'MarpaX::Interface::Inline::Recognizer';
}

# set up a grammar that can be used for our tests.
my $grammar = Marpa::R2::Thin::G->new({ if => 1 });

# all symbols (rules and terminals) in the grammar
my $s_TOP           = $grammar->symbol_new;
my $s_Expression    = $grammar->symbol_new;
my $s_Expression_1  = $grammar->symbol_new; # plus, minus
my $s_Expression_2  = $grammar->symbol_new; # mult, div
my $s_Expression_3  = $grammar->symbol_new; # literal
my $s_Number        = $grammar->symbol_new;
my $s_OpPlus        = $grammar->symbol_new;
my $s_OpMinus       = $grammar->symbol_new;
my $s_OpMult        = $grammar->symbol_new;
my $s_OpDivide      = $grammar->symbol_new;

# declare and register the TOP rule
my $r_TOP           = $grammar->rule_new($s_TOP,        [$s_Expression]);
$grammar->start_symbol_set($r_TOP);

# often used action
my $a_first = sub {
    return shift;
};

# define the rules for our grammar as data
my @rules = (
    [$s_Expression =>
        [[$s_Expression_1],
         [0] => $a_first],
    ],
    [$s_Expression_1 =>
        [[$s_Expression_1, $s_OpPlus, $s_Expression_2],
         [0, 2] => sub {
            my ($x, $y) = @_;
            return $x + $y;
        }],
        [[$s_Expression_1, $s_OpMinus, $s_Expression_2],
         [0, 2] => sub {
            my ($x, $y) = @_;
            return $x - $y;
        }],
        [[$s_Expression_2],
         [0] => $a_first],
    ],
    [$s_Expression_2 =>
        [[$s_Expression_2, $s_OpMult, $s_Expression_3],
         [0, 2] => sub {
            my ($x, $y) = @_;
            return $x * $y;
        }],
        [[$s_Expression_2, $s_OpDivide, $s_Expression_3],
         [0, 2] => sub {
            my ($x, $y) = @_;
            return $x / $y;
        }],
        [[$s_Expression_3],
         [0] => $a_first],
    ],
    [$s_Expression_3 =>
        [[$s_Number],
         [0] => $a_first],
    ],
);

# create Marpa versions of our rules and register actions
my %actions = ($r_TOP => [[0], $a_first]);
for my $rule (@rules) {
    my ($lhs, @options) = @$rule;
    for my $i (0 .. $#options) {
        my ($rhs, $signature, $action) = @{ $options[$i] };
        my $rule_id = $grammar->rule_new($lhs, $rhs);
        $grammar->rule_rank_set($rule_id, @options - $i);
        $actions{$rule_id} = [$signature, $action] if $action;
    }
}

$grammar->precompute;

my %terminals = (
    $s_Number => qr/\G([+-]?[0-9]+(?:[.][0-9]+)?)/,
    $s_OpPlus   => \'+',
    $s_OpMinus  => \'-',
    $s_OpMult   => \'*',
    $s_OpDivide => \'/',
);

# create our recognizer instance
my $recce = new_ok 'MarpaX::Interface::Inline::Recognizer', [
    grammar => $grammar,
    actions => \%actions,
    terminals => \%terminals,
];

# parse a tricky string
my $result = $recce->parse(\'2*-3--6+-4/2');
#   2 * -3  -  -6  +  -4 / 2
#  (2 * -3) -  -6  + (-4 / 2)
#  (  -6  ) -  -6  + (  -2  )
# ((  -6  ) -  -6) + (  -2  )
#           0      + (  -2  )
#                 -2
is $result, -2;
