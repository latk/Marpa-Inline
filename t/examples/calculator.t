#!/usr/bin/env perl
# kate: hl perl
use strict;
use warnings;
use MarpaX::DSL::InlineActions;
use Test::More tests => 1;

my $parser = MarpaX::DSL::InlineActions->new(string => <<'END_GRAMMAR');
    TOP
        := _ $val=Expression _ {{ $val }}
    Expression
        :=  "(" _ $list=Expression +% "," _ ")" {{ $list->[-1] }}
        ||  $number=m/\d+/ {{ 0+$number }}
        ||  $x=Expression _ "+" _ $y=Expression {{ $x + $y }}
        ||  $x=Expression _ "-" _ $y=Expression {{ $x - $y }}
        ||  $x=Expression _ "*" _ $y=Expression {{ $x * $y }}
        ||  $x=Expression _ "/" _ $y=Expression {{ $x / $y }}
    _
        := m/\s*/ {{ }}
END_GRAMMAR

is $parser->parse(\<<'END'), 14;
    3 * 4 + (4,2)
END