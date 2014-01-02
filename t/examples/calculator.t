#!/usr/bin/env perl
# kate: hl perl
use strict;
use warnings;
use MarpaX::DSL::InlineActions;
use Test::More tests => 1;

my $parser = MarpaX::DSL::InlineActions->new(string => <<'END_GRAMMAR');
    TOP:
    || _ $val=Expression _ => {{
        return $val;
    }}
    
    Expression:
    "(" _ $list=Expression +% (_ "," _ => {{ }}) _ ")" => {{
        return $list->[-1];
    }}
    $number=m/\d+/ => {{
        return 0+$number;
    }}
    $x=Expression _ "+" _ $y=Expression => {{
        return $x + $y;
    }}
    $x=Expression _ "-" _ $y=Expression => {{
        return $x - $y;
    }}
    $x=Expression _ "*" _ $y=Expression => {{
        return $x * $y;
    }}
    $x=Expression _ "/" _ $y=Expression => {{
        return $x / $y;
    }}
    
    _:
    || m/\s+/? => {{
        return;
    }}
END_GRAMMAR

is $parser->parse(\<<'END'), 14;
    3 * 4 + (4, 2)
END