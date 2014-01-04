#!/usr/bin/env perl
# kate: hl perl
use strict;
use warnings;
use MarpaX::DSL::InlineActions;
use Test::More tests => 1;

my $parser = MarpaX::DSL::InlineActions->new(string => <<'END_GRAMMAR');
    # see "ECMA 404" for the JSON standard, or <json.org> for a summary
    TOP:
        _ $val=Value _ => {{ $val }}
    Value:
        "{" _ $data=KVPair* %! comma _ "}" => {{
            my %hash = map { @$_ } @$data;
            return \%hash;
        }}
        "[" _ $data=Value* %! comma _ "]" => {{
            return $data;
        }}
        $x=m/[-]? (?:0 | [1-9][0-9]*) (?:[.][0-9]+)? (?:[eE][+-]?[0-9]+)?/x => {{
            return 0+$x,
        }}
        $str=String => {{ $str }}
        "true"  => {{ 1 }}
        "false" => {{ 0 }}
        "null"  => {{ undef }}
    KVPair:
        $key=String _ ":" _ $val=Value => {{
            return [$key => $val];
        }}
    comma:
        _ "," _ => {{ }}
    String:
        '"' $chars=StringChar+ '"' => {{
            return join "" => @$chars;
        }}
    StringChar:
        "\\u" $hex=m/[0-9A-Fa-f]{4}/ => {{
            return chr hex $hex;
        }}
        "\\" $c=(
                || $escaped=m<["\\/]>   => {{ $escaped }}
                || "b"  => {{ "\b" }}
                || "f"  => {{ "\f" }}
                || "n"  => {{ "\n" }}
                || "r"  => {{ "\r" }}
                || "t"  => {{ "\t" }}) => {{
            return $c;
        }}
        $substr=m/[^\\"]++/ => {{
            return $substr;
        }}
    _:
        m/[\t\n\r\ ]+/? => {{ }}
END_GRAMMAR

my $json = <<'END';
    {
        "foo":42E0,
        "bar" : [ -12.3e-1, "foo\r\n\u000a\f\b\tbar", null],
        "true": false,
        "false" :true
    }
END
is_deeply $parser->parse(\$json), {
    foo => 42,
    bar => [-1.23, "foo\r\n\n\f\b\tbar", undef],
    true => 0,
    false => 1,
};