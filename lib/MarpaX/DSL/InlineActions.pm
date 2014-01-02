package MarpaX::DSL::InlineActions;

use strict;
use warnings;
use utf8;
use feature qw< unicode_strings say >;
use Marpa::R2;
use String::Escape ();
use mop;

use MarpaX::DSL::InlineActions::Ast;
use MarpaX::DSL::InlineActions::Compiler;

use Data::Dump;

my $grammar_source = <<'END_GRAMMAR';
:default ::= action => [values]
:start ::= Grammar

Grammar
    ::= (_) Statements (_) action => do_Grammar
Statements
    ::= Statement+ separator => __
Statement
    ::= name (_ ':=' _) Options action => do_Statement
Options
    ::= Option* separator => OpOption
OpOption
    ::= (_) '||' (_)
Option
    ::= Patterns (__) code action => do_Option
Patterns
    ::= Pattern+ separator => __    action => [values]
Pattern
    ::= scalarVariable ('=') Rule   action => do_Pattern
    ||  Null                 Rule   action => do_Pattern
    
Rule
    ::= stringDq                    action => do_stringDq
    ||  stringSq                    action => do_stringSq
    ||  regexBody regexModifiers    action => do_Regex
    ||  regexBody                   action => do_Regex
    ||  name                        action => do_RuleReference
    ||  Rule (_) '+%' (_) Rule  action => do_SequenceRule
    ||  Rule (_) '*%' (_) Rule  action => do_SequenceRule
    ||  Rule (_) '+'            action => do_SequenceRule
    ||  Rule (_) '*'            action => do_SequenceRule
    
regexBody
    ~   regexIntro '/' regexBodySlash   '/'
    |   regexIntro '(' regexBodyParen   ')'
    |   regexIntro '{' regexBodyCurly   '}'
    |   regexIntro '[' regexBodyBracket ']'
    |   regexIntro '<' regexBodyLtGt    '>'
regexIntro ~ [mr]
regexBodySlash   ~ regexBodySlashChar+
regexBodySlashChar   ~ [^\\/]  | escaped
regexBodyParen   ~ regexBodyParenChar+
regexBodyParenChar   ~ [^\\)]  | escaped
regexBodyCurly   ~ regexBodyCurlyChar+
regexBodyCurlyChar   ~ [^\\}]  | escaped
regexBodyBracket ~ regexBodyBracketChar+
regexBodyBracketChar ~ [^\\\]] | escaped
regexBodyLtGt    ~ regexBodyLtGtChar+
regexBodyLtGtChar    ~ [^\\>]  | escaped
escaped ~ '\' [\p{Any}]

regexModifiers ~ regexModifier+
regexModifier ~ [alupimsx]

Null ::= action => ::undef
_ ::= ws
_ ::= comment
_ ::=
__ ::= ws
ws ~ [\s]+
comment ~ '#' commentBody [\n]
commentBody ~ [^\n]*

name ~ nameStart nameBody
scalarVariable ~ '$' nameStart nameBody
nameStart ~ [\pL_]
nameBody ~ [\p{Word}]*

stringDq ~ ["] stringDqBody ["]
stringDqBody ~ stringDqPart+
stringDqPart ~ [^\\"] | escaped
stringSq ~ ['] stringSqBody [']
stringSqBody ~ stringSqPart+
stringSqPart ~ [^\\'] | escaped
    
code ~ 
code ~ '{{' codeBody '}}'
codeBody ~ codeBodyChar+
codeBodyChar ~ [^}] | '}' [^}]
END_GRAMMAR

my $grammar = Marpa::R2::Scanless::G->new({
    source => \$grammar_source,
    bless_package => 'MarpaX::DSL::InlineActions::Ast',
});

sub new {
    my ($class, %args) = @_;
    my $string = delete $args{string} or die q("string" required);
    
    my $recce = Marpa::R2::Scanless::R->new({
        grammar => $grammar,
        semantics_package => 'MarpaX::DSL::InlineActions::Actions',
    });
        
    $recce->read(\$string);
    my $result = $recce->value // die "No parse!";
    my $ast = $$result;

    say "$ast";

    return MarpaX::DSL::InlineActions::Compiler->new->compile($ast);
}

class MarpaX::DSL::InlineActions::Actions {

    my $new = sub {
        my ($class, @args) = @_;
        "MarpaX::DSL::InlineActions::Ast::$class"->new(@args);
    };

    method do_Grammar($statements) {
        return Grammar->$new(statements => $statements);
    }

    method do_Statement($name, $options) {
        return Statement->$new(name => $name, options => $options);
    }
    
    method do_Option($pattern, $action) {
        $action =~ s/\A\{\{//;
        $action =~ s/\}\}\z//;
        return Option->$new(pattern => $pattern, action => $action);
    }
    
    method do_Pattern($name, $rule) {
        return NamedPattern->$new(name => $name, rule => $rule) if defined $name;
        return Pattern->$new(rule => $rule);
    }
    
    method do_stringDq($str) {
        return String->$new(value => String::Escape::unqqbackslash $str);
    }
    
    method do_stringSq($str) {
        return String->$new(value => String::Escape::unbackslash String::Escape::unsinglequote $str);
    }
    
    method do_Regex($pattern, $modifiers) {
        $pattern =~ s/\A[mr].//;
        $pattern =~ s/.\z//;
        return Regex->$new(value => length $modifiers ? "(?$modifiers)$pattern" : $pattern);
    }
    
    method do_RuleReference($name) {
        return RuleReference->$new(name => $name);
    }
    
    method do_SequenceRule($rule, $op, $sep) {
        return Sequence->$new(
            rule => $rule,
            min => 0+($op =~ /\A[+]/),
            sep => $sep,
        );
    }
}

1;