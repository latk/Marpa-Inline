package MarpaX::DSL::InlineActions;

use strict;
use warnings;
use utf8;
use feature qw< unicode_strings say >;
use Marpa::R2;
use String::Escape ();

use MarpaX::DSL::InlineActions::Ast;
use MarpaX::DSL::InlineActions::Compiler;

my $grammar_source = <<'END_GRAMMAR';
:default ::= action => [values]
:start ::= Grammar

Grammar
    ::= (_) Statements (_) action => do_Grammar
Statements
    ::= Statement+ separator => __
Statement
    ::= name (_ ':' _) Options action => do_Statement
InnerStatement
    ::= Statement       action => ::first
    ||  Null Options    action => do_Statement
Options
    ::= Option+
Option
    ::= (OpOption) Patterns (_ '=>' _) code action => do_Option
OpOption
    ::= _
    ||  _ '||' _
    
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
    ||  ('(' _) InnerStatement (_ ')')   action => ::first
    ||  Rule sequenceQuantifier (_ '%') '!'  (_) Rule    action => do_SequenceRule # a,a,a
    ||  Rule sequenceQuantifier (_ '%') Null (_) Rule    action => do_SequenceRule # a,a,a,
    ||  Rule sequenceQuantifier                         action => do_SequenceRule # aaa
    ||  Rule ('?')              action => do_Maybe
sequenceQuantifier ~  '+' | '*'
    
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
_ ::= _ comment _
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

#     say "$ast";

    return MarpaX::DSL::InlineActions::Compiler::compile($ast);
}

package MarpaX::DSL::InlineActions::Actions {

    my $new = sub {
        my ($class, @args) = @_;
        "MarpaX::DSL::InlineActions::Ast::$class"->new(@args);
    };

    sub do_Grammar {
        my ($self, $statements) = @_;
        return Grammar->$new(statements => $statements);
    }

    sub do_Statement {
        my ($self, $name, $options) = @_;
        return Statement->$new(name => $name, options => $options);
    }
    
    sub do_Option {
        my ($self, $pattern, $action) = @_;
        $action =~ s/\A\{\{//;
        $action =~ s/\}\}\z//;
        return Option->$new(pattern => $pattern, action => $action);
    }
    
    sub do_Pattern {
        my ($self, $name, $rule) = @_;
        return NamedPattern->$new(name => $name, rule => $rule) if defined $name;
        return Pattern->$new(rule => $rule);
    }
    
    sub do_stringDq {
        my ($self, $str) = @_;
        return String->$new(value => String::Escape::unqqbackslash $str);
    }
    
    sub do_stringSq {
        my ($self, $str) = @_;
        return String->$new(value => String::Escape::unbackslash String::Escape::unsinglequote $str);
    }
    
    sub do_Regex {
        my ($self, $pattern, $modifiers) = @_;
        $pattern =~ s/\A[mr].//;
        $pattern =~ s/.\z//;
        return Regex->$new(value => length $modifiers ? "(?$modifiers)$pattern" : $pattern);
    }
    
    sub do_RuleReference {
        my ($self, $name) = @_;
        return RuleReference->$new(name => $name);
    }
    
    sub do_SequenceRule {
        my ($self, $rule, $op, $exact, $sep) = @_;
        return Sequence->$new(
            rule => $rule,
            min => 0+($op eq '+'),
            sep => $sep,
            proper => 0+!!$exact,
        );
    }
    
    sub do_Maybe {
        my ($self, $rule) = @_;
        return Maybe->$new(
            rule => $rule,
        );
    }
}

1;