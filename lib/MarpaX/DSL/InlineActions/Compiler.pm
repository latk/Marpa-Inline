package MarpaX::DSL::InlineActions::Compiler;
use strict;
use warnings;
use feature qw< unicode_strings say state >;
use mop;

use Data::Dump ();
use Eval::Closure ();
use Marpa::R2 ();

class MarpaX::DSL::InlineActions::Compiler {
    
    method compile($ast) {
        my ($rules, $terminals, $actions) = MarpaX::DSL::InlineActions::Compiler::FlatteningVisitor->new->visit($ast);
#         say "Rules:";
#         say "    $_\t=> ", $rules->{$_}->ast =~ s/\n/\n    /gr for sort keys %$rules;
#         say "Terminals:";
#         say "    $_\t=> ", Data::Dump::pp $terminals->{$_} for sort keys %$terminals;
#         say "Actions:";
#         say "    $_\t=> $actions->{$_}" for sort keys %$actions;
        my $compiling_visitor = MarpaX::DSL::InlineActions::Compiler::CompilingVisitor->new(
            rules => $rules,
            terminals => $terminals,
            actions => $actions,
        );
        $_->accept($compiling_visitor) for values %$rules;
#         Data::Dump::dd $compiling_visitor->compiled_rules;
        my $closures = $self->codegen($actions);
        
        return MarpaX::DSL::InlineActions::Compiler::Grammar->new(
            marpa_grammar => Marpa::R2::Grammar->new({
                start => 'TOP',
                rules => $compiling_visitor->compiled_rules,
                terminals => [keys %$terminals],
            })->precompute,
            closures => $closures,
            terminals => $terminals,
            rules => $rules,
        );
    }
    
    method codegen($actions) {
        state $counter = 0;
        my $package_name = "MarpaX::DSL::InlineActions::ANON_" . ++$counter;
        my $code = qq();
        $code .= qq(  package $package_name;\n);
        $code .= qq(  no strict 'refs';\n);
        $code .= qq(  return +{\n);
        for my $rule (sort keys %$actions) {
            $code .= qq(    "$rule" => sub{ use strict; use warnings; $actions->{$rule}},\n);
        }
        $code .= qq(    "::array" => sub { my (\$self, \@parts) = \@_; return \\\@parts }\n);
        $code .= qq(  };);
        
        $code = qq(sub {\n$code\n});
        
#         say $code;
        
        return Eval::Closure::eval_closure source => $code;
    }
}

class Grammar {
    has $!marpa_grammar is ro = die q("marpa_grammar" required);
    has $!closures is ro = die q("closures required");
    has $!terminals is ro = die q("terminals" required);
    has $!rules is ro = die q("rules" required);
    
    method parse($ref) {
        my $pos = 0;
        my $length = length $$ref;
        my $recce = Marpa::R2::Recognizer->new({
            grammar => $!marpa_grammar,
            ranking_method => 'high_rule_only',
            closures => $!closures->(),
        });
        
        POSITION:
        while ($pos < $length) {
            my $result_len = 0;
            my @result;
            
            my @expected_tokens = @{ $recce->terminals_expected };
            if (not @expected_tokens) {
                die "No tokens are being expected here... which is wrong";
            }
            
            TERMINAL:
            for my $expected (@expected_tokens) {
                my $regex = $!terminals->{$expected};
                pos($$ref) = $pos;
                next TERMINAL unless $$ref =~ $regex;
                my $result = $1;
                if (length $result >= $result_len) {
                    if (length $result > $result_len) {
                        $result_len = length $result;
                        @result = ();
                    }
                    push @result, [$expected => \$result];
                }
            }
            
            if (not @result) {
                my @human_readable = map { '' . $!rules->{$_}->ast } @expected_tokens;
                die "Expected any of the following tokens:\n", (join "\n" => @human_readable), "\n";
            }
            for my $token (@result) {
                $recce->alternative(@$token);
            }
            $recce->earleme_complete;
            
            $pos += $result_len;
        }
        
        $recce->end_input;
        my $ret = $recce->value // die "No parse";
        return $$ret;
    }
}

class RuleReference {
    has $!lhs is ro = Carp::confess q("lhs" required);
    
    method reference {
        return MarpaX::DSL::InlineActions::Compiler::RuleReference->new(
            lhs => $self->lhs,
        );
    }
}

class Terminal extends MarpaX::DSL::InlineActions::Compiler::RuleReference {
    my $counter = 0;
    has $!ast is ro = die q("ast" required);
    has $!pattern is ro = die q("pattern" required);
    
    method new($class: %args) {
        return $class->next::method(lhs => "Token-" . ++$counter, %args);
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_Terminal($self, @args);
    }
}

class Rule extends MarpaX::DSL::InlineActions::Compiler::RuleReference {
    my $counter = 0;
    has $!ast is ro = die q("ast" required);
    has $!rhs is ro = [];
    
    method new($class: %args) {
        return $class->next::method(lhs => "Rule-" . ++$counter, %args);
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_Rule($self, @args);
    }
}

class SequenceRule extends MarpaX::DSL::InlineActions::Compiler::Rule {
    has $!min is ro = 1;
    has $!sep is ro;
    
    method accept($visitor, @args) {
        return $visitor->visit_SequenceRule($self, @args);
    }
}

class Option extends MarpaX::DSL::InlineActions::Compiler::Rule {
    method accept($visitor, @args) {
        return $visitor->visit_Option($self, @args);
    }
}

class Statement extends MarpaX::DSL::InlineActions::Compiler::RuleReference {
    has $!ast is ro = die q("ast" required);
    has $!rhses is ro = die q("rhses" required);
    
    method accept($visitor, @args) {
        return $visitor->visit_Statement($self, @args);
    }
}

class CompilingVisitor {
    has $!rules = die q("rules" required);
    has $!terminals = die q("terminals" required);
    has $!actions = die q("actions" required);
    has $!compiled_rules is ro = [];
    
    method visit($rule, @args) {
        return $rule->accept($rule, @args);
    }
    
    method visit_Terminal($rule) {
        return;
    }
    
    method visit_Rule($rule) {
        push @{ $!compiled_rules }, {
            lhs => $rule->lhs,
            rhs => [map { $self->assert_rule_exists($_); $_->lhs } @{$rule->rhs}],
            exists $!actions->{$rule->lhs} ? (action => $rule->lhs) : (action => '::array'),
        };
        return;
    }
    
    method visit_SequenceRule($rule) {
        push @{ $!compiled_rules }, {
            lhs => $rule->lhs,
            rhs => [do { $self->assert_rule_exists($rule->rhs); $rule->rhs->lhs }],
            min => $rule->min,
            separator => do { $self->assert_rule_exists($rule->sep); $rule->sep->lhs },
            exists $!actions->{$rule->lhs} ? (action => $rule->lhs) : (action => '::array'),
        };
        return;
    }
    
    method visit_Option($rule) {
        return; # the processing happens in visit_Statement
    }
    
    method visit_Statement($rule) {
        my @options = @{ $rule->rhses };
        for my $i (0 .. $#options) {
            my $option = $!rules->{$options[$i]->lhs};
            push @{ $!compiled_rules }, {
                lhs => $rule->lhs,
                rhs => [map { $self->assert_rule_exists($_); $_->lhs } @{$option->rhs}],
                rank => $#options - $i,
                exists $!actions->{$option->lhs} ? (action => $option->lhs) : (action => '::array'),
            };
        }
        return;
    }
    
    method assert_rule_exists($rule) {
        my $name = $rule->lhs;
        if (not exists $!rules->{$name}) {
            die qq(No rule with the name "$name" was defined);
        }
        return;
    }
}

class FlatteningVisitor {
    has $!rules = {};
    has $!terminals = {};
    has $!actions = {};
    
    method visit($ast, @args) {
        return $ast->accept($self, @args);
    }
    
    method visit_String($ast) {
        my $pattern = quotemeta $ast->value;
        my $rule = MarpaX::DSL::InlineActions::Compiler::Terminal->new(
            ast => $ast,
            pattern => qr/\G($pattern)/,
        );
        $self->register_terminal($rule->lhs, $rule);
        return $rule->reference;
    }
    
    method visit_Regex($ast) {
        my $pattern = $ast->value;
        my $rule = MarpaX::DSL::InlineActions::Compiler::Terminal->new(
            ast => $ast,
            pattern => qr/\G($pattern)/,
        );
        $self->register_terminal($rule->lhs, $rule);
        return $rule->reference;
    }
    
    method visit_RuleReference($ast) {
        my $rule = MarpaX::DSL::InlineActions::Compiler::RuleReference->new(
            lhs => $ast->name,
        );
        return $rule;
    }
    
    method visit_Sequence($ast) {
        my $rule = MarpaX::DSL::InlineActions::Compiler::SequenceRule->new(
            ast => $ast,
            rhs => $ast->rule->accept($self),
            min => $ast->min,
            $ast->sep ? (sep => $ast->sep->accept($self)) : (),
        );
        $self->register_rule($rule->lhs, $rule);
        return $rule->reference;
    }
    
    method visit_Option($ast) {
        my @captures;
        my @rules;
        for my $pattern (@{ $ast->pattern }) {
            push @captures, $pattern->can('name') ? $pattern->name : "undef";
            push @rules, $pattern->rule->accept($self)
        }
        my $rule = MarpaX::DSL::InlineActions::Compiler::Option->new(
            ast => $ast,
            rhs => \@rules,
        );
        $self->register_rule($rule->lhs, $rule);
        $self->register_action($rule->lhs, $ast->action, @captures);
        return $rule->reference;
    }
    
    method visit_Statement($ast) {
        my $rule = MarpaX::DSL::InlineActions::Compiler::Statement->new(
            lhs => $ast->name,
            ast => $ast,
            rhses => [map { $_->accept($self) } @{ $ast->options }],
        );
        $self->register_rule($rule->lhs, $rule);
        return $rule->reference;
    }
    
    method visit_Grammar($ast) {
        $_->accept($self) for @{ $ast->statements };
        return $!rules, $!terminals, $!actions;
    }
    
    method register_rule($name, $rule) {
        if (exists $!rules->{$name}) {
            die qq(Rule "$name" was already defined);
        }
        $!rules->{$name} = $rule;
        return;
    }
    
    method register_terminal($name, $terminal) {
        my $name = $terminal->lhs;
        if (exists $!terminals->{$name}) {
            die qq(Terminal "$name" was already defined);
        }
        $!terminals->{$name} = $terminal->pattern;
        $self->register_rule($terminal->lhs, $terminal);
        return;
    }
    
    method register_action($name, $action, @captures) {
        my $arglist = join ", " => '$self', @captures;
        die qq(Action "$name" already exists) if exists $!actions->{$name};
        $!actions->{$name} = "my ($arglist) = \@_;$action";
    }
}

1;
