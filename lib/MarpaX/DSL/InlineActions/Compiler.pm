package MarpaX::DSL::InlineActions::Compiler;
use strict;
use warnings;
use feature qw< unicode_strings say state >;

use Eval::Closure ();
use Marpa::R2 ();

# use Data::Dump ();

sub compile {
    my ($ast) = @_;
    my ($rules, $terminals, $actions) = MarpaX::DSL::InlineActions::Compiler::FlatteningVisitor->new->visit($ast);
#     say "Rules:";
#     say "    $_\t=> ", $rules->{$_}->ast =~ s/\n/\n    /gr for sort keys %$rules;
#     say "Terminals:";
#     say "    $_\t=> ", Data::Dump::pp $terminals->{$_} for sort keys %$terminals;
#     say "Actions:";
#     say "    $_\t=> $actions->{$_}" for sort keys %$actions;
    my $compiling_visitor = MarpaX::DSL::InlineActions::Compiler::CompilingVisitor->new(
        rules => $rules,
        terminals => $terminals,
        actions => $actions,
    );
    $_->accept($compiling_visitor) for values %$rules;
#         Data::Dump::dd $compiling_visitor->compiled_rules;
    my $closures = codegen($actions);
    
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
    
sub codegen {
    my ($actions) = @_;
    state $counter = 0;
    my $package_name = "MarpaX::DSL::InlineActions::ANON_" . ++$counter;
    my $code = qq();
    $code .= qq(  package $package_name;\n);
    $code .= qq(  no strict 'refs';\n);
    $code .= qq(  return +{\n);
    for my $rule (sort keys %$actions) {
        $code .= qq(    "$rule" => sub{ use strict; use warnings; $actions->{$rule}},\n);
    }
    $code .= qq(    "::array" => sub { my (undef, \@parts) = \@_; return \\\@parts },\n);
    $code .= qq(    "::first" => sub { my (undef, \$thing) = \@_; return \$thing },\n);
    $code .= qq(    "::undef" => sub { return },\n);
    $code .= qq(  };);
    
    $code = qq(sub {\n$code\n});
    
#         say $code;
    
    return Eval::Closure::eval_closure source => $code;
}

package MarpaX::DSL::InlineActions::Compiler::Grammar {
    use Moo;
    has marpa_grammar => (
        is => 'ro',
        required => 1,
    );
    has closures => (
        is => 'ro',
        required => 1,
    );
    has terminals => (
        is => 'ro',
        required => 1,
    );
    has rules => (
        is => 'ro',
        required => 1,
    );
    
    sub parse {
        my ($self, $ref) = @_;
        my $pos = 0;
        my $length = length $$ref;
        my $recce = Marpa::R2::Recognizer->new({
            grammar => $self->marpa_grammar,
            ranking_method => 'high_rule_only',
            closures => $self->closures->(),
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
                my $regex = $self->terminals->{$expected};
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
                my @human_readable = map { '' . $self->rules->{$_}->ast } @expected_tokens;
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

package MarpaX::DSL::InlineActions::Compiler::RuleReference {
    use Moo;
    has lhs => (
        is => 'ro',
        required => 1,
    );
    
    sub reference {
        my ($self) = @_;
        return MarpaX::DSL::InlineActions::Compiler::RuleReference->new(
            lhs => $self->lhs,
        );
    }
}

package MarpaX::DSL::InlineActions::Compiler::Terminal {
    use Moo;
    extends 'MarpaX::DSL::InlineActions::Compiler::RuleReference';
    my $counter = 0;
    has ast => (
        is => 'ro',
        required => 1,
    );
    has pattern => (
        is => 'ro',
        required => 1,
    );
    
    sub BUILDARGS {
        my ($class, %args) = @_;
        return { lhs => "Token-" . ++$counter, %args };
    }
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Terminal($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Compiler::Rule {
    use Moo;
    extends 'MarpaX::DSL::InlineActions::Compiler::RuleReference';
    my $counter = 0;
    has ast => (
        is => 'ro',
        required => 1,
    );
    has rhs => (
        is => 'ro',
        default => sub { [] },
    );
    
    sub BUILDARGS {
        my ($class, %args) = @_;
        return { lhs => "Rule-" . ++$counter, %args };
    }
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Rule($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Compiler::SequenceRule {
    use Moo;
    extends 'MarpaX::DSL::InlineActions::Compiler::Rule';
    has min => (
        is => 'ro',
        default => sub { 1 },
    );
    has sep => (
        is => 'ro',
    );
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_SequenceRule($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Compiler::Option {
    use Moo;
    extends 'MarpaX::DSL::InlineActions::Compiler::Rule';
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Option($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Compiler::Maybe {
    use Moo;
    extends 'MarpaX::DSL::InlineActions::Compiler::Rule';
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Maybe($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Compiler::Statement {
    use Moo;
    extends 'MarpaX::DSL::InlineActions::Compiler::RuleReference';
    has ast => (
        is => 'ro',
        required => 1,
    );
    has rhses => (
        is => 'ro',
        required => 1,
    );
    
    my $counter = 0;
    sub BUILDARGS {
        my ($class, %args) = @_;
        return { lhs => "Statement-" . ++$counter, %args };
    }
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Statement($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Compiler::CompilingVisitor {
    use Moo;
    has rules => (
        is => 'ro',
        required => 1,
    );
    has terminals => (
        is => 'ro',
        required => 1,
    );
    has actions => (
        is => 'ro',
        required => 1,
    );
    has compiled_rules => (
        is => 'ro',
        default => sub { [] },
    );
    
    sub visit {
        my ($self, $rule, @args) = @_;
        return $rule->accept($rule, @args);
    }
    
    sub visit_Terminal {
        return;
    }
    
    sub visit_Rule {
        my ($self, $rule) = @_;
        push @{ $self->compiled_rules }, {
            lhs => $rule->lhs,
            rhs => [map { $self->assert_rule_exists($_); $_->lhs } @{$rule->rhs}],
            exists $self->actions->{$rule->lhs} ? (action => $rule->lhs) : (action => '::array'),
        };
        return;
    }
    
    sub visit_Maybe {
        my ($self, $rule) = @_;
        push @{ $self->compiled_rules }, {
            lhs => $rule->lhs,
            rhs => [map { $self->assert_rule_exists($_); $_->lhs } $rule->rhs],
            action => '::first',
        };
        push @{ $self->compiled_rules }, {
            lhs => $rule->lhs,
            rhs => [],
            action => '::undef',
        };
        return;
    }
    
    sub visit_SequenceRule {
        my ($self, $rule) = @_;
        push @{ $self->compiled_rules }, {
            lhs => $rule->lhs,
            rhs => [do { $self->assert_rule_exists($rule->rhs); $rule->rhs->lhs }],
            min => $rule->min,
            separator => do { $self->assert_rule_exists($rule->sep); $rule->sep->lhs },
            exists $self->actions->{$rule->lhs} ? (action => $rule->lhs) : (action => '::array'),
        };
        return;
    }
    
    sub visit_Option {
        my ($self, $rule) = @_;
        return; # the processing happens in visit_Statement
    }
    
    sub visit_Statement {
        my ($self, $rule) = @_;
        my @options = @{ $rule->rhses };
        for my $i (0 .. $#options) {
            my $option = $self->rules->{$options[$i]->lhs};
            push @{ $self->compiled_rules }, {
                lhs => $rule->lhs,
                rhs => [map { $self->assert_rule_exists($_); $_->lhs } @{$option->rhs}],
                rank => $#options - $i,
                exists $self->actions->{$option->lhs} ? (action => $option->lhs) : (action => '::array'),
            };
        }
        return;
    }
    
    sub assert_rule_exists {
        my ($self, $rule) = @_;
        my $name = $rule->lhs;
        if (not exists $self->rules->{$name}) {
            die qq(No rule with the name "$name" was defined);
        }
        return;
    }
}

package MarpaX::DSL::InlineActions::Compiler::FlatteningVisitor {
    use Moo;
    has rules => (
        is => 'ro',
        default => sub { +{} },
    );
    has terminals => (
        is => 'ro',
        default => sub { +{} },
    );
    has actions => (
        is => 'ro',
        default => sub { +{} },
    );
    
    sub visit {
        my ($self, $ast, @args) = @_;
        return $ast->accept($self, @args);
    }
    
    sub visit_String {
        my ($self, $ast) = @_;
        my $pattern = quotemeta $ast->value;
        my $rule = MarpaX::DSL::InlineActions::Compiler::Terminal->new(
            ast => $ast,
            pattern => qr/\G($pattern)/,
        );
        $self->register_terminal($rule->lhs, $rule);
        return $rule->reference;
    }
    
    sub visit_Regex {
        my ($self, $ast) = @_;
        my $pattern = $ast->value;
        my $rule = MarpaX::DSL::InlineActions::Compiler::Terminal->new(
            ast => $ast,
            pattern => qr/\G($pattern)/,
        );
        $self->register_terminal($rule->lhs, $rule);
        return $rule->reference;
    }
    
    sub visit_RuleReference {
        my ($self, $ast) = @_;
        my $rule = MarpaX::DSL::InlineActions::Compiler::RuleReference->new(
            lhs => $ast->name,
        );
        return $rule;
    }
    
    sub visit_Maybe {
        my ($self, $ast) = @_;
        my $rhs = $ast->rule->accept($self);
        my $rule = MarpaX::DSL::InlineActions::Compiler::Maybe->new(
            ast => $ast,
            lhs => $rhs->lhs . "?",
            rhs => $rhs,
        );
        $self->register_rule($rule->lhs, $rule);
        return $rule->reference;
    }
    
    sub visit_Sequence {
        my ($self, $ast) = @_;
        my $rule = MarpaX::DSL::InlineActions::Compiler::SequenceRule->new(
            ast => $ast,
            rhs => $ast->rule->accept($self),
            min => $ast->min,
            $ast->sep ? (sep => $ast->sep->accept($self)) : (),
        );
        $self->register_rule($rule->lhs, $rule);
        return $rule->reference;
    }
    
    sub visit_Option {
        my ($self, $ast) = @_;
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
    
    sub visit_Statement {
        my ($self, $ast) = @_;
        my $rule = MarpaX::DSL::InlineActions::Compiler::Statement->new(
            defined $ast->name ? (lhs => $ast->name) : (),
            ast => $ast,
            rhses => [map { $_->accept($self) } @{ $ast->options }],
        );
        $self->register_rule($rule->lhs, $rule);
        return $rule->reference;
    }
    
    sub visit_Grammar {
        my ($self, $ast) = @_;
        $_->accept($self) for @{ $ast->statements };
        return $self->rules, $self->terminals, $self->actions;
    }
    
    sub register_rule {
        my ($self, $name, $rule) = @_;
        if (exists $self->rules->{$name}) {
            die qq(Rule "$name" was already defined);
        }
        $self->rules->{$name} = $rule;
        return;
    }
    
    sub register_terminal {
        my ($self, $name, $terminal) = @_;
        if (exists $self->terminals->{$name}) {
            die qq(Terminal "$name" was already defined);
        }
        $self->terminals->{$name} = $terminal->pattern;
        $self->register_rule($terminal->lhs, $terminal);
        return;
    }
    
    sub register_action {
        my ($self, $name, $action, @captures) = @_;
        my $arglist = join ", " => '$self', @captures;
        die qq(Action "$name" already exists) if exists $self->actions->{$name};
        $self->actions->{$name} = "my ($arglist) = \@_;$action";
    }
}

1;
