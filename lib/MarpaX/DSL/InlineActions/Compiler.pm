package MarpaX::DSL::InlineActions::Compiler;
use strict;
use warnings;
use feature qw< unicode_strings say state >;

use MarpaX::DSL::InlineActions::Compiler::FlatteningVisitor;
use MarpaX::DSL::InlineActions::Compiler::CompilingVisitor;

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
    has proper => (
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

1;
