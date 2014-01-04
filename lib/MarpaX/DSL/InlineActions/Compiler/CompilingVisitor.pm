package MarpaX::DSL::InlineActions::Compiler::CompilingVisitor;
use strict;
use warnings;

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
        $rule->sep ? (separator => do { $self->assert_rule_exists($rule->sep); $rule->sep->lhs }) : (),
        proper => $rule->proper,
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

1;