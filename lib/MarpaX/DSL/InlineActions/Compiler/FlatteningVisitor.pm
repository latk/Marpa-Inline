package MarpaX::DSL::InlineActions::Compiler::FlatteningVisitor;
use strict;
use warnings;

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
        proper => $ast->proper,
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

1;
