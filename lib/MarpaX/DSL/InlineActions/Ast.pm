package MarpaX::DSL::InlineActions::Ast;
use strict;
use warnings;
use feature qw< unicode_strings say >;
use Carp ();
use Scalar::Util ();
use String::Escape ();

use Moo::Role;
requires 'accept'; # ($visitor, @args)

package MarpaX::DSL::InlineActions::Ast::Grammar {
    use Moo;
    with 'MarpaX::DSL::InlineActions::Ast';
    has statements => (
        is => 'ro',
        required => 1,
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        return join "\n" => @{ $self->statements };
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Grammar($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Ast::Statement {
    use Moo;
    with 'MarpaX::DSL::InlineActions::Ast';
    has name => (
        is => 'ro',
        requires => 1,
    );
    has options => (
        is => 'ro',
        required => 1,
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        return $self->name . "\n    :=  " . join "\n    ||  " => @{ $self->options };
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Statement($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Ast::Option {
    use Moo;
    with 'MarpaX::DSL::InlineActions::Ast';
    has pattern => (
        is => 'ro',
        required => 1,
    );
    has action => (
        is => 'ro',
        required => 1,
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        my $pat = join " " => @{ $self->pattern };
        return "$pat {{" . $self->action . "}}";
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Option($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Ast::Pattern {
    use Moo;
    with 'MarpaX::DSL::InlineActions::Ast';
    has rule => (
        is => 'ro',
        required => 1,
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        return '' . $self->rule;
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        die "This sub shouldn't be called";
    }
}

package MarpaX::DSL::InlineActions::Ast::NamedPattern {
    use Moo;
    extends 'MarpaX::DSL::InlineActions::Ast::Pattern';
    has name => (
        is => 'ro',
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        return $self->name . '=' . $self->rule;
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        die "This sub shouldn't be called";
    }
}

package MarpaX::DSL::InlineActions::Ast::Rule {
    use Moo::Role;
    with 'MarpaX::DSL::InlineActions::Ast';
}

package MarpaX::DSL::InlineActions::Ast::String {
    use Moo;
    with 'MarpaX::DSL::InlineActions::Ast::Rule';
    has value => (
        is => 'ro',
        required => 1,
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        return String::Escape::qqbackslash $self->value;
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_String($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Ast::Regex {
    use Moo;
    with 'MarpaX::DSL::InlineActions::Ast::Rule';
    has value => (
        is => 'ro',
        required => 1,
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        my $value = $self->value;
        return 'm/' . qr/$value/ . '/';
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Regex($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Ast::RuleReference {
    use Moo;
    with 'MarpaX::DSL::InlineActions::Ast::Rule';
    has name => (
        is => 'ro',
        required => 1,
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        return $self->name;
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_RuleReference($self, @args);
    }
}

package MarpaX::DSL::InlineActions::Ast::Sequence {
    use Moo;
    with 'MarpaX::DSL::InlineActions::Ast::Rule';
    has rule => (
        is => 'ro',
        required => 1,
    );
    has min => (
        is => 'ro',
        default => sub { 1 },
    );
    has sep => (
        is => 'ro',
    );
    
    use overload '""' => sub {
        my ($self) = @_;
        my $rule = $self->rule;
        my $op = $self->min ? '+' : '*';
        my $sep = $self->sep;
        return "$rule $op% $sep" if $sep;
        return "$rule$op";
    };
    
    sub accept {
        my ($self, $visitor, @args) = @_;
        return $visitor->visit_Sequence($self, @args);
    }
}

1;