package MarpaX::DSL::InlineActions::Ast;
use strict;
use warnings;
use feature qw< unicode_strings say >;
use mop;
use Carp ();
use Scalar::Util ();
use String::Escape ();

role MarpaX::DSL::InlineActions::Ast {
    method as_string;
    method accept; # ($visitor, @args)
}

class Grammar with MarpaX::DSL::InlineActions::Ast {
    has $!statements is ro = die q("statements" required);
    
    method as_string is overload('""') {
        return join "\n" => @{ $!statements };
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_Grammar($self, @args);
    }
}

class Statement with MarpaX::DSL::InlineActions::Ast {
    has $!name is ro    = die q("name" required);
    has $!options is ro = die q("options" required);
    
    method as_string is overload('""') {
        return "$!name\n    :=  " . join "\n    ||  " => @{ $!options };
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_Statement($self, @args);
    }
}

class Option with MarpaX::DSL::InlineActions::Ast {
    has $!pattern is ro = die q("pattern" required);
    has $!action  is ro = die q("action" required);
    
    method as_string is overload('""') {
        my $pat = join " " => @{ $!pattern };
        return "$pat {{$!action}}";
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_Option($self, @args);
    }
}

class Pattern with MarpaX::DSL::InlineActions::Ast {
    has $!rule is ro = die q("rule" required);
    
    method as_string is overload('""') {
        return ''.$!rule;
    }
    
    method accept($visitor, @args) {
        die "This method shouldn't be called";
    }
}

class NamedPattern extends MarpaX::DSL::InlineActions::Ast::Pattern {
    has $!name is ro;
    
    method as_string is overload('""') {
        return $!name . '=' . $self->rule;
    }
    
    method accept($visitor, @args) {
        die "This method shouldn't be called";
    }
}

class Rule with MarpaX::DSL::InlineActions::Ast is abstract {
    
}

class String extends MarpaX::DSL::InlineActions::Ast::Rule {
    has $!value is ro = die q("value" required);
    
    method as_string is overload('""') {
        return String::Escape::qqbackslash $!value;
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_String($self, @args);
    }
}

class Regex extends MarpaX::DSL::InlineActions::Ast::Rule {
    has $!value is ro = die q("value" required);
    
    method as_string is overload('""') {
        return 'm/' . qr/$!value/ . '/';
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_Regex($self, @args);
    }
}

class RuleReference extends MarpaX::DSL::InlineActions::Ast::Rule {
    has $!name is ro = die q("name" required);
    
    method as_string is overload('""') {
        return $!name;
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_RuleReference($self, @args);
    }
}

class Sequence extends MarpaX::DSL::InlineActions::Ast::Rule {
    has $!rule is ro = die q("rule" required);
    has $!min  is ro = 1;
    has $!sep  is ro;
    
    method as_string is overload('""') {
        my $op = $!min ? '+' : '*';
        return "$!rule $op% $!sep" if $!sep;
        return "$!rule$op";
    }
    
    method accept($visitor, @args) {
        return $visitor->visit_Sequence($self, @args);
    }
}

1;