package Perinci::CmdLine::Base;

use 5.010;

use Mo qw'build default';

has url => ();
has summary => ();
has description => ();
has program_name => (
    default => sub {
        my $pn = $ENV{PERINCI_CMDLINE_PROGRAM_NAME};
        if (!defined($pn)) {
            $pn = $0; $pn =~ s!.+/!!;
        }
        $pn;
    });
has subcommands => ();
has default_subcommand => ();
has pass_cmdline_object => (default=>0);
has format => (default=>'text');
has common_opts => ();

# role:
# requires 'run'

sub get_subcommand {
    my ($self, $name) = @_;

    my $scs = $self->subcommands;
    return undef unless $scs;

    if (reftype($scs) eq 'CODE') {
        return $scs->($self, name=>$name);
    } else {
        return $scs->{$name};
    }
}

sub list_subcommands {
    my ($self) = @_;
    state $cached;
    return $cached if $cached;

    my $scs = $self->subcommands;
    my $res;
    if ($scs) {
        if (reftype($scs) eq 'CODE') {
            $scs = $scs->($self);
            $self->_err("Subcommands code didn't return a hashref")
                unless ref($scs) eq 'HASH';
        }
        $res = $scs;
    } else {
        $res = {};
    }
    $cached = $res;
}

sub get_meta {
}

1;
# ABSTRACT: Base class for Perinci::CmdLine{,::Lite}

=for Pod::Coverage ^(.+)$

