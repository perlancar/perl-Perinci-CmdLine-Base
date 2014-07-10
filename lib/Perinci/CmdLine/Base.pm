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

sub BUILD {
    my ($self, $args) = @_;

    # set default common_opts
    if (!$self->{common_opts}) {
        $self->{common_opts} = {
            version => {
                getopt  => 'version|v',
                summary => 'Show program version',
                handler => sub { run_version() },
            },
            help => {
                getopt  => 'help|h|?',
                summary => 'Show help message',
                handler => sub { $self->{_action} = 'help' },
            },
            format => {
                getopt  => 'format=s',
                summary => 'Set output format (text/text-simple/text-pretty/json/json-pretty)',
                handler => sub { $self->{format} = $_[1] },
            },
            json => {
                getopt  => 'json',
                summary => 'Set output format to json',
                handler => sub { $self->{format} = 'json' },
            },
            subcommands => {
                getopt  => 'json',
                summary => 'Set output format to json',
                handler => sub { $self->{_action} = 'json' },
            },
        };
    }
}

sub get_meta {
}

1;
# ABSTRACT: Base class for Perinci::CmdLine{,::Lite}

=for Pod::Coverage ^(.+)$

