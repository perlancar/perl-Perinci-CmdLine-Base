package Perinci::CmdLine::Base;

# DATE
# VERSION

use 5.010001;

# this class can actually be a role instead of base class for pericmd &
# pericmd-lite, but Mo is more lightweight than Role::Tiny (also R::T doesn't
# have attributes), Role::Basic, or Moo::Role.

use Mo qw'build default';

has actions => ();
has common_opts => ();
has default_subcommand => ();
has description => ();
has exit => (is=>'rw', default=>1);
has selected_format => ();
has formats => ();
has pass_cmdline_object => (default=>0);
has program_name => (
    default => sub {
        my $pn = $ENV{PERINCI_CMDLINE_PROGRAM_NAME};
        if (!defined($pn)) {
            $pn = $0; $pn =~ s!.+/!!;
        }
        $pn;
    });
has selected_action => (is => 'rw');
has selected_subcommand_data => (is => 'rw');
has selected_subcommand_name => (is => 'rw');
has subcommands => ();
has summary => ();
has tags => ();
has url => ();

# role: requires 'get_meta' # ($url)
# to actually implement getting function metadata

# role: requires 'format_result' # ($res, $format, $meta)
#
# role: requires 'display_result' # ($res, $fres)

# role: requires 'hook_before_run' # ()

# role: requires 'hook_after_run' # ()

# role: requires 'hook_after_parse_opts' # ($parse_res)

# role: requires 'hook_after_select_subcommand' ($name)

sub get_program_and_subcommand_name {
    my $self = shift;
    my $res = $self->program_name . " " .
        ($self->selected_subcommand_name // "");
    $res =~ s/\s+$//;
    $res;
}

sub get_subcommand_data {
    my ($self, $name) = @_;

    my $scs = $self->subcommands;
    return undef unless $scs;

    if (ref($scs) eq 'CODE') {
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

# if argument is undef, it means we're selecting the primary url (+ summary,
# ...). selected_subcommand_name will be set to '' (empty string).
sub select_subcommand {
    my ($self, $name) = @_;

    my $scd;
    if (defined $name) {
        $scd = $self->get_subcommand_data($name);
        die [500, "No such subcommand: $name"] unless $scd;
    } else {
        $scd = {
            url => $self->url,
            summary => $self->summary,
            description => $self->description,
            pass_cmdline_object => $self->pass_cmdline_object,
            tags => $self->tags,
        };
    }

    $self->selected_subcommand_name($name // '');
    $self->selected_subcommand_data($scd);
    $self->hook_after_select_subcommand($name);
    $scd;
}

sub status2exitcode {
    my ($self, $status) = @_;
    return 0 if $status == 200;
    $status - 300;
}

# XXX
sub do_completion {
    my $self = shift;

    # @ARGV given by bash is messed up / different, we get words from parsing,
    # COMP_LINE/COMP_POINT. this might not be the case with other shells like
    # zsh/fish. XXX detect running shell.
    require Complete::Bash;
    my ($words, $cword) = Complete::Bash::parse_cmdline();

}

sub _get_subcommand_name_from_argv {
    require Getopt::Long;

    my $self = shift;

    # we want to allow common options to be inserted before subcommand name,
    # e.g. 'cmd --help subcmd', so we parse it out first
    my @orig_argv = @ARGV;
    my $old_conf = Getopt::Long::Configure(qw/bundling no_permute ignore_case
                                              pass_through/);
    my @go;
    my $co = $self->common_opts // {};
    for (keys %$co) {
        push @go, $co->{$_}{getopt} => sub{};
    }
    Getopt::Long::GetOptions(@go);

    my $scn;
    if (@ARGV) {
        $scn = shift @ARGV;
        # restore options in front of subcommand name that were eaten by
        # Getopt::Long
        splice @ARGV, 0, 0, @orig_argv[0..(@orig_argv - @ARGV - 2)];
    } else {
        @ARGV = @orig_argv;
    }
    use DD; dd \@ARGV;

    Getopt::Long::Configure($old_conf);
    $scn;
}

sub parse_opts {
    my ($self) = @_;

    # first we get subcommand name
    {
        my $scn;
        if ($self->subcommands && @ARGV) {
            $scn = $self->_get_subcommand_name_from_argv;
        }
        $self->select_subcommand($scn);
    }

    my %args;

    # also set dry-run on environment
    do { $self->{_dry_run} = 1; $ENV{VERBOSE} = 1 } if $ENV{DRY_RUN};

    my $scd = $self->selected_subcommand_data;
    my $meta = $self->get_meta($scd->{url});

    # parse argv
    my $co = $self->common_opts;
    require Perinci::Sub::GetArgs::Argv;
    my $res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(
        argv                => \@ARGV,
        args                => $scd->{args} ? { %{$scd->{args}} } : undef,
        meta                => $meta,
        allow_extra_elems   => 0,
        per_arg_json        => 1,
        per_arg_yaml        => 1,
        common_opts         => { map {$co->{$_}{getopt} => $co->{$_}{handler}}
                                     keys %$co },
        on_missing_required_args => sub {
            my %a = @_;
            my ($an, $aa, $as) = ($a{arg}, $a{args}, $a{spec});
            my $src = $as->{cmdline_src};
            if ($src && $as->{req}) {
                # don't complain, we will fill argument from other source
                return 1;
            } else {
                # we have no other sources, so we complain about missing arg
                return 0;
            }
        },
    );
    die $res if $res->[0] != 200;
    $self->hook_after_parse_opts($res);
    $res;
}

sub run {
    my ($self) = @_;

    my ($res, $fres, $meta);
    eval {
        $self->selected_action(undef);
        $self->selected_format(undef);
        $self->selected_subcommand_data(undef);
        $self->selected_subcommand_name(undef);
        $self->hook_before_run;

        # completion is special case, we exit early
        if ($ENV{COMP_LINE}) {
            die do_completion();
        }

        $self->parse_opts;

        my $action = $self->selected_action;
        my $meth = "run_$action";
        die [500, "Unknown action $action"] unless $self->can($meth);

        $res = $self->$meth;
        $fres = $self->format_result($res, $self->selected_format, $meta);
    };
    my $err = $@;
    if ($err) {
        $err = [500, "Died: $err"] unless ref($err) eq 'ARRAY';
        $res = $err;
        # format error again
        unless (defined $fres) {
            $fres = $self->format_result($res, 'text', $meta);
        }
    }
    $self->display_result($res, $fres);

  L1:
    $self->hook_after_run;
    if ($self->exit) {
        exit $self->status2exitcode($res->[0]);
    } else {
        return $res;
    }
}

1;
# ABSTRACT: Base class for Perinci::CmdLine{,::Lite}

=for Pod::Coverage ^(.+)$
