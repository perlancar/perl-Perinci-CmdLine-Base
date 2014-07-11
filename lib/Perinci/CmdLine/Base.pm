package Perinci::CmdLine::Base;

use 5.010;

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
has url => ();

# role: requires '_get_meta' # ($url)
# role: requires 'format_result' # ($res, $format, $meta)
# role: requires 'display_result' # ($res, $fres)
# role: requires 'hook_before_run' # ()
# role: requires 'hook_after_run' # ()
# role: requires 'hook_after_get_meta' # ($meta)

sub program_and_subcommand_name {
    my $self = shift;
    my $res = $self->program_name . " " .
        ($self->selected_subcommand_name // "");
    $res =~ s/\s+$//;
    $res;
}

sub get_meta {
    my ($self, $url) = @_;
    my $meta = $self->_get_meta($url);
    $self->hook_after_get_meta($meta);
    $meta;
}

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

sub select_subcommand {
    my ($self, $name) = @_;

    my $scd = $self->get_subcommand($name);
    die [500, "No such subcommand: $name"] unless $scd;
    $self->selected_subcommand_name($name);
    $self->selected_subcommand_data($scd);
    $scd;
}

sub status2exitcode {
    my ($self, $status) = @_;
    return 0 if $status == 200;
    $status - 300;
}

sub do_completion {
    my $self = shift;

    # @ARGV given by bash is messed up / different, we get words from parsing,
    # COMP_LINE/COMP_POINT. this might not be the case with other shells like
    # zsh/fish. XXX detect running shell.
    require Complete::Bash;
    my ($words, $cword) = Complete::Bash::parse_cmdline();

}

sub parse_opts {
    require Perinci::Sub::GetArgs::Argv;

    #$log->tracef("-> parse_opts()");
    my ($self) = @_;

    # copy original argv before parsing, this is used e.g. in undo history list
    $self->{_orig_argv} = [@ARGV];

    my $sc = $self->{_subcommand};
    return unless $sc && $sc->{url};
    #$log->tracef("-> parse_subcommand_opts()");

    my $res = $self->_pa->request(meta=>$sc->{url});
    if ($res->[0] == 200) {
        # prefill arguments using 'args' from subcommand specification, if any
        $self->{_args} = {};
        if ($sc->{args}) {
            for (keys %{ $sc->{args} }) {
                $self->{_args}{$_} = $sc->{args}{$_};
            }
        }
    } else {
        #$log->warnf("Can't get metadata from %s: %d - %s", $sc->{url},
        #            $res->[0], $res->[1]);
        #$log->tracef("<- parse_subcommand_opts() (bailed)");
        return;
    }
    my $meta = $res->[2];
    $self->{_meta} = $meta;
    $self->_add_common_opts_after_meta;

    # also set dry-run on environment
    do { $self->{_dry_run} = 1; $ENV{VERBOSE} = 1 } if $ENV{DRY_RUN};

    # parse argv
    my $src_seen;
    my %ga_args = (
        argv                => \@ARGV,
        meta                => $meta,
        allow_extra_elems   => 1,
        per_arg_json        => 1,
        per_arg_yaml        => 1,
        on_missing_required_args => sub {
            my %a = @_;
            my ($an, $aa, $as) = ($a{arg}, $a{args}, $a{spec});
            my $src = $as->{cmdline_src};
            if ($src && $as->{req}) {
                # don't complain, we will fill argument from other source
                return 1;
            } else {
                # we have no other sources, so we complain about missing arg
                say "Missing required argument: $an"
                    if $self->{_check_required_args} // 1;
            }
            0;
        },
    );
    $res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(%ga_args);

    # We load Log::Any::App rather late here, to be able to customize level via
    # --debug, --dry-run, etc.
    unless ($ENV{COMP_LINE}) {
        my $do_log = $self->{_subcommand}{log_any_app};
        $do_log //= $ENV{LOG};
        $do_log //= $self->{action_metadata}{$self->{_actions}[0]}{default_log}
            if @{ $self->{_actions} };
        $do_log //= $self->log_any_app;
        $self->_load_log_any_app if $do_log;
    }

    # we'll try giving argv to server side, but this currently means we skip
    # processing cmdline_src.
    if ($res->[0] == 502) {
        #$log->debugf("Failed parsing arguments (status 502), will try to send ".
        #                 "argv to server");
        $self->{_getargs_result} = $res;
        $self->{_send_argv} = 1;
        return;
    }

    $self->_err("Failed parsing arguments: $res->[0] - $res->[1]")
        unless $res->[0] == 200;
    for (keys %{ $res->[2] }) {
        $self->{_args}{$_} = $res->[2]{$_};
    }
    #$log->tracef("result of GetArgs for subcommand: remaining argv=%s, args=%s".
    #                 ", actions=%s", \@ARGV, $self->{_args}, $self->{_actions});

    # handle cmdline_src
    if (!$ENV{COMP_LINE} && ($self->{_actions}[0] // "") eq 'call') {
        my $args_p = $meta->{args} // {};
        my $stdin_seen;
        for my $an (sort keys %$args_p) {
            #$log->tracef("TMP: handle cmdline_src for arg=%s", $an);
            my $as = $args_p->{$an};
            my $src = $as->{cmdline_src};
            if ($src) {
                $self->_err(
                    "Invalid 'cmdline_src' value for argument '$an': $src")
                    unless $src =~ /\A(stdin|file|stdin_or_files)\z/;
                $self->_err(
                    "Sorry, argument '$an' is set cmdline_src=$src, but type ".
                        "is not 'str' or 'array', only those are supported now")
                    unless $as->{schema}[0] =~ /\A(str|array)\z/;
                if ($src =~ /stdin/) {
                    $self->_err("Only one argument can be specified ".
                                    "cmdline_src stdin/stdin_or_files")
                        if $stdin_seen++;
                }
                my $is_ary = $as->{schema}[0] eq 'array';
                if ($src eq 'stdin' || $src eq 'file' &&
                        ($self->{_args}{$an}//"") eq '-') {
                    $self->_err("Argument $an must be set to '-' which means ".
                                    "from stdin")
                        if defined($self->{_args}{$an}) &&
                            $self->{_args}{$an} ne '-';
                    #$log->trace("Getting argument '$an' value from stdin ...");
                    $self->{_args}{$an} = $is_ary ? [<STDIN>] :
                        do { local $/; <STDIN> };
                } elsif ($src eq 'stdin_or_files') {
                    # push back argument value to @ARGV so <> can work to slurp
                    # all the specified files
                    local @ARGV = @ARGV;
                    unshift @ARGV, $self->{_args}{$an}
                        if defined $self->{_args}{$an};
                    #$log->tracef("Getting argument '$an' value from ".
                    #                 "stdin_or_files, \@ARGV=%s ...", \@ARGV);
                    $self->{_args}{$an} = $is_ary ? [<>] : do { local $/; <> };
                } elsif ($src eq 'file') {
                    unless (exists $self->{_args}{$an}) {
                        if ($as->{req}) {
                            $self->_err(
                                "Please specify filename for argument '$an'");
                        } else {
                            next;
                        }
                    }
                    $self->_err("Please specify filename for argument '$an'")
                        unless defined $self->{_args}{$an};
                    #$log->trace("Getting argument '$an' value from ".
                    #                "file ...");
                    my $fh;
                    unless (open $fh, "<", $self->{_args}{$an}) {
                        $self->_err("Can't open file '$self->{_args}{$an}' ".
                                        "for argument '$an': $!")
                    }
                    $self->{_args}{$an} = $is_ary ? [<$fh>] :
                        do { local $/; <$fh> };
                }
            }
        }
    }
    #$log->tracef("args after cmdline_src is processed: %s", $self->{_args});

    #$log->tracef("<- _parse_subcommand_opts()");
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

