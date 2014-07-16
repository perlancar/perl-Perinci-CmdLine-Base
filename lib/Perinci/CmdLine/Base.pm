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
has get_subcommand_from_arg => (is=>'rw', default=>1);
has description => ();
has exit => (is=>'rw', default=>1);
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
has subcommands => ();
has summary => ();
has tags => ();
has url => ();

# role: requires 'get_meta' # ($url)

# role: requires 'hook_before_run'
# role: requires 'hook_after_parse_argv'
# role: requires 'hook_after_select_subcommand'
# role: requires 'hook_format_result'
# role: requires 'hook_display_result'
# role: requires 'hook_after_run'

sub get_program_and_subcommand_name {
    my ($self, $r) = @_;
    my $res = $self->program_name . " " .
        ($r->{subcommand_name} // "");
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
    #require Complete::Bash;
    #my ($words, $cword) = Complete::Bash::parse_cmdline();
}

sub parse_argv {
    my ($self, $r) = @_;

    # we parse argv twice. the first parse is with common_opts only so we're
    # able to catch --help, --version, etc early without having to know about
    # subcommands. two reasons for this: sometimes we need to get subcommand
    # name *from* cmdline opts (e.g. --cmd) and thus it's a chicken-and-egg
    # problem. second, it's faster (especially in P::C case).
    #
    # the second parse is after ge get subcommand name and the function
    # metadata. we can parse the remaining argv to get function arguments.
    #
    # note that when doing completion we're not using this algorithem and only
    # parse argv once. this is to make completion work across common- and
    # per-subcommand opts, e.g. --he<tab> resulting in --help (common opt) as
    # well as --height (function argument).

    {
        # one small downside for this is that we cannot do autoabbrev here,
        # because we're not yet specifying all options here.

        require Getopt::Long;
        my $old_go_conf = Getopt::Long::Configure(
            'pass_through', 'permute', 'no_auto_abbrev');
        my @go_spec;
        my $co = $self->common_opts // {};
        for my $k (keys %$co) {
            push @go_spec, $co->{$k}{getopt} => sub {
                my ($go, $val) = @_;
                $co->{$k}{handler}->($r, $go, $val);
            };
        }
        Getopt::Long::GetOptions(@go_spec);
        Getopt::Long::Configure($old_go_conf);
    }

    # select subcommand and fill subcommand data
    {
        my $scn = $r->{subcommand_name};
        if (!defined($scn) && defined($self->{default_subcommand})) {
            # get from default_subcommand
            if ($self->{get_subcommand_data} == 1) {
                $scn = $self->{default_subcommand};
            } elsif ($self->{get_subcommand_data} == 2 && !@ARGV) {
                $scn = $self->{default_subcommand};
            }
        }
        if (!defined($scn) && $self->{subcommands} && @ARGV) {
            # get from first command-line arg
            if ($ARGV[0] =~ /\A-/) {
                die [400, "Unknown option: $ARGV[0]"];
            } else {
                $scn = shift @ARGV;
            }
        }

        my $scd;
        if (defined $scn) {
            $scd = $self->get_subcommand_data($scn);
            die [500, "Unknown subcommand: $scn"] unless $scd;
        } elsif (!$r->{action}) {
            # user doesn't specify any subcommand, or specific action. display
            # help instead.
            $r->{action} = 'help';
            $r->{skip_parse_subcommand_argv} = 1;
        } else {
            $scn = '';
            $scd = {
                url => $self->url,
                summary => $self->summary,
                description => $self->description,
                pass_cmdline_object => $self->pass_cmdline_object,
                tags => $self->tags,
            };
        }
        $r->{subcommand_name} = $scn;
        $r->{subcommand_data} = $scd;
    }

    my %args;

    # also set dry-run on environment
    $r->{dry_run} = 1 if $ENV{DRY_RUN};

    # parse argv for per-subcommand command-line opts
    if ($r->{skip_parse_subcommand_argv}) {
        return [200, "OK (subcommand options parsing skipped)", \%args];
    } else {
        my $scd = $r->{subcommand_data};
        my $meta = $self->get_meta($scd->{url});
        $r->{meta} = $meta;

        my $co = $self->common_opts;
        require Perinci::Sub::GetArgs::Argv;
        my $res = Perinci::Sub::GetArgs::Argv::get_args_from_argv(
            argv                => \@ARGV,
            args                => $scd->{args} ? { %{$scd->{args}} } : undef,
            meta                => $meta,
            allow_extra_elems   => 0,
            per_arg_json        => 1,
            per_arg_yaml        => 1,
            common_opts         => { map {$co->{$_}{getopt} => sub{}}
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
        return $res;
    }
}

sub run {
    my ($self) = @_;

    # completion is special case, we delegate to do_completion()
    if ($ENV{COMP_LINE}) {
        return do_completion();
    }

    my $r;
    eval {
        $r = {};
        $self->hook_before_run($r);

        my $parse_res = $self->parse_argv($r);
        die $parse_res unless $parse_res->[0] == 200;
        $r->{parse_argv_res} = $parse_res;

        # set defaults
        $r->{action} //= 'call';

        $self->hook_after_parse_argv($r);
        my $missing = $parse_res->[3]{"func.missing_args"};
        die [400, "Missing required argument(s): ".join(", ", @$missing)]
            if $missing && @$missing;

        my $args = $parse_res->[2];
        $r->{args} = $args;
        my $scd = $r->{subcommand_data};
        $args->{-cmdline} = $self if $scd->{pass_cmdline_object} //
            $self->pass_cmdline_object;

        my $meth = "run_$r->{action}";
        die [500, "Unknown action $r->{action}"] unless $self->can($meth);
        $r->{res} = $self->$meth($r);
    };
    my $err = $@;
    if ($err || !$r->{res}) {
        if ($err) {
            $err = [500, "Died: $err"] unless ref($err) eq 'ARRAY';
            $r->{res} = $err;
        } else {
            $r->{res} = [500, "Bug: no response produced"];
        }
    }
    $r->{fres} = $self->hook_format_result($r);
    $self->hook_display_result($r);

  L1:
    $self->hook_after_run($r);
    if ($self->exit) {
        if ($r->{res}[3] && $r->{res}[3]{'cmdline.exit_code'}) {
            exit $r->{res}[3]{'cmdline.exit_code'};
        } else {
            exit $self->status2exitcode($r->{res}[0]);
        }
    } else {
        return $r->{res};
    }
}

1;
# ABSTRACT: Base class for Perinci::CmdLine{,::Lite}

=for Pod::Coverage ^(.+)$

=head1 REQUEST KEYS

The various values in the C<$r> hash/stash.

=over

=item * action => str

Selected action to use. Usually set from the common options.

=item * format => str

Selected format to use. Usually set from the common option C<--format>.

=item * parse_argv_res => array

Enveloped result of C<parse_argv()>.

=item * skip_parse_subcommand_argv => bool

Checked by C<parse_argv()>. Can be set to 1, e.g. in common option handler for
C<--help> or C<--version> to skip parsing @ARGV for per-subcommand options.

=item * args => hash

Also taken from C<parse_arg()> result.

=item * meta => hash

Result of C<get_meta()>.

=item * dry_run => bool

Whether to pass C<-dry_run> special argument to function.

=item * res => array

Enveloped result of C<action_ACTION()>.

=item * fres => str

Result from C<hook_format_result()>.

=back


=head1 ATTRIBUTES

=head2 actions => array

=head2 common_opts => hash

=head2 default_subcommand => str

=head2 get_subcommand_from_arg => int (default: 1)

The default is 1, which is to get subcommand from the first command-line
argument except when there is C<default_subcommand> defined. Other valid values
are: 0 (not getting from first command-line argument), 2 (get from first
command-line argument even though there is C<default_subcommand> defined).

=head2 description => str

=head2 exit => bool (default: 1)

=head2 formats => array

Available output formats.

=head2 pass_cmdline_object => bool (default: 0)

=head2 program_name => str

Default is from PERINCI_CMDLINE_PROGRAM_NAME environment or from $0.

=head2 subcommands => hash | code

=head2 summary => str

=head2 tags => array of str

=head2 url => str


=head1 METHODS

=head2 $cmd->run() => ENVRES

Will parse command-line arguments with C<parse_argv()>, select/set subcommand,
call hooks, run the appropriate C<run_ACTION()> method, and finally format and
display the result.

The C<run_ACTION()> methods will be passed C<$r> and is supposed to return an
enveloped result. The result will then be put in C<$r->{res}>.

If C<exit> attribute is true, will exit with the action's envelope result
status. If status is 200, exit code is 0. Otherwise exit code is status minus
300. So, a response C<< [501, "Not implemented"] >> will result in exit code of
201.

If C<exit> attribute is false, will simply return the action result
(C<$r->{res}>).


=head2 $cmd->do_completion() => ENVRES

Called by run().

=head2 $cmd->parse_argv() => ENVRES

Called by run().

=head2 $cmd->get_meta($url) => ENVRES

Called by parse_argv() or do_completion(). Subclass has to implement this.


=head1 HOOKS

All hooks will receive the argument C<$r>, a per-request hash/stash. The list
below is by order of calling.

=head2 $cmd->hook_before_run($r)

Called at the start of C<run()>. Can be used to set some initial values of other
C<$r> keys. Or setup the logger.

=head2 $cmd->hook_after_parse_argv($r)

Called after C<run()> calls C<parse_argv()> and before it checks the result.
C<$r->{parse_argv_res}> will contain the result of C<parse_argv()>. The hook
gets a chance to, e.g. fill missing arguments from other source.

=head2 $cmd->hook_format_result($r)

The hook is supposed to format result in C<$res->{res}> (an array).

=head2 $cmd->hook_display_result($r)

The hook is supposed to display the formatted result (stored in C<$r->{fres}>)
to STDOUT. But in the case of streaming output, this hook can also set it up.

=head2 $cmd->hook_after_run($r)

Called at the end of C<run()>, right before it exits (if C<exit> attribute is
true) or returns C<$r->{res}>. The hook has a chance to modify exit code or
result.
