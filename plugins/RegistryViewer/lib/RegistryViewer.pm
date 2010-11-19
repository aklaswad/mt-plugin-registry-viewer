package RegistryViewer;
use strict;
use warnings;
use MT;
use MT::Component;
use MT::Util;

sub uri {
    my @path = @_;
    my $opts = {};
    if ( ref $path[0] eq 'HASH' ) {
        $opts = $path[0];
        @path = @{ $opts->{path} };
    }
    my $app   = MT->app;
    my %param = (
        mode => 'registry_viewer',
        args => {
            ( scalar @path ? ( path => join( ',', @path ) ) : () ),
            mt_nav => defined $opts->{mt_nav}
            ? $opts->{mt_nav}
            : $app->{__mt_nav},
        },
    );
    $app->uri(%param);
}

sub view {
    my $app    = shift;
    my $path   = $app->param('path');
    my $mt_nav = $app->param('mt_nav');
    $mt_nav = 1 if !defined $mt_nav;
    local $app->{__mt_nav} = $mt_nav;
    my @paths  = split ',', $path;
    my $plugin = MT->component('RegistryViewer');
    my $param  = run( $plugin, @paths )
        or return $app->error( $plugin->errstr );
    $param->{mt_nav}             = $mt_nav;
    $param->{toggle_mt_nav_link} = uri(
        {   path   => \@paths,
            mt_nav => $mt_nav ? 0 : 1,
        }
    );

    if ( !$mt_nav ) {
        $param->{build_menus}         = 0;
        $param->{build_blog_selector} = 0;
        $param->{build_compose_menus} = 0;
    }
    return $app->load_tmpl( 'registry_viewer.tmpl', $param );
}

sub run {
    my $plugin = shift;
    my @paths  = @_;
    my %param;
    my $current_path = $paths[-1];
    my @parent_path;

    ## Make breadcrumb
    my @breadcrumb;
    $param{root_link} = my $root_link = uri();
    push @breadcrumb, { link => $root_link, label => '(root)' };
    for my $p (@paths) {
        push @parent_path, $p;
        my $link = uri(@parent_path);
        push @breadcrumb, { link => $link, label => $p };
    }
    $param{breadcrumb} = \@breadcrumb;

    ## Get description of current path.
    my @descriptions = find_desc( path => \@paths );
    $param{descriptions} = \@descriptions;

    ## Get Possible values.
    my @wild_descs = find_desc( path => [ @paths, '+' ] );
    $param{wild_descs} = [
        sort { $a->{name} cmp $b->{name} }
        grep { $_->{for} =~ m/\*|\(\w+\)/ } @wild_descs
        ]
        if scalar @wild_descs;

    ## Get child values from real registry.
    my @components = sort keys %MT::Components;
    my $last_type;
    my %registries;
    for my $c (@components) {
        my $reg;
        eval { $reg = MT->component($c)->registry(@paths); };
        if ($@) {
            my @parent_path = @paths;
            my $me          = pop @parent_path;
            $reg = MT->component($c)->registry(@parent_path);
            $reg = $reg->{$me} if $me;
        }
        next unless defined $reg;
        my $type = ref $reg || 'scalar';
        if ($last_type) {
            $type eq $last_type
                or return $plugin->errtrans('Multiple types mixed registry.');
        }
        $last_type = $type;
        $registries{$c} = $reg;
    }
    my $type = $param{type} = $last_type;

    if ( $type eq 'HASH' ) {
        my %wild_desc = map { $_->{name} => $_ } @wild_descs;
        my %keys = map { $_ => 1 } map { keys %$_ } values %registries;
        my @childs = sort keys %keys;
        my @child_keys;
        for my $child (@childs) {
            my @child_paths = ( @paths, $child );
            my $link = uri(@child_paths);
            my @values;
            for my $c ( keys %registries ) {
                my $hash = $registries{$c} or next;
                my $c_value = $hash->{$child};
                next if !defined $c_value;
                $c_value = ref $c_value if ref $c_value;
                push(
                    @values,
                    {   component => $c,
                        value     => $c_value,
                    }
                );
            }
            my @descs = find_desc( path => \@child_paths, wild => 0 );
            push(
                @child_keys,
                {   link   => $link,
                    name   => $child,
                    values => \@values,
                    descs  => \@descs
                }
            );
        }
        $param{child_keys} = \@child_keys;
    }
    else {
        my @registries;
        for my $c ( keys %registries ) {
            my $reg = $registries{$c};
            my %reg_param;
            if ( $current_path =~ /label$/ ) {
                $reg_param{registry_value}
                    = ref $reg eq 'CODE' ? $reg->() : $reg;
            }
            elsif ( 'ARRAY' eq ref $reg ) {
                $reg_param{registry_values} = $reg;
            }
            elsif ( 'CODE' eq ref $reg ) {
                $reg_param{registry_value} = 'CODE';
            }
            elsif ( !ref $reg ) {
                $reg_param{registry_value} = $reg;
            }
            else {
                require MT::Util::YAML;
                my $dump = MT::Util::YAML::Dump $reg;
                $reg_param{registry_value} = qq{<pre>$dump</pre>};
            }
            $reg_param{__component} = $c;
            push @registries, \%reg_param;
        }
        $param{registries} = \@registries;
    }
    return \%param;
}

sub find_desc {
    my (%opts) = @_;
    my $path = $opts{path};
    my $base_path = $opts{base_path} || 'registry_descriptions';
    my $depth     = $opts{depth}     || 0;
    my $wild      = exists $opts{wild}      ? $opts{wild}      : 1;
    my $orig_path = exists $opts{orig_path} ? $opts{orig_path} : [@$path];
    my $current_path = $opts{current_path} || [];
    return if !defined $path;

    my @rest_path = @$path;
    my $r = MT->registry( $base_path, @$current_path );
    if ( 'HASH' ne ref $r ) {
        return (
            make_desc(
                name      => $opts{name},
                value     => $r,
                path      => $current_path,
                orig_path => $orig_path,
            )
        ) if !scalar @rest_path;
        return ();
    }

    my @results;
    if ( $r->{_} && !scalar @rest_path ) {
        push(
            @results,
            (   make_desc(
                    name      => $opts{name},
                    value     => $r->{_},
                    path      => $current_path,
                    orig_path => $orig_path,
                )
            )
        );
    }

    my @this_key;
    while ( my $this_key = shift @rest_path ) {
        my $regex;
        my $key = quotemeta($this_key);
        if ( $this_key eq '+' ) {
            $regex .= '([^/]+)';
        }
        elsif ( $wild && $this_key ne '*' ) {
            $regex .= '(' . $key . '|\(\w+\)|\*)';
        }
        else {
            $regex .= '(' . $key . ')';
        }
        push @this_key, $regex;
        for my $hash_key ( keys %$r ) {
            next if '_' eq $hash_key;
            my $match;
            {
                local $" = '/';
                $match = $hash_key =~ /^@this_key$/;
            }
            if ($match) {
                push(
                    @results,
                    find_desc(
                        path         => \@rest_path,
                        base_path    => $base_path,
                        depth        => $depth + 1,
                        wild         => $wild,
                        orig_path    => $orig_path,
                        name         => $hash_key,
                        current_path => [ @$current_path, $hash_key ],
                    )
                );
            }
        }
    }
    return @results;
}

sub make_desc {
    my (%opts)    = @_;
    my $values    = $opts{value};
    my @path      = @{ $opts{path} };
    my $name      = $opts{name};
    my $orig_path = $opts{orig_path};
    $values = [$values] unless ref $values;
    @path = map { split '\/', $_ } @path;

    if ( $name =~ m{/} ) {
        my @names = split '\/', $name;
        $name = pop @names;
    }
    my @descs;
    for my $desc (@$values) {
        if ( $desc =~ /\s*sub\s*\{/ || $desc =~ /^\$.*::/ ) {
            my $code = MT->handler_to_coderef($desc);
            $desc = $code->( $orig_path, \@path );
        }
        push(
            @descs,
            {   name => $name,
                for  => join( '/', @path ),
                desc => $desc,
                link => uri(@path),
            }
        );
    }
    return @descs;
}

1;
