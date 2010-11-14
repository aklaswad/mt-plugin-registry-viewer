package RegistryViewer;
use strict;
use warnings;
use MT;
use MT::Component;
use MT::Util;

sub uri {
    my @path = @_;
    my %param = (
        mode => 'registry_viewer',
        args => {
            path => join( ',', @path ),
        },
    );
    MT->app->uri(%param);
}

sub run {
    my $app = shift;
    my %param;
    my $path = $app->param('path');
    my @paths = split ',', $path;
    my $current_path = $paths[-1];
    my @parent_path;
    my @breadcrumb;
    $param{root_link} = my $root_link = uri();
    push @breadcrumb, { link => $root_link, label => '(root)' };
    for my $p (@paths) {
        push @parent_path, $p;
        my $link = uri(@parent_path);
        push @breadcrumb, { link => $link, label => $p };
    }
    $param{breadcrumb} = \@breadcrumb;

    my @descriptions = find_desc( path => \@paths );
    $param{descriptions} = \@descriptions;

    my @components = sort keys %MT::Components;
    my $last_type;
    my %registries;
    for my $c ( @components ) {
        my $reg;
        eval {
            $reg = MT->component($c)->registry(@paths);
        };
        if ( $@ ) {
            my @parent_path = @paths;
            my $me = pop @parent_path;
            $reg = MT->component($c)->registry(@parent_path);
            $reg = $reg->{$me} if $me;
        }
        next unless defined $reg;
        my $type = ref $reg || 'scalar';
        if ( $last_type ) {
            $type eq $last_type
                or return $app->errtrans('Multiple types mixed registry.');
        }
        $last_type = $type;
        $registries{$c} = $reg;
    }
    my $type = $param{type} = $last_type;

    if ( $type eq 'HASH' ) {
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
                push @values, {
                    component => $c,
                    value     => $c_value,
                };
            }
            my @descs = find_desc( path => \@child_paths, no_wild => 1 );
            push @child_keys, { link => $link, label => $child, values => \@values, descs => \@descs };
        }
        $param{child_keys} = \@child_keys;
    }
    else {
        my @registries;
        for my $c ( keys %registries ) {
            my $reg = $registries{$c};
            my %reg_param;
            if ( 'ARRAY' eq ref $reg ) {
                $reg_param{registry_values} = $reg;
            }
            elsif ( !ref $reg ) {
                $reg_param{registry_value} = $reg;
            }
            elsif ( $current_path =~ /label$/ ) {
                $reg_param{registry_value} = ref $reg eq 'CODE' ? $reg->() : $reg;
            }
            else {
                require MT::Util::YAML;
                my $dump =  MT::Util::YAML::Dump $reg;
                $reg_param{registry_value} = qq{<pre>$dump</pre>};
            }
            $reg_param{__component} = $c;
            push @registries, \%reg_param;
        }
        $param{registries} = \@registries;
    }
    return $app->load_tmpl('registry_viewer.tmpl', \%param);
}

sub find_desc {
    my ( %opts ) = @_;
    my $path      = $opts{path};
    my $orig_path = $opts{orig_path};
    my $idx       = $opts{idx} || 0;
    $orig_path = [ @$path ] unless defined $orig_path;
    return if !defined $path;
    return if !scalar @$path;
    if ( $idx == scalar @$path ) {
        my $description = MT->registry( 'registry_descriptions', @$path );
        if ( 'HASH' eq ref $description ) {
            $description = $description->{_};
        }
        return () unless $description;
        if ( $description =~ /sub \{/ || $description =~ /^\$.*::/ ) {
            my $code = MT->handler_to_coderef($description);
            $description = $code->($orig_path, $path);
        }
        $description = {
            for => '/' . join( '/', @$path ),
            desc => $description,
        };
        return ( $description );
    }
    my @clone = @$path;
    my @descriptions;

    for my $wild ( 0..( $opts{no_wild} ? 0 : 1 ) ) {
        $clone[$idx] = '*' if $wild;
        my @moredescs = find_desc(
            %opts,
            path      => \@clone,
            orig_path => $orig_path,
            idx       => $idx + 1, );
        push @descriptions, @moredescs;
    }
    return @descriptions;
}

1;
