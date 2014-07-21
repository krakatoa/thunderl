class erlang() {}
define erlang::install() {
  $packages = ["erlang-base", "erlang-reltool", "erlang-dev", "erlang-eunit"] # erlang-dev and erlang-unit are needed, else it throws eunit related bugs on compilation

  include apt

  apt::key { 'erlang':
    key => 'A14F4FCA',
    key_source => 'http://packages.erlang-solutions.com/debian/erlang_solutions.asc'
  }

  apt::source { 'erlang':
    location => 'http://packages.erlang-solutions.com/debian',
    release => 'wheezy',
    repos => 'contrib',
    include_src => false
  } -> exec { "apt-get update": path => ["/usr/bin", "/bin"] } ->
  package { $packages:
    ensure => latest,
    require => [Apt::Source['erlang']]
  }

}

erlang::install { 'erlang': }

package { "make":
  ensure => installed
}

file { "/var/www/":
  ensure => "directory",
  recurse => true,
  owner => "deploy",
  group => "deploy"
}

class rebar($base_path = "/tmp/rebar") {

  $rebar_dependencies = ["git-core"]
  package { "rebar dependencies":
    name => $rebar_dependencies,
    ensure => installed
  }

  exec { "rebar fetch":
    command => "git clone git://github.com/rebar/rebar.git ${base_path}",
    path => ["/usr/bin", "/bin"],
    creates => $base_path,
    notify => Exec["rebar make"]
  }

  exec { "rebar make":
    cwd => $base_path,
    command => "make",
    environment => ['HOME=/root'],
    path => ["/usr/bin", "/bin"],
    require => [ Erlang::Install["erlang"], Package['make'], Exec["rebar fetch"] ],
    creates => "${base_path}/rebar"
  }

  file { "rebar install":
    path => "/usr/local/bin/rebar",
    source => "${base_path}/rebar",
    mode => "a+x",
    require => Exec["rebar make"]
  }
}

include rebar

file { "${app_name} service":
  path => "/etc/init.d/${app_name}",
  content => template("init.script.erb"),
  mode => "0755"
}

file { "${app_name} defaults":
  path => "/etc/default/${app_name}",
  content => template("default"),
}
