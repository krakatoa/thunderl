import 'freeswitch.pp'

node /fs/ {
  freeswitch::install { 'freeswitch': }
  freeswitch::setup { 'freeswitch':
    freeswitch_conf_path => "/etc/freeswitch",
  #  freeswitch_conf_path => "/usr/local/freeswitch/conf",
    require => Freeswitch::Install['freeswitch']
  }

  service { "freeswitch":
    ensure => "running",
    require => [ Freeswitch::Setup['freeswitch'] ]
  }

  # development only
  # include sipp

  package { "curl":
    ensure => installed
  }
  
  # freeswitch::development_install { 'freeswitch-development': }

  user {'vagrant':
    groups => ['staff']
  }

  host { 'thunderl':
    name => 'fs.thunderl.com',
    ip => $ipaddress_eth1,
    ensure => present
  }
}

