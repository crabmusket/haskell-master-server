# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure('2') do |config|
  config.vm.box = 'ubuntu/trusty32'

  config.vm.provision :shell, path: 'vm/provision.sh'

  config.vm.provider 'virtualbox' do |vb|
    vb.memory = 1024
  end

  config.vm.network 'forwarded_port', guest: 3000, host: 3000
end
