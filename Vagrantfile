$script = <<-SCRIPT
sudo apt-get update
sudo apt-get install -y nginx certbot python-certbot-nginx
stack --version || wget -qO- https://get.haskellstack.org/ | sh
SCRIPT

$build = <<-SCRIPT
cd /vagrant
sudo stack --allow-different-user build
SCRIPT

$test = <<-SCRIPT
cd /vagrant
sudo stack --allow-different-user test
SCRIPT

$run = <<-SCRIPT
sudo killall website
cd /vagrant
sudo stack --allow-different-user exec website
SCRIPT

$runconf = <<-SCRIPT
sudo killall website
cd /vagrant
sudo stack --allow-different-user exec website config.json
SCRIPT

$deploy = <<-SCRIPT
cd /vagrant
sudo stack --allow-different-user install
cd /root/.local/bin
tar -cvjf website.tar.bz website
SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "debian/buster64"

  config.vm.provider "virtualbox" do |vb|
    vb.gui = false
    vb.customize ["modifyvm", :id, "--memory", "6144"]
    vb.customize ["modifyvm", :id, "--cpus", 4]
    vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
    vb.customize ["modifyvm", :id, "--uartmode1", "disconnected"]
  end

  config.vm.network "forwarded_port", guest: 80, host: 7000
  config.vm.network "forwarded_port", guest: 22, host: 2822, id: "ssh"
  config.vm.provision "shell", inline: $script

  config.vm.provision "build", type: "shell", run: "never", inline: $build
  config.vm.provision "test", type: "shell", run: "never", inline: $test
  config.vm.provision "run", type: "shell", run: "never", inline: $run
  config.vm.provision "runconf", type: "shell", run: "never", inline: $runconf
  config.vm.provision "deploy", type: "shell", run: "never", inline: $deploy
end
