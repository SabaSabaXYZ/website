$script = <<-SCRIPT
sudo apt-get update
sudo apt-get install -y nginx certbot python-certbot-nginx
stack --version || wget -qO- https://get.haskellstack.org/ | sh
SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "generic/debian10"

  config.vm.provider "virtualbox" do |vb|
    vb.gui = false
    vb.customize ["modifyvm", :id, "--memory", "6144"]
    vb.customize ["modifyvm", :id, "--cpus", 4]
    vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
    vb.customize ["modifyvm", :id, "--uartmode1", "disconnected"]
  end

  config.vm.network "forwarded_port", guest: 80, host: 7000
  config.vm.provision "shell", inline: $script

end
