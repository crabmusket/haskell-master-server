# Copy profile into home directory
cd /vagrant/vm
cp .bash_profile $HOME/.bash_profile

# Install Haskell tools
echo "Installing GHC and cabal-install"
apt-add-repository "deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main"
apt-get update
apt-get install zlib1g-dev -y
apt-get install -y --force-yes ghc-7.8.3 cabal-install-1.20

# Install Heroku tools
if hash heroku 2>/dev/null; then
	echo "Heroku already installed :)"
else
	echo "Installing Heroku"
	wget -qO- https://toolbelt.heroku.com/install-ubuntu.sh | sh
fi
