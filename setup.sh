#!/bin/sh

# You need to be in the root folder first.

apt-get update
apt-get install haskell-platform npm nginx-full mongodb

mkdir /data
mkdir /data/db

apt-get install node-less
npm install -g typescript

./compile.sh
./mkfolders.sh

# Build Parser project and make a symbolic link
sudo apt-get install libpcre3-dev
cd Parser
cabal update
cabal configure
cabal install --force-reinstalls

# Build main project
cd ..
cabal configure
cabal install --force-reinstalls


# if you need to migrate DB.
# apt-get install ruby rubygems
# gem install mongo
