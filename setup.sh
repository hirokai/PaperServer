#!/bin/sh

# You need to be in the root folder first.

apt-get update
apt-get install haskell-platform npm nginx-full mongodb

mkdir /data
mkdir /data/db

apt-get install node-less
npm install -g typescript

# Compile less
lessc -x ./static/css/welcome.less > ./static/css/welcome.css
lessc -x ./static/css/duplicated_user.less > ./static/css/duplicated_user.css
lessc -x ./static/css/paperlist.less > ./static/css/paperlist.css
lessc -x ./static/css/format_a.less > ./static/css/format_a.css
lessc -x ./static/css/format_b.less > ./static/css/format_b.css
lessc -x ./static/css/format_a_mobile.less > ./static/css/format_a_mobile.css
lessc -x ./static/css/infoModal.less > ./static/css/infoModal.css
lessc -x ./static/css/paperlist_tablet.less > ./static/css/paperlist_tablet.css
lessc -x ./static/css/paperlist_with_json.less > ./static/css/paperlist_with_json.css

# Compile TypeScript
cd ./static/js
tsc paperlist.ts
cd ../..

# Make folders
mkdir temp
mkdir temp/epub_source
mkdir data
mkdir data/paper
mkdir data/image
mkdir data/FormatA
mkdir data/FormatB
mkdir data/FormatATablet
mkdir data/FormatBTablet
mkdir data/FormatAMobile
mkdir data/FormatBMobile

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
