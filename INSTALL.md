How to install the server (Checked on Ubuntu 12.04)
===================================================

### 1. Clone Git repo
`git clone https://github.com/hirokai/PaperServer`

### 1. Modify config/settings.yml

### 1. Set `approot`, `hostname`, and `analytics_code` in `config` folder correctly

### 1. Run the following scripts
> ./mkfolders.sh<br>
> ./compile.sh


### 1. Edit "db-path" option in /etc/init.d/mongodb.conf if needed
http://docs.mongodb.org/manual/tutorial/install-mongodb-on-ubuntu/

### 1. Put something like `macosx_nginx.conf` into /etc/nginx/nginx.conf


### 1. Start MongoDB and nginx
> service mongodb start<br>
> service nginx start

### 1. Set up the PaperServer as a service

> cat > /etc/init/paper.conf<br>
> description "Paper server"<br>
> start on runlevel [2345];<br>
> stop on runlevel [!2345];<br>
> respawn<br>
> chdir /root/PaperServer<br>
> exec /root/PaperServer/dist/build/PaperServer/PaperServer Production

### 1. Paper server can be started and stopped by
> start paper
> stop paper

