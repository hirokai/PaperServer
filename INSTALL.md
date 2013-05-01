How to install the server (Checked on Ubuntu 12.04)
===================================================

1. Clone Git repo

2. Modify config/settings.yml

Set approot correctly
> approot: "http://157.7.167.209:3000"

3. Install dependencies by cabal

4. Use ghc-pkg to resolve version conflicts.
Removing conflicting packages and adding again should work.
(This glitch should be fixed at some point.)

5. Configure, compile, and install
> cabal clean
> cabal configure
> cabal build

6. Install and set up MongoDB and nginx
Following steps are described in http://www.yesodweb.com/book/deploying-your-webapp for more info.

> apt-get update
> apt-get install mongodb-10gen
> apt-get install nginx 

Edit "db-path" option in /etc/init.d/mongodb.conf
http://docs.mongodb.org/manual/tutorial/install-mongodb-on-ubuntu/

Put something like following into /etc/nginx/nginx.conf

>	server {
>   listen 80;
>   server_name 157.7.167.209;
>   rewrite ^/(.*) http://157.7.167.209:3000/$1 permanent;
>   location / {
>     proxy_pass http://157.7.167.209:3000;
>   }
>   location /static {
>     root /root/PaperServer/static;
>     expires max;
>   }
> }

7. Start MongoDB and nginx
> service mongodb start
> service nginx start

8. Set up the PaperServer as a service
> cat > /etc/init/paper.conf
> description "Paper server"
> start on runlevel [2345];
> stop on runlevel [!2345];
> respawn
> chdir /root/PaperServer
> exec /root/PaperServer/dist/build/PaperServer/PaperServer Production

Paper server can be started and stopped by
> start paper
> stop paper

