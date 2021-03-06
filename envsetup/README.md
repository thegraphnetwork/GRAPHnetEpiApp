 
# Host Configuration files
Some basic configuration files are stored in this directory and must be applied on the host on deploy.

## Basic configuration
After the first login, remember to  allow only public-key authentication. For that, you must edit the file `/etc/ssh/sshd_config` and set
```
PubkeyAuthentication yes
PasswordAuthentication no
``` 
And then restart `ssh`
```bash
sudo service ssh restart
``` 

Create a non-root user and put it in the adm group
```bash
usermod -aG sudo <username>
```

You should also create a non root user with sudo powers.

## Installing Docker
To install docker follow these [instructions](https://docs.docker.com/engine/install/ubuntu/).

then run these commands to give permission to your non-root user to issue docker commands

```bash
$ sudo groupadd docker
$ sudo usermod -aG docker $USER
$ newgrp docker
```

Also install docker-compose by following [these instructions](https://docs.docker.com/compose/install/)

## Firewall Configuration
Make sure that `ufw` firewall configuration program is  installed.

Run the configuration script below which will turn on the firewal, blocking up all connection except the ones required by EpigraphHub

```bash
sudo ./ufw_setup.sh 
```

Aditionally, in order to fix docker UFW vulnerability, edit the `/etc/default/docker` and add the following line:

```
DOCKER_OPTS="--iptables=false"
```

After saving and closing the  file, make sure to restart docker using the following command:

```bash
sudo service docker restart
```
## Setting up Nginx
After building the containers with `docker-compose`, you need to configure Nginx to serve the app and PGadmin4.

Copy the file `nginx_default.conf` as the default site configuration with the following command:

```bash
sudo cp nginx_default.conf /etc/nginx/sites-available/default
```
then restart Nginx with the following command:

```bash
sudo service nginx restart
```
