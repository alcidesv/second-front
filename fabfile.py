
from __future__ import with_statement, print_function
from fabric.api import local, settings, abort, run, sudo, cd, hosts, env, execute
from fabric.contrib.console import confirm
from fabric.operations import put, get
from fabric.contrib.project import rsync_project

import time
import re
import subprocess as sp 
import os.path 
from   StringIO import StringIO
import json



deploy_settings = json.load( open("tools/deploy_settings.json") )
normal = deploy_settings["host"]["normal"]
root = deploy_settings["host"]["root"]
user = deploy_settings["user"]

upstart_supervisord = """
description "Supervisor"

start on runlevel [2345]
stop on runlevel [!2345]

umask 022

console log

env PATH=/opt/openssl-1.0.2/bin/:/usr/bin:/usr/local/bin:/usr/sbin:/bin
export PATH
env LD_LIBRARY_PATH=/opt/openssl-1.0.2/lib
export LD_LIBRARY_PATH


script
    exec /usr/local/bin/supervisord -n -c /home/{user}/supervisor.conf
end script

"""

supervisor_conf = """
[unix_http_server]
file = /tmp/supervisor.sock
chmod = 0777
chown= www:www

[inet_http_server]
port = 127.0.0.1:9001


[supervisord]
logfile=%(here)s/logs/supervisord.log

[rpcinterface:supervisor]
supervisor.rpcinterface_factory = supervisor.rpcinterface:make_main_rpcinterface

[program:second-front-fs]
command=/home/{user}/second-front-fs
environment=LD_LIBRARY_PATH=/opt/openssl-1.0.2/lib
directory=/home/{user}/
user={user}
autostart=true
autorestart=true
redirect_stderr=true
stdout_logfile=/home/{user}/logs/second-front-fs.log
stopsignal=TERM
"""

iptables_config = """
*nat
:PREROUTING ACCEPT [1:52]
:INPUT ACCEPT [1:52]
:OUTPUT ACCEPT [1:152]
:POSTROUTING ACCEPT [1:152]
-A PREROUTING -p tcp --dport 443 -j REDIRECT --to-port 4043
-A OUTPUT -o lo -p tcp --dport 443 -j REDIRECT --to-port 4043
-A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8080
-A OUTPUT -o lo -p tcp --dport 80 -j REDIRECT --to-port 8080
COMMIT
*filter
:INPUT ACCEPT [662:1870237]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [614:87156]
COMMIT
"""

@hosts(root)
def apt_get_update():
    run("apt-get update")

@hosts(normal)
def make_dirs():
    run("mkdir -p /home/{user}/logs".format(user=user))
    run("mkdir -p /home/{user}/_priv".format(user=user))
    run("mkdir -p /home/{user}/website/".format(user=user))

@hosts(root)
def put_openssl_tooling():
    rsync_project(
        "/opt/",
        "/opt/openssl-1.0.2",
        extra_opts="-avz"
    )

@hosts(root)
def enable_ip_forwarding():
    put(
        local_path=StringIO("net.ipv4.ip_forward = 1"),
        remote_path="/etc/sysctl.d/60-ip-forward.conf"
    )
    run("service procps start")

@hosts(root)
def install_supervisord():
    run("apt-get install -y python-pip")
    run("pip install supervisor")

@hosts(root)
def update_supervisor_config():
    with settings(warn_only=True):
        run("service supervisord stop")

    put(
        local_path = StringIO(upstart_supervisord.format(user=user)),
        remote_path="/etc/init/supervisord.conf"
    )
    put(
        local_path = StringIO(supervisor_conf.format(user=user)),
        remote_path="/home/{user}/supervisor.conf".format(user=user)
    )
    with settings(warn_only=True):
        run("service supervisord start")

@hosts(root)
def update_iptables():
    run("apt-get install -y libgmp-dev iptables-persistent")
    put(
        local_path = StringIO(iptables_config),
        remote_path = "/etc/iptables/rules.v4"
    )
    run("service iptables-persistent restart")

@hosts(root)
def put_server_binary():
    with settings(warn_only=True):
        run("service supervisord stop")
    put(
        "dist/build/second-front-fs/second-front-fs",
        "/home/{user}/".format(user=user)
    )
    run("service supervisord start")

@hosts(normal)
def put_certificate():
    put(deploy_settings["certificate"]["cert"], "/home/{user}/_priv/".format(user=user))
    put(deploy_settings["certificate"]["key"],  "/home/{user}/_priv/".format(user=user))

@hosts(normal)
def rsync_site():
    rsync_project(
        "/home/{user}/".format(user=user),
        "website", 
        extra_opts="-avz"
        )

@hosts(root)
def all():
    apt_get_update()
    make_dirs()
    put_openssl_tooling()
    put_server_binary()
    install_supervisord()
    update_supervisor_config()
    put_certificate()
    rsync_site()