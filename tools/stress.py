
import subprocess as sp 
import os.path 
import os 
import sys 
import signal


RUN_TIMES = 20

many_headers = [
    "-H", "X-rnd-header: j33 3434 4 434 3sfsadaffdfsd",
    "-H", "X-rnd-header: jsdfsd f3234ra gfdgcxmkllyrewd",
    "-H", "X-rnd-header: dsaddasd f34 23441 dfvsdfvsd",
    "-H", "X-rnd-header: 34wt40gdfbmcxerprlhhfktglq2l;2l",
    "-H", "X-rnd-header: 309002jvbmokm,wqlqwewrektjektt",
    "-H", "X-rnd-header: j33 adf4 4 434 3sfsadaffdfsd",
    "-H", "X-rnd-header: j3dfhd f3234ra gfdgcxmkllyrewd",
    "-H", "X-rnd-header: dsiresd f34 23441 dfvsdfvsd",
    "-H", "X-rnd-header: xhhdfbmcxerprlhhfktglq2l;2l",
    "-H", "X-rnd-header: 309002jvbmokm,wqlqwewffasdtt",    
    "-H", "X-rnd-header: j33 3affa34 4 43xf1 3sfeadaffdfsd",
    "-H", "X-rnd-header: jsdfsdaffaf3234raxf1gfdgexmkllyrewd",
    "-H", "X-rnd-header: dsaddaaffad f34 2xf1441 efvsdfvsd",
    "-H", "X-rnd-header: 34wt40affadfbmcxexf1prlhefktglq2l;2l",
    "-H", "X-rnd-header: 309002affavbmokm,xf1qlqwewrektjektt",
    "-H", "X-rnd-header: j33 adaffa4 4 434xf13sfsedaffdfsd",
    "-H", "X-rnd-header: j3dfhdaffaf3234raxf1gfdgexmkllyrewd",
    "-H", "X-rnd-header: dsiresaffa f34 23xf141 devsdfvsd",
    "-H", "X-rnd-header: xhhdfbaffacxerprlxf1hfktelq2l;2l",
    "-H", "X-rnd-header: 309002affavbmokm,xf1qlqwewffasdtt",   
    ]

def run_server():
    p = sp.Popen([
        "dist/build/second-front-fs/second-front-fs",
        "+RTS", "-Pa",
        ],
        #stderr=open("/dev/null", "a")
        )
    return p 

def run_client():
    for i in range(RUN_TIMES):
        p = sp.check_call([
            "curl", "--http2", "-s", "-k", "https://www.httpdos.com:8080/sample-website/zero.html"
            ] + many_headers[:8],
            stdout=open("/dev/null", "a")
            )


p = run_server()
run_client()
p.send_signal(signal.SIGINT)
p.wait()
print(p.returncode)