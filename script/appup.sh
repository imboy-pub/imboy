#!/bin/bash

# vsn=0.2.7
mkdir -p /usr/local/imboy/releases/$vsn/ && cp -Rf _rel/imboy/imboy-$vsn.tar.gz /usr/local/imboy/releases/$vsn/
echo /usr/local/imboy/releases/$vsn/;
/usr/local/imboy/bin/imboy upgrade $vsn

/usr/local/imboy/bin/imboy pid

/usr/local/imboy/bin/imboy versions
