#!/bin/bash
cd /home/qwe12345678/1206/Typus
./dist-newstyle/build/x86_64-linux/ghc-9.6.3/typus-0.12.0/x/typus/build/typus/typus debug <<EOF
help
breakpoint set Parser.parseTypus
breakpoint list
log level debug
stats
exit
EOF