#!/bin/sh

tar cvf build/clpm.tar build/bin/ build/lib/ build/man/ LICENSE README.org install.sh
gzip build/clpm.tar
