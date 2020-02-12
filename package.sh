#!/bin/sh

BASE="${1:-.}"
tar -cvf "$BASE/build/clpm.tar" "$BASE/build/bin/" "$BASE/build/lib/" "$BASE/build/man/" "$BASE/LICENSE" "$BASE/README.org" "$BASE/install.sh"
gzip "$BASE/build/clpm.tar"
