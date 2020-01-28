#!/bin/sh

INSTALL_ROOT=${INSTALL_ROOT:-/usr/local}

install -d "$INSTALL_ROOT/bin"
install -d "$INSTALL_ROOT/lib/clpm"
install -d "$INSTALL_ROOT/man/man1"

install -T build/bin/clpm "$INSTALL_ROOT/bin/clpm"

for f in build/lib/clpm/*; do
    install -T "$f" "$INSTALL_ROOT/lib/clpm/$(basename "$f")"
done

for f in build/man/man1/*; do
    install -T "$f" "$INSTALL_ROOT/man/man1/$(basename "$f")"
done