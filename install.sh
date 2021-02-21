#!/bin/sh

INSTALL_ROOT=${INSTALL_ROOT:-/usr/local}

install_directory() {
  install -d "$INSTALL_ROOT/$1"
  find "$1" -maxdepth 1 -type f -exec install {} "$INSTALL_ROOT/$1/" \;
  # I don't think this works with hidden directories, but we have none of
  # those...
  for f in "build/$1"/*; do
    if [ -d "$f" ]; then
      install_directory "$1/$(basename "$f")"
    fi
  done

}

install_directory bin
install_directory share

# Local Variables:
# sh-basic-offset: 2
# End:
