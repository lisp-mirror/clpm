This directory contains the licenses for every dependency of CLPM that is
included in it when built. When possible, the license files are just symlinks to
the license files provided by that dependency's upstream. When that is not
possible, the file is a plain text file containing the license as extracted from
one of its source files and/or system definition file.

During the build process, these licenses are included in the generated image and
can be printed with `clpm license-info`.
