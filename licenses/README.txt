This directory contains the licenses for every dependency of CLPM that is
included in it when built. The license should be copied into this directory,
not symlinked, in order to make sure everything works appropriately on
Windows. Ideally, upstream has a LICENSE file that can be copied, but when
that's not possible, the appropriate license should be determined based on the
source file headers and/or system definition file.

During the build process, these licenses are included in the generated image and
can be printed with `clpm license-info`.
