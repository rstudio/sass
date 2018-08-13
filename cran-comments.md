## Test environments
* local OS X install, R 3.5.0
* ubuntu 18.04 (docker), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 3 notes

* This is a new release.

* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

GNU make is used to build the underlying libSass library.

* checking pragmas in C/C++ headers and code ... NOTE
  File which contains pragma(s) suppressing diagnostics:
    ‘src/libsass/src/parser.hpp’

This pragma is a part of the libSass library.

## Downstream dependencies


