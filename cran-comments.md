## Test environments
* local OS X install, R 3.5.0
* ubuntu 18.04 (docker), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 4 notes

* This is a new release.

* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

GNU make is used to build the underlying libSass library.

* checking pragmas in C/C++ headers and code ... NOTE
  File which contains pragma(s) suppressing diagnostics:
    ‘src/libsass/src/parser.hpp’

This pragma is a part of the libSass library.

* checking installed package size ... NOTE
    installed size is  6.4Mb
    sub-directories of 1Mb or more:
      libs   6.3Mb

This is only a note on Windows builds.

## Downstream dependencies


Failed `build_win`:
```
* installing *source* package 'sass' ...
** libs

*** arch - i386
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc  -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O3 -Wall  -std=gnu99 -mtune=core2 -c compile.c -o compile.o
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc  -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O3 -Wall  -std=gnu99 -mtune=core2 -c create_string.c -o create_string.o
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc  -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O3 -Wall  -std=gnu99 -mtune=core2 -c init.c -o init.o
MAKEFLAGS= CC=d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc  CXX=d:/Compiler/gcc-4.9.3/mingw_32/bin/g++  AR=d:/Compiler/gcc-4.9.3/mingw_32/bin/ar /usr/bin/make -C libsass -j5
make[1]: Entering directory `/cygdrive/d/temp/RtmpQZjAJf/R.INSTALL47e86c7e7ca3/sass/src-i386/libsass'
mkdir lib
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc -Wall -O2 -I include -c -o src/cencode.o src/cencode.c
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/ast.o src/ast.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/node.o src/node.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/context.o src/context.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/constants.o src/constants.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_utils.o src/fn_utils.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_miscs.o src/fn_miscs.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_maps.o src/fn_maps.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_lists.o src/fn_lists.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_colors.o src/fn_colors.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_numbers.o src/fn_numbers.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_strings.o src/fn_strings.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_selectors.o src/fn_selectors.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/functions.o src/functions.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/color_maps.o src/color_maps.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/environment.o src/environment.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/ast_fwd_decl.o src/ast_fwd_decl.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/bind.o src/bind.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/file.o src/file.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/util.o src/util.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/json.o src/json.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/units.o src/units.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/values.o src/values.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/plugins.o src/plugins.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/position.o src/position.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/lexer.o src/lexer.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/parser.o src/parser.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/prelexer.o src/prelexer.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/eval.o src/eval.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/expand.o src/expand.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/listize.o src/listize.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/cssize.o src/cssize.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/extend.o src/extend.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/output.o src/output.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/inspect.o src/inspect.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/emitter.o src/emitter.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/check_nesting.o src/check_nesting.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/remove_placeholders.o src/remove_placeholders.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass.o src/sass.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass_util.o src/sass_util.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass_values.o src/sass_values.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass_context.o src/sass_context.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass_functions.o src/sass_functions.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass2scss.o src/sass2scss.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/backtrace.o src/backtrace.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/operators.o src/operators.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/ast2c.o src/ast2c.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/c2ast.o src/c2ast.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/to_value.o src/to_value.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/source_map.o src/source_map.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/subset_map.o src/subset_map.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/error_handling.o src/error_handling.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/memory/SharedPtr.o src/memory/SharedPtr.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/utf8_string.o src/utf8_string.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/base64vlq.o src/base64vlq.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/ar rcvs lib/libsass.a src/cencode.o src/ast.o src/node.o src/context.o src/constants.o src/fn_utils.o src/fn_miscs.o src/fn_maps.o src/fn_lists.o src/fn_colors.o src/fn_numbers.o src/fn_strings.o src/fn_selectors.o src/functions.o src/color_maps.o src/environment.o src/ast_fwd_decl.o src/bind.o src/file.o src/util.o src/json.o src/units.o src/values.o src/plugins.o src/position.o src/lexer.o src/parser.o src/prelexer.o src/eval.o src/expand.o src/listize.o src/cssize.o src/extend.o src/output.o src/inspect.o src/emitter.o src/check_nesting.o src/remove_placeholders.o src/sass.o src/sass_util.o src/sass_values.o src/sass_context.o src/sass_functions.o src/sass2scss.o src/backtrace.o src/operators.o src/ast2c.o src/c2ast.o src/to_value.o src/source_map.o src/subset_map.o src/error_handling.o src/memory/SharedPtr.o src/utf8_string.o src/base64vlq.o
a - src/cencode.o
a - src/ast.o
a - src/node.o
a - src/context.o
a - src/constants.o
a - src/fn_utils.o
a - src/fn_miscs.o
a - src/fn_maps.o
a - src/fn_lists.o
a - src/fn_colors.o
a - src/fn_numbers.o
a - src/fn_strings.o
a - src/fn_selectors.o
a - src/functions.o
a - src/color_maps.o
a - src/environment.o
a - src/ast_fwd_decl.o
a - src/bind.o
a - src/file.o
a - src/util.o
a - src/json.o
a - src/units.o
a - src/values.o
a - src/plugins.o
a - src/position.o
a - src/lexer.o
a - src/parser.o
a - src/prelexer.o
a - src/eval.o
a - src/expand.o
a - src/listize.o
a - src/cssize.o
a - src/extend.o
a - src/output.o
a - src/inspect.o
a - src/emitter.o
a - src/check_nesting.o
a - src/remove_placeholders.o
a - src/sass.o
a - src/sass_util.o
a - src/sass_values.o
a - src/sass_context.o
a - src/sass_functions.o
a - src/sass2scss.o
a - src/backtrace.o
a - src/operators.o
a - src/ast2c.o
a - src/c2ast.o
a - src/to_value.o
a - src/source_map.o
a - src/subset_map.o
a - src/error_handling.o
a - src/memory/SharedPtr.o
a - src/utf8_string.o
a - src/base64vlq.o
make[1]: Leaving directory `/cygdrive/d/temp/RtmpQZjAJf/R.INSTALL47e86c7e7ca3/sass/src-i386/libsass'
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc -shared -s -static-libgcc -o sass.dll tmp.def compile.o create_string.o init.o ./libsass/lib/libsass.a -lstdc++ -Ld:/Compiler/gcc-4.9.3/local330/lib/i386 -Ld:/Compiler/gcc-4.9.3/local330/lib -LD:/RCompile/recent/R/bin/i386 -lR
installing to d:/RCompile/CRANguest/R-devel/lib/sass/libs/i386

*** arch - x64
d:/Compiler/gcc-4.9.3/mingw_64/bin/gcc -m64 -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O2 -Wall  -std=gnu99 -mtune=core2 -c compile.c -o compile.o
d:/Compiler/gcc-4.9.3/mingw_64/bin/gcc -m64 -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O2 -Wall  -std=gnu99 -mtune=core2 -c create_string.c -o create_string.o
d:/Compiler/gcc-4.9.3/mingw_64/bin/gcc -m64 -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O2 -Wall  -std=gnu99 -mtune=core2 -c init.c -o init.o
MAKEFLAGS= CC=d:/Compiler/gcc-4.9.3/mingw_64/bin/gcc -m64 CXX=d:/Compiler/gcc-4.9.3/mingw_64/bin/g++ -m64 AR=d:/Compiler/gcc-4.9.3/mingw_64/bin/ar /usr/bin/make -C libsass -j5
-m64: not found
make: *** [libsass/lib/libsass.a] Error 127
ERROR: compilation failed for package 'sass'
* removing 'd:/RCompile/CRANguest/R-devel/lib/sass'
In R CMD INSTALL
```

## Comments 2
### Check

```
* using log directory 'd:/RCompile/CRANguest/R-devel/sassr.Rcheck'
* using R Under development (unstable) (2018-08-13 r75131)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: ISO8859-1
* checking for file 'sassr/DESCRIPTION' ... OK
* checking extension type ... Package
* this is package 'sassr' version '0.1.0'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Timothy Mastny <tim.mastny@gmail.com>'

New submission
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking serialization versions ... OK
* checking whether package 'sassr' can be installed ... ERROR
Installation failed.
See 'd:/RCompile/CRANguest/R-devel/sassr.Rcheck/00install.out' for details.
* DONE
Status: 1 ERROR, 1 NOTE
```

```
* installing *source* package 'sassr' ...
** libs

*** arch - i386
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc  -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O3 -Wall  -std=gnu99 -mtune=core2 -c compile.c -o compile.o
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc  -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O3 -Wall  -std=gnu99 -mtune=core2 -c create_string.c -o create_string.o
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc  -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O3 -Wall  -std=gnu99 -mtune=core2 -c init.c -o init.o
MAKEFLAGS= CC=d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc  CXX=d:/Compiler/gcc-4.9.3/mingw_32/bin/g++  AR=d:/Compiler/gcc-4.9.3/mingw_32/bin/ar /usr/bin/make -C libsass
make[1]: Entering directory `/cygdrive/d/temp/Rtmp0sLAgz/R.INSTALL1e654584363be/sassr/src-i386/libsass'
mkdir lib
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc -Wall -O2 -I include -c -o src/cencode.o src/cencode.c
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/ast.o src/ast.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/node.o src/node.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/context.o src/context.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/constants.o src/constants.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_utils.o src/fn_utils.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_miscs.o src/fn_miscs.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_maps.o src/fn_maps.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_lists.o src/fn_lists.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_colors.o src/fn_colors.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_numbers.o src/fn_numbers.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_strings.o src/fn_strings.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/fn_selectors.o src/fn_selectors.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/functions.o src/functions.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/color_maps.o src/color_maps.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/environment.o src/environment.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/ast_fwd_decl.o src/ast_fwd_decl.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/bind.o src/bind.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/file.o src/file.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/util.o src/util.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/json.o src/json.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/units.o src/units.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/values.o src/values.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/plugins.o src/plugins.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/position.o src/position.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/lexer.o src/lexer.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/parser.o src/parser.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/prelexer.o src/prelexer.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/eval.o src/eval.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/expand.o src/expand.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/listize.o src/listize.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/cssize.o src/cssize.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/extend.o src/extend.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/output.o src/output.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/inspect.o src/inspect.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/emitter.o src/emitter.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/check_nesting.o src/check_nesting.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/remove_placeholders.o src/remove_placeholders.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass.o src/sass.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass_util.o src/sass_util.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass_values.o src/sass_values.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass_context.o src/sass_context.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass_functions.o src/sass_functions.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/sass2scss.o src/sass2scss.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/backtrace.o src/backtrace.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/operators.o src/operators.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/ast2c.o src/ast2c.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/c2ast.o src/c2ast.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/to_value.o src/to_value.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/source_map.o src/source_map.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/subset_map.o src/subset_map.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/error_handling.o src/error_handling.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/memory/SharedPtr.o src/memory/SharedPtr.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/utf8_string.o src/utf8_string.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/g++ -Wall -O2 -std=c++0x -I include -c -o src/base64vlq.o src/base64vlq.cpp
d:/Compiler/gcc-4.9.3/mingw_32/bin/ar rcvs lib/libsass.a src/cencode.o src/ast.o src/node.o src/context.o src/constants.o src/fn_utils.o src/fn_miscs.o src/fn_maps.o src/fn_lists.o src/fn_colors.o src/fn_numbers.o src/fn_strings.o src/fn_selectors.o src/functions.o src/color_maps.o src/environment.o src/ast_fwd_decl.o src/bind.o src/file.o src/util.o src/json.o src/units.o src/values.o src/plugins.o src/position.o src/lexer.o src/parser.o src/prelexer.o src/eval.o src/expand.o src/listize.o src/cssize.o src/extend.o src/output.o src/inspect.o src/emitter.o src/check_nesting.o src/remove_placeholders.o src/sass.o src/sass_util.o src/sass_values.o src/sass_context.o src/sass_functions.o src/sass2scss.o src/backtrace.o src/operators.o src/ast2c.o src/c2ast.o src/to_value.o src/source_map.o src/subset_map.o src/error_handling.o src/memory/SharedPtr.o src/utf8_string.o src/base64vlq.o
a - src/cencode.o
a - src/ast.o
a - src/node.o
a - src/context.o
a - src/constants.o
a - src/fn_utils.o
a - src/fn_miscs.o
a - src/fn_maps.o
a - src/fn_lists.o
a - src/fn_colors.o
a - src/fn_numbers.o
a - src/fn_strings.o
a - src/fn_selectors.o
a - src/functions.o
a - src/color_maps.o
a - src/environment.o
a - src/ast_fwd_decl.o
a - src/bind.o
a - src/file.o
a - src/util.o
a - src/json.o
a - src/units.o
a - src/values.o
a - src/plugins.o
a - src/position.o
a - src/lexer.o
a - src/parser.o
a - src/prelexer.o
a - src/eval.o
a - src/expand.o
a - src/listize.o
a - src/cssize.o
a - src/extend.o
a - src/output.o
a - src/inspect.o
a - src/emitter.o
a - src/check_nesting.o
a - src/remove_placeholders.o
a - src/sass.o
a - src/sass_util.o
a - src/sass_values.o
a - src/sass_context.o
a - src/sass_functions.o
a - src/sass2scss.o
a - src/backtrace.o
a - src/operators.o
a - src/ast2c.o
a - src/c2ast.o
a - src/to_value.o
a - src/source_map.o
a - src/subset_map.o
a - src/error_handling.o
a - src/memory/SharedPtr.o
a - src/utf8_string.o
a - src/base64vlq.o
make[1]: Leaving directory `/cygdrive/d/temp/Rtmp0sLAgz/R.INSTALL1e654584363be/sassr/src-i386/libsass'
d:/Compiler/gcc-4.9.3/mingw_32/bin/gcc -shared -s -static-libgcc -o sassr.dll tmp.def compile.o create_string.o init.o ./libsass/lib/libsass.a -lstdc++ -Ld:/Compiler/gcc-4.9.3/local330/lib/i386 -Ld:/Compiler/gcc-4.9.3/local330/lib -LD:/RCompile/recent/R/bin/i386 -lR
installing to d:/RCompile/CRANguest/R-devel/lib/sassr/libs/i386

*** arch - x64
d:/Compiler/gcc-4.9.3/mingw_64/bin/gcc -m64 -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O2 -Wall  -std=gnu99 -mtune=core2 -c compile.c -o compile.o
d:/Compiler/gcc-4.9.3/mingw_64/bin/gcc -m64 -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O2 -Wall  -std=gnu99 -mtune=core2 -c create_string.c -o create_string.o
d:/Compiler/gcc-4.9.3/mingw_64/bin/gcc -m64 -I"D:/RCompile/recent/R/include" -DNDEBUG -I./libsass/include    -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O2 -Wall  -std=gnu99 -mtune=core2 -c init.c -o init.o
MAKEFLAGS= CC=d:/Compiler/gcc-4.9.3/mingw_64/bin/gcc -m64 CXX=d:/Compiler/gcc-4.9.3/mingw_64/bin/g++ -m64 AR=d:/Compiler/gcc-4.9.3/mingw_64/bin/ar /usr/bin/make -C libsass
-m64: not found
make: *** [libsass/lib/libsass.a] Error 127
ERROR: compilation failed for package 'sassr'
* removing 'd:/RCompile/CRANguest/R-devel/lib/sassr'
In R CMD INSTALL
```

