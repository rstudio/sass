# sass layer format

    Code
      format(core)
    Output
      [1] "@function my_invert($color, $amount: 100%) {\n    $inverse: change-color($color, $hue: hue($color) + 180);\n    @return mix($inverse, $color, $amount);\n  }\n$color: blue !default;\nbody { background-color: $color; color: my_invert($color); }"

---

    Code
      core
    Output
      /* Sass Bundle */
      @function my_invert($color, $amount: 100%) {
          $inverse: change-color($color, $hue: hue($color) + 180);
          @return mix($inverse, $color, $amount);
        }
      $color: blue !default;
      body { background-color: $color; color: my_invert($color); }
      /* *** */

# sass_layer_file() basically works

    Code
      str(sass_layer_file(f))
    Output
      List of 1
       $ layers:List of 1
        ..$ :List of 7
        .. ..$ functions       : chr [1:7] "@function color-contrast($color) {" "  @return if(" "    red($color) * 0.299 + green($color) * 0.587 + blue($color) * 0.114 > 186," "    black, white" ...
        .. ..$ defaults        : chr [1:2] "$body-color:            color-contrast($body-bg) !default;" ""
        .. ..$ mixins          : chr [1:4] "@mixin body-color {" "  color: $body-color" "}" ""
        .. ..$ rules           : chr [1:3] "body {" "  @include body-color;" "}"
        .. ..$ declarations    : NULL
        .. ..$ html_deps       : NULL
        .. ..$ file_attachments: chr(0) 
        .. ..- attr(*, "class")= chr [1:2] "sass_layer" "list"
       - attr(*, "class")= chr "sass_bundle"

