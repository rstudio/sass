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

