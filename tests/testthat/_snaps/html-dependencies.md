# sass()/as_sass() relay html dependencies

    Code
      sass(scss)
    Output
      /* CSS */
      body {
        color: green;
      }
      
      
      /* HTML Dependencies */
      List of 1
       $ :List of 10
        ..$ name      : chr "fake1"
        ..$ version   : chr "1.0.0"
        ..$ src       :List of 1
        .. ..$ file: chr ""
        ..$ meta      : NULL
        ..$ script    : NULL
        ..$ stylesheet: NULL
        ..$ head      : NULL
        ..$ attachment: NULL
        ..$ package   : NULL
        ..$ all_files : logi TRUE
        ..- attr(*, "class")= chr "html_dependency"

---

    Code
      as_sass(scss)
    Output
      /* Sass */
      body{color: green}
      
      /* HTML Dependencies */
      List of 1
       $ :List of 10
        ..$ name      : chr "fake1"
        ..$ version   : chr "1.0.0"
        ..$ src       :List of 1
        .. ..$ file: chr ""
        ..$ meta      : NULL
        ..$ script    : NULL
        ..$ stylesheet: NULL
        ..$ head      : NULL
        ..$ attachment: NULL
        ..$ package   : NULL
        ..$ all_files : logi TRUE
        ..- attr(*, "class")= chr "html_dependency"

