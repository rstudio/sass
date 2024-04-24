# Remote font importing basically works

    Code
      tagz$html
    Output
      <style>body {
        font-family: foo;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"bar\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: foo;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"bar\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: foo;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      NULL

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: foo;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      NULL

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: Pacifico;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=Pacifico&amp;display=swap\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: Pacifico;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=Pacifico&amp;display=auto\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: "Crimson Pro";
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=Crimson%20Pro:wght@200..900&amp;display=swap\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: "Crimson Pro";
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=Crimson%20Pro:ital,wght@1,200..900&amp;display=swap\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: "Crimson Pro";
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=Crimson%20Pro:ital,wght@1,400;1,500&amp;display=swap\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: "Crimson Pro";
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=Crimson%20Pro:ital,wght@0,400;0,500;1,400;1,500&amp;display=swap\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: "Crimson Pro";
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=Crimson%20Pro:ital,wght@0,400;0,500;0,600;1,400;1,500;1,600&amp;display=swap\" rel=\"stylesheet\"/>"

# font_google(local = TRUE) basically works

    Code
      tagz <- renderTags(tags$style(sass(scss)))
    Message
      Downloading google font Pacifico to local cache (<temp-cache>)

---

    Code
      tagz <- renderTags(tags$style(sass(scss)))

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: Pacifico;
      }
      </style>

# Special named args in font_collection

    Code
      tagz$html
    Output
      <style>body {
        font-family: foo;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=foo&amp;display=swap\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: foo;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"bar\" rel=\"stylesheet\"/>"

---

    Code
      tagz$html
    Output
      <style>body {
        font-family: foo;
      }
      </style>

---

    Code
      tagz$dependencies[[1]]$head
    Output
      [1] "<link href=\"https://fonts.googleapis.com/css2?family=foo&amp;display=swap\" rel=\"stylesheet\"/>"

