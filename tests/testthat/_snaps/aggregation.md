# get_data() [plain]

    Code
      out <- get_data(list(a = tibble(id = c("a", "b"), info = paste(id, "info")), b = tibble(
        id = "a", x = 1:10, y = 42), data = tibble(id = "a", x = 1:10, z = x * 10)),
      a = everything(), b = c("id", "x"), data = everything(), by = c("id", "x"))
    Message
      v eval() retrieved 10 records from the combination of a (2), b (10), and data
      (10) via id and x

# get_data() [fancy]

    Code
      out <- get_data(list(a = tibble(id = c("a", "b"), info = paste(id, "info")), b = tibble(
        id = "a", x = 1:10, y = 42), data = tibble(id = "a", x = 1:10, z = x * 10)),
      a = everything(), b = c("id", "x"), data = everything(), by = c("id", "x"))
    Message
      [32m✔[39m [1meval()[22m retrieved 10 records from the combination of [34ma[39m (2), [34mb[39m (10), and [34mdata[39m
      (10) via [32mid[39m and [32mx[39m

