
test_that("get_data() works", {

  # errors
  get_data() |> expect_error("must be.*list")
  get_data(42) |> expect_error("must be.*list")
  get_data(list()) |> expect_error("at least one")
  get_data(list(a = tibble()), by = 42) |> expect_error("must be.*character")
  get_data(list(a = tibble())) |> expect_error("no.*selections")
  get_data(list(a = tibble()), b = "a")  |> expect_error("dataset.*not in the data")
  get_data(list(a = tibble(), d = tibble()), a = "a") |> expect_error("error selecting columns")
  get_data(list(a = cars, b = cars), a = "speed", b = "dist") |>
    expect_error("unclear how to join")
  get_data(list(a = cars, b = cars |> mutate(speed = as.character(speed))), 
           a = "speed", b = "dist", by = "speed") |>
    expect_error("unclear how to join")
  get_data(list(a = cars, b = cars), a = everything(), b = everything(), by = "speed") |>
    expect_error("encountered warning") # many-to-many relationship
  
  # works
  list(
    a = tibble(id = c("a", "b"), info = paste(id, "info")), 
    b = tibble(id = "a", x = 1:10, y = 42), 
    data = tibble(id = "a", x = 1:10, z = x * 10)
  ) |> 
    get_data(a = everything(), b = c("id", "x"), data = everything(), by = c("id", "x")) |>
    expect_snapshot()
  
  get_data(list(a = cars, b = cars), a = everything(), b = everything(), by = "speed", relationship = "many-to-many") |>
    expect_snapshot()
})
