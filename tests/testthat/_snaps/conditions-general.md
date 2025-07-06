# try_catch_cnds() works

    Code
      show_cnds(out$conditions)
    Message
      x in `eval()`: 3 warnings and 1 error
        > ! in `wrap_f()`: we're here!
        > ! no context
        > ! in `my_f()`: trouble!
        > x in `internal_func()`: oh no internal error!

---

    Code
      show_cnds(out$conditions, include_call = FALSE, summary_message = "hello {.pkg isoorbi}")
    Message
      x 3 warnings and 1 error hello isoorbi
        > ! in `wrap_f()`: we're here!
        > ! no context
        > ! in `my_f()`: trouble!
        > x in `internal_func()`: oh no internal error!

---

    Code
      show_cnds(out$conditions, include_summary = FALSE, include_cnd_calls = FALSE)
    Message
      ! we're here!
      ! no context
      ! trouble!
      x oh no internal error!

