# start/finish_info() [plain]

    Code
      test_info()
    Message
      v test_info() done with x = 5

---

    Code
      test_info(keep = TRUE)
    Message
      i test_info() testing x = 5...
      v test_info() done with x = 5

---

    Code
      test_info(keep = TRUE, func = FALSE)
    Message
      i testing x = 5...
      v done with x = 5

---

    Code
      test_info(conditions = conditions)
    Message
      x test_info() done with x = 5 but encountered 1 warning and 1 error
        > ! in wrap_f(): my warning
        > x in internal_func(): my error

---

    Code
      test_info(conditions = conditions, show_conditions = FALSE)
    Message
      x test_info() done with x = 5 but encountered 1 warning and 1 error

---

    Code
      test_info(conditions = conditions[1, ])
    Message
      !  test_info() done with x = 5 but encountered 1 warning
        > ! in wrap_f(): my warning

---

    Code
      test_info(conditions = conditions[2, ])
    Message
      x test_info() done with x = 5 but encountered 1 error
        > x in internal_func(): my error

---

    Code
      test_info(conditions = conditions, abort_if_warnings = TRUE)
    Condition
      Error in `test_info()`:
      ! test_info() done with x = 5 but encountered 1 warning and 1 error
        > ! in wrap_f(): my warning
        > x in internal_func(): my error

---

    Code
      test_info(conditions = conditions, abort_if_errors = TRUE)
    Condition
      Error in `test_info()`:
      ! test_info() done with x = 5 but encountered 1 warning and 1 error
        > ! in wrap_f(): my warning
        > x in internal_func(): my error

# start/finish_info() [fancy]

    Code
      test_info()
    Message
      [32m✔[39m [1mtest_info()[22m done with [32mx = 5[39m

---

    Code
      test_info(keep = TRUE)
    Message
      [34mℹ[39m [1mtest_info()[22m testing [32mx = 5[39m...
      [32m✔[39m [1mtest_info()[22m done with [32mx = 5[39m

---

    Code
      test_info(keep = TRUE, func = FALSE)
    Message
      [34mℹ[39m testing [32mx = 5[39m...
      [32m✔[39m done with [32mx = 5[39m

---

    Code
      test_info(conditions = conditions)
    Message
      [31m✖[39m [1mtest_info()[22m done with [32mx = 5[39m but encountered [33m1 warning[39m and [31m1 error[39m
        → [33m![39m in [1mwrap_f()[22m: my warning
        → [31m✖[39m in [1minternal_func()[22m: my error

---

    Code
      test_info(conditions = conditions, show_conditions = FALSE)
    Message
      [31m✖[39m [1mtest_info()[22m done with [32mx = 5[39m but encountered [33m1 warning[39m and [31m1 error[39m

---

    Code
      test_info(conditions = conditions[1, ])
    Message
      [33m![39m [1mtest_info()[22m done with [32mx = 5[39m but encountered [33m1 warning[39m
        → [33m![39m in [1mwrap_f()[22m: my warning

---

    Code
      test_info(conditions = conditions[2, ])
    Message
      [31m✖[39m [1mtest_info()[22m done with [32mx = 5[39m but encountered [31m1 error[39m
        → [31m✖[39m in [1minternal_func()[22m: my error

---

    Code
      test_info(conditions = conditions, abort_if_warnings = TRUE)
    Condition
      [1m[33mError[39m in `test_info()`:[22m
      [1m[22m[33m![39m [1mtest_info()[22m done with [32mx = 5[39m but encountered [33m1 warning[39m and [31m1 error[39m
        → [33m![39m in [1mwrap_f()[22m: my warning
        → [31m✖[39m in [1minternal_func()[22m: my error

---

    Code
      test_info(conditions = conditions, abort_if_errors = TRUE)
    Condition
      [1m[33mError[39m in `test_info()`:[22m
      [1m[22m[33m![39m [1mtest_info()[22m done with [32mx = 5[39m but encountered [33m1 warning[39m and [31m1 error[39m
        → [33m![39m in [1mwrap_f()[22m: my warning
        → [31m✖[39m in [1minternal_func()[22m: my error

