# safety checks work

    Code
      define_pkg_option(default = 42L, check_fn = function(x) stop("issue"))
    Condition
      Error in `define_pkg_option()`:
      ! there was a problem while checking if default is valid for option
      Caused by error:
      ! issue

---

    Code
      define_pkg_option(default = 42L, check_fn = function(x) {
        return("not T/F")
      })
    Condition
      Error in `define_pkg_option()`:
      ! option check function `function(x) ...` returned a string instead of TRUE/FALSE when checking `default`

---

    Code
      define_pkg_option(default = 42L, check_fn = function(x) {
        return(FALSE)
      })
    Condition
      Error in `define_pkg_option()`:
      ! invalid value for option
      i `default` is an integer
      x the check function `function(x) ...` returned FALSE for this value

# functionality works

    Code
      set_pkg_option("c", "42", "pkg", .options)
    Condition
      Error:
      ! invalid value for option c
      i `value` is a string
      x the check function `is_integerish` returned FALSE for this value

---

    Code
      get_pkg_option("c", "pkg", .options)
    Condition
      Error:
      ! invalid value for option c
      i `value` is a string
      x the check function `is_integerish` returned FALSE for this value

