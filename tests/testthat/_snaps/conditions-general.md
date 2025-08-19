# try_catch_cnds()

    Code
      try_catch_cnds(wrap_f())$conditions$condition[[4]]$trace
    Output
          x
       1. \-isoorbi (local) wrap_f() at isoorbi/R/conditions-general.R:86:5
       2.   \-isoorbi (local) my_f(show_message = show_message) at test-conditions-general.R:5:3
       3.     \-isoorbi (local) internal_func() at test-conditions-general.R:18:3
       4.       \-rlang::abort("oh no internal {value} error!") at test-conditions-general.R:16:5

---

    Code
      try_catch_cnds(wrap_f(), truncate_call_stack = FALSE)$conditions$condition[[4]]$
        trace
    Output
           x
        1. +-isoorbi:::try_catch_cnds(wrap_f(), truncate_call_stack = FALSE)
        2. | +-base::tryCatch(...) at isoorbi/R/conditions-general.R:86:5
        3. | | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        4. | |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        5. | |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        6. | \-base::withCallingHandlers(warning = handle_warning, expr) at isoorbi/R/conditions-general.R:86:5
        7. \-isoorbi (local) wrap_f() at isoorbi/R/conditions-general.R:86:5
        8.   \-isoorbi (local) my_f(show_message = show_message) at test-conditions-general.R:5:3
        9.     \-isoorbi (local) internal_func() at test-conditions-general.R:18:3
       10.       \-rlang::abort("oh no internal {value} error!") at test-conditions-general.R:16:5

---

    Code
      out$conditions
    Output
      # A tibble: 4 x 4
        type    call          message                                       condition 
        <chr>   <chr>         <chr>                                         <list>    
      1 warning wrap_f        we're here!                                   <smplWrnn>
      2 warning <NA>          no context                                    <rlng_wrn>
      3 warning my_f          long trouble! This sentences is easily longe~ <smplWrnn>
      4 error   internal_func oh no internal {value} error!                 <rlng_rrr>

# summarize_cnds() [plain]

    Code
      cli_bullets(summarize_cnds(out$conditions, .call = NULL))
    Message
      x 3 warnings and 1 error

---

    Code
      test_summarize_cnds()
    Message
      x in test_summarize_cnds(): 3 warnings and 1 error

---

    Code
      test_summarize_cnds(include_symbol = FALSE)
    Message
      in test_summarize_cnds(): 3 warnings and 1 error

---

    Code
      test_summarize_cnds(include_call = FALSE)
    Message
      x 3 warnings and 1 error

---

    Code
      test_summarize_cnds(conditions = out$conditions[1:3, ])
    Message
      ! in test_summarize_cnds(): 3 warnings

---

    Code
      test_summarize_cnds(conditions = out$conditions[c(), ])
    Message
      v in test_summarize_cnds(): no issues

---

    Code
      test_summarize_cnds(call_format = "hey {.fn {call}}! ")
    Message
      x hey `test_summarize_cnds()`! 3 warnings and 1 error

---

    Code
      test_summarize_cnds(message = "and we get more {fun}")
    Message
      x in test_summarize_cnds(): 3 warnings and 1 error and we get more {fun}

---

    Code
      test_summarize_cnds(message = format_inline("so this is {.field field}"))
    Message
      x in test_summarize_cnds(): 3 warnings and 1 error so this is field

---

    Code
      test_summarize_cnds(summary_format = "{message} = {issues} {.strong yeah!}")
    Message
      x in test_summarize_cnds(): = 3 warnings and 1 error yeah!

# summarize_cnds() [fancy]

    Code
      cli_bullets(summarize_cnds(out$conditions, .call = NULL))
    Message
      [31m✖[39m [33m3 warnings[39m and [31m1 error[39m

---

    Code
      test_summarize_cnds()
    Message
      [31m✖[39m in [1mtest_summarize_cnds()[22m: [33m3 warnings[39m and [31m1 error[39m

---

    Code
      test_summarize_cnds(include_symbol = FALSE)
    Message
      in [1mtest_summarize_cnds()[22m: [33m3 warnings[39m and [31m1 error[39m

---

    Code
      test_summarize_cnds(include_call = FALSE)
    Message
      [31m✖[39m [33m3 warnings[39m and [31m1 error[39m

---

    Code
      test_summarize_cnds(conditions = out$conditions[1:3, ])
    Message
      [33m![39m in [1mtest_summarize_cnds()[22m: [33m3 warnings[39m

---

    Code
      test_summarize_cnds(conditions = out$conditions[c(), ])
    Message
      [32m✔[39m in [1mtest_summarize_cnds()[22m: [32mno issues[39m

---

    Code
      test_summarize_cnds(call_format = "hey {.fn {call}}! ")
    Message
      [31m✖[39m hey `test_summarize_cnds()`! [33m3 warnings[39m and [31m1 error[39m

---

    Code
      test_summarize_cnds(message = "and we get more {fun}")
    Message
      [31m✖[39m in [1mtest_summarize_cnds()[22m: [33m3 warnings[39m and [31m1 error[39m and we get more {fun}

---

    Code
      test_summarize_cnds(message = format_inline("so this is {.field field}"))
    Message
      [31m✖[39m in [1mtest_summarize_cnds()[22m: [33m3 warnings[39m and [31m1 error[39m so this is [32mfield[39m

---

    Code
      test_summarize_cnds(summary_format = "{message} = {issues} {.strong yeah!}")
    Message
      [31m✖[39m in [1mtest_summarize_cnds()[22m: = [33m3 warnings[39m and [31m1 error[39m [1myeah![22m

# format_cnds() [plain]

    Code
      format_cnds(out$conditions)
    Output
      [1] "! in wrap_f(): we're here!"                                                     
      [2] "! no context"                                                                   
      [3] "! in my_f(): long trouble! This sentences is easily longer than a single line"  
      [4] "in standard line width and therefore needs a sensible linebreak somewhere along"
      [5] "the way."                                                                       
      [6] "x in internal_func(): oh no internal {{value}} error!"                          

---

    Code
      format_cnds(out$conditions, include_symbol = FALSE)
    Output
      [1] "in wrap_f(): we're here!"                                                      
      [2] "no context"                                                                    
      [3] "in my_f(): long trouble! This sentences is easily longer than a single line in"
      [4] "standard line width and therefore needs a sensible linebreak somewhere along"  
      [5] "the way."                                                                      
      [6] "in internal_func(): oh no internal {{value}} error!"                           

---

    Code
      format_cnds(out$conditions, include_call = FALSE)
    Output
      [1] "! we're here!"                                                                 
      [2] "! no context"                                                                  
      [3] "! long trouble! This sentences is easily longer than a single line in standard"
      [4] "line width and therefore needs a sensible linebreak somewhere along the way."  
      [5] "x oh no internal {{value}} error!"                                             

---

    Code
      format_cnds(out$conditions, call_format = "hey {.fn {call}}! ")
    Output
      [1] "! hey `wrap_f()`! we're here!"                                                 
      [2] "! no context"                                                                  
      [3] "! hey `my_f()`! long trouble! This sentences is easily longer than a single"   
      [4] "line in standard line width and therefore needs a sensible linebreak somewhere"
      [5] "along the way."                                                                
      [6] "x hey `internal_func()`! oh no internal {{value}} error!"                      

---

    Code
      format_cnds(out$conditions, indent = 1)
    Output
                                                                                      
                                                         "! in wrap_f(): we're here!" 
                                                                                      
                                                                       "! no context" 
                                                                                      
      "! in my_f(): long trouble! This sentences is easily longer than a single line" 
                                                                                      
          "in standard line width and therefore needs a sensible linebreak somewhere" 
                                                                                      
                                                                     "along the way." 
                                                                                      
                              "x in internal_func(): oh no internal {{value}} error!" 

---

    Code
      format_cnds(out$conditions, indent = 3)
    Output
                                                                                     
                                                    "    ! in wrap_f(): we're here!" 
                                                                                     
                                                                  "    ! no context" 
                                                                                     
      "    ! in my_f(): long trouble! This sentences is easily longer than a single" 
                                                                                     
          "    line in standard line width and therefore needs a sensible linebreak" 
                                                                                     
                                                      "    somewhere along the way." 
                                                                                     
                         "    x in internal_func(): oh no internal {{value}} error!" 

---

    Code
      format_cnds(out$conditions, prefix = "an {x} ")
    Output
      [1] "an {{x}} ! in wrap_f(): we're here!"                                        
      [2] "an {{x}} ! no context"                                                      
      [3] "an {{x}} ! in my_f(): long trouble! This sentences is easily longer than a" 
      [4] "single line in standard line width and therefore needs a sensible linebreak"
      [5] "somewhere along the way."                                                   
      [6] "an {{x}} x in internal_func(): oh no internal {{value}} error!"             

---

    Code
      format_cnds(out$conditions, prefix = format_inline("{.field x} = "))
    Output
      [1] "x = ! in wrap_f(): we're here!"                                                
      [2] "x = ! no context"                                                              
      [3] "x = ! in my_f(): long trouble! This sentences is easily longer than a single"  
      [4] "line in standard line width and therefore needs a sensible linebreak somewhere"
      [5] "along the way."                                                                
      [6] "x = x in internal_func(): oh no internal {{value}} error!"                     

# format_cnds() [fancy]

    Code
      format_cnds(out$conditions)
    Output
      [1] "\033[33m!\033[39m in \033[1mwrap_f()\033[22m: we're here!"                                                   
      [2] "\033[33m!\033[39m no context"                                                                                
      [3] "\033[33m!\033[39m in \033[1mmy_f()\033[22m: long trouble! This sentences is easily longer than a single line"
      [4] "in standard line width and therefore needs a sensible linebreak somewhere along"                             
      [5] "the way."                                                                                                    
      [6] "\033[31m✖\033[39m in \033[1minternal_func()\033[22m: oh no internal {{value}} error!"                        

---

    Code
      format_cnds(out$conditions, include_symbol = FALSE)
    Output
      [1] "in \033[1mwrap_f()\033[22m: we're here!"                                                      
      [2] "no context"                                                                                   
      [3] "in \033[1mmy_f()\033[22m: long trouble! This sentences is easily longer than a single line in"
      [4] "standard line width and therefore needs a sensible linebreak somewhere along"                 
      [5] "the way."                                                                                     
      [6] "in \033[1minternal_func()\033[22m: oh no internal {{value}} error!"                           

---

    Code
      format_cnds(out$conditions, include_call = FALSE)
    Output
      [1] "\033[33m!\033[39m we're here!"                                                                 
      [2] "\033[33m!\033[39m no context"                                                                  
      [3] "\033[33m!\033[39m long trouble! This sentences is easily longer than a single line in standard"
      [4] "line width and therefore needs a sensible linebreak somewhere along the way."                  
      [5] "\033[31m✖\033[39m oh no internal {{value}} error!"                                             

---

    Code
      format_cnds(out$conditions, call_format = "hey {.fn {call}}! ")
    Output
      [1] "\033[33m!\033[39m hey `wrap_f()`! we're here!"                                              
      [2] "\033[33m!\033[39m no context"                                                               
      [3] "\033[33m!\033[39m hey `my_f()`! long trouble! This sentences is easily longer than a single"
      [4] "line in standard line width and therefore needs a sensible linebreak somewhere"             
      [5] "along the way."                                                                             
      [6] "\033[31m✖\033[39m hey `internal_func()`! oh no internal {{value}} error!"                   

---

    Code
      format_cnds(out$conditions, indent = 1)
    Output
                                                                                                                     
                                                         "\033[33m!\033[39m in \033[1mwrap_f()\033[22m: we're here!" 
                                                                                                                     
                                                                                      "\033[33m!\033[39m no context" 
                                                                                                                     
      "\033[33m!\033[39m in \033[1mmy_f()\033[22m: long trouble! This sentences is easily longer than a single line" 
                                                                                                                     
                                         "in standard line width and therefore needs a sensible linebreak somewhere" 
                                                                                                                     
                                                                                                    "along the way." 
                                                                                                                     
                              "\033[31m✖\033[39m in \033[1minternal_func()\033[22m: oh no internal {{value}} error!" 

---

    Code
      format_cnds(out$conditions, indent = 3)
    Output
                                                                                                                    
                                                    "    \033[33m!\033[39m in \033[1mwrap_f()\033[22m: we're here!" 
                                                                                                                    
                                                                                 "    \033[33m!\033[39m no context" 
                                                                                                                    
      "    \033[33m!\033[39m in \033[1mmy_f()\033[22m: long trouble! This sentences is easily longer than a single" 
                                                                                                                    
                                         "    line in standard line width and therefore needs a sensible linebreak" 
                                                                                                                    
                                                                                     "    somewhere along the way." 
                                                                                                                    
                         "    \033[31m✖\033[39m in \033[1minternal_func()\033[22m: oh no internal {{value}} error!" 

---

    Code
      format_cnds(out$conditions, prefix = "an {x} ")
    Output
      [1] "an {{x}} \033[33m!\033[39m in \033[1mwrap_f()\033[22m: we're here!"                                       
      [2] "an {{x}} \033[33m!\033[39m no context"                                                                    
      [3] "an {{x}} \033[33m!\033[39m in \033[1mmy_f()\033[22m: long trouble! This sentences is easily longer than a"
      [4] "single line in standard line width and therefore needs a sensible linebreak"                              
      [5] "somewhere along the way."                                                                                 
      [6] "an {{x}} \033[31m✖\033[39m in \033[1minternal_func()\033[22m: oh no internal {{value}} error!"            

---

    Code
      format_cnds(out$conditions, prefix = format_inline("{.field x} = "))
    Output
      [1] "\033[32mx\033[39m = \033[33m!\033[39m in \033[1mwrap_f()\033[22m: we're here!"                                              
      [2] "\033[32mx\033[39m = \033[33m!\033[39m no context"                                                                           
      [3] "\033[32mx\033[39m = \033[33m!\033[39m in \033[1mmy_f()\033[22m: long trouble! This sentences is easily longer than a single"
      [4] "line in standard line width and therefore needs a sensible linebreak somewhere"                                             
      [5] "along the way."                                                                                                             
      [6] "\033[32mx\033[39m = \033[31m✖\033[39m in \033[1minternal_func()\033[22m: oh no internal {{value}} error!"                   

# show_cnds() [plain]

    Code
      show_cnds(out$conditions, call = NULL)
    Message
      x 3 warnings and 1 error
        > ! in wrap_f(): we're here!
        > ! no context
        > ! in my_f(): long trouble! This sentences is easily longer than a single
        line in standard line width and therefore needs a sensible linebreak
        somewhere along the way.
        > x in internal_func(): oh no internal {value} error!

---

    Code
      test_show_cnds()
    Message
      x in test_show_cnds(): 3 warnings and 1 error
        > ! in wrap_f(): we're here!
        > ! no context
        > ! in my_f(): long trouble! This sentences is easily longer than a single
        line in standard line width and therefore needs a sensible linebreak
        somewhere along the way.
        > x in internal_func(): oh no internal {value} error!

# show_cnds() [fancy]

    Code
      show_cnds(out$conditions, call = NULL)
    Message
      [31m✖[39m [33m3 warnings[39m and [31m1 error[39m
        → [33m![39m in [1mwrap_f()[22m: we're here!
        → [33m![39m no context
        → [33m![39m in [1mmy_f()[22m: long trouble! This sentences is easily longer than a single
        line in standard line width and therefore needs a sensible linebreak
        somewhere along the way.
        → [31m✖[39m in [1minternal_func()[22m: oh no internal {value} error!

---

    Code
      test_show_cnds()
    Message
      [31m✖[39m in [1mtest_show_cnds()[22m: [33m3 warnings[39m and [31m1 error[39m
        → [33m![39m in [1mwrap_f()[22m: we're here!
        → [33m![39m no context
        → [33m![39m in [1mmy_f()[22m: long trouble! This sentences is easily longer than a single
        line in standard line width and therefore needs a sensible linebreak
        somewhere along the way.
        → [31m✖[39m in [1minternal_func()[22m: oh no internal {value} error!

# abort_cnds() [plain]

    Code
      abort_cnds(out$conditions, call = NULL)
    Condition
      Error:
      ! 3 warnings and 1 error
        > ! in wrap_f(): we're here!
        > ! no context
        > ! in my_f(): long trouble! This sentences is easily longer than a single
        line in standard line width and therefore needs a sensible linebreak
        somewhere along the way.
        > x in internal_func(): oh no internal {value} error!

---

    Code
      test_abort_cnds()
    Condition
      Error in `test_abort_cnds()`:
      ! in test_abort_cnds(): 3 warnings and 1 error
        > ! in wrap_f(): we're here!
        > ! no context
        > ! in my_f(): long trouble! This sentences is easily longer than a single
        line in standard line width and therefore needs a sensible linebreak
        somewhere along the way.
        > x in internal_func(): oh no internal {value} error!

# abort_cnds() [fancy]

    Code
      abort_cnds(out$conditions, call = NULL)
    Condition
      [1m[33mError[39m:[22m
      [1m[22m[33m![39m [33m3 warnings[39m and [31m1 error[39m
        → [33m![39m in [1mwrap_f()[22m: we're here!
        → [33m![39m no context
        → [33m![39m in [1mmy_f()[22m: long trouble! This sentences is easily longer than a single
        line in standard line width and therefore needs a sensible linebreak
        somewhere along the way.
        → [31m✖[39m in [1minternal_func()[22m: oh no internal {value} error!

---

    Code
      test_abort_cnds()
    Condition
      [1m[33mError[39m in `test_abort_cnds()`:[22m
      [1m[22m[33m![39m in [1mtest_abort_cnds()[22m: [33m3 warnings[39m and [31m1 error[39m
        → [33m![39m in [1mwrap_f()[22m: we're here!
        → [33m![39m no context
        → [33m![39m in [1mmy_f()[22m: long trouble! This sentences is easily longer than a single
        line in standard line width and therefore needs a sensible linebreak
        somewhere along the way.
        → [31m✖[39m in [1minternal_func()[22m: oh no internal {value} error!

