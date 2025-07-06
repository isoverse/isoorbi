# options works

    Code
      orbi_get_options()
    Output
      $di_ref_name
      [1] "ref"
      
      $di_sample_name
      [1] "sam"
      
      $data_type_data
      [1] "data"
      
      $data_type_startup
      [1] "startup"
      
      $data_type_changeover
      [1] "changeover"
      
      $data_type_unused
      [1] "unused"
      
      $include_spectra
      [1] FALSE
      
      $raw_aggregator
      # A tibble: 22 x 8
         dataset   column              source     default regexp cast     func  args  
         <chr>     <chr>               <list>     <lgl>   <lgl>  <chr>    <chr> <list>
       1 file_info "uid"               <list [1]> NA      FALSE  as.fact~ <NA>  <NULL>
       2 file_info "\\1"               <list [1]> NA      TRUE   as.char~ <NA>  <NULL>
       3 scans     "scan"              <list [1]> NA      FALSE  as.inte~ <NA>  <NULL>
       4 scans     "time.min"          <list [1]> NA      FALSE  as.nume~ <NA>  <NULL>
       5 scans     "tic"               <list [1]> NA      FALSE  as.nume~ <NA>  <NULL>
       6 scans     "it.ms"             <list [1]> NA      FALSE  as.nume~ <NA>  <NULL>
       7 scans     "resolution"        <list [1]> NA      FALSE  as.nume~ <NA>  <NULL>
       8 scans     "basePeakIntensity" <list [1]> NA      FALSE  as.nume~ sapp~ <list>
       9 scans     "rawOvFtT"          <list [1]> NA      FALSE  as.nume~ <NA>  <NULL>
      10 scans     "intensCompFactor"  <list [1]> NA      FALSE  as.nume~ <NA>  <NULL>
      # i 12 more rows
      
      $debug
      [1] FALSE
      

