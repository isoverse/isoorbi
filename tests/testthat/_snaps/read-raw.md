# orbi_read_raw() works

    Code
      withr::with_options({
        x <- orbi_read_raw(orbi_find_raw(system.file("extdata", package = "isoorbi")),
        cache = FALSE)
        select(x, -"file_path")
      }, new = list(show_exec_times = FALSE))
    Message
      v orbi_read_raw() read 'nitrate_test_10scans.raw' from cache
      v orbi_read_raw() read 'nitrate_test_1scan.raw' from cache
      v orbi_read_raw() finished reading 2 files
    Output
      # A tibble: 2 x 5
        file_info         scans              peaks              spectra  problems
        <list>            <list>             <list>             <list>   <list>  
      1 <tibble [1 x 37]> <tibble [10 x 89]> <tibble [126 x 5]> <tibble> <tibble>
      2 <tibble [1 x 37]> <tibble [1 x 89]>  <tibble [12 x 5]>  <tibble> <tibble>

---

    Code
      withr::with_options({
        y <- orbi_aggregate_raw(x, show_progress = FALSE)
      }, new = list(show_exec_times = FALSE))
    Message
      v aggregate_files() aggregated file_info (2), scans (11), peaks (138), and
      spectra (0) from 2 files
        > file_info: uid, file_path, RAW file, RAW file version, Creation date,
        Operator, Number of instruments, Description, Instrument model, Instrument
        name, Instrument method, Serial number, Software version, Firmware version,
        Units, Mass resolution, Number of scans, Number of ms2 scans, ..., User text
        3, and User text 4
        > scans: uid, scan, time.min, tic, it.ms, resolution, basePeakIntensity,
        rawOvFtT, intensCompFactor, agc, agcTarget, microscans,
        numberLockmassesFound, and analyzerTemperature
        > peaks: uid, scan, mzMeasured, intensity, and peakNoise
        > spectra: uid, scan, mz, and intensity
        > problems: uid, type, call, message, and condition

---

    Code
      y
    Output
      $file_info
      # A tibble: 2 x 37
        uid              `RAW file` `RAW file version` Operator Number of instrument~1
        <fct>            <chr>      <chr>              <chr>    <chr>                 
      1 nitrate_test_10~ nitrate_t~ 66                 SYSTEM   1                     
      2 nitrate_test_1s~ nitrate_t~ 66                 SYSTEM   1                     
      # i abbreviated name: 1: `Number of instruments`
      # i 32 more variables: Description <chr>, `Instrument model` <chr>,
      #   `Instrument name` <chr>, `Instrument method` <chr>, `Serial number` <chr>,
      #   `Software version` <chr>, `Firmware version` <chr>, Units <chr>,
      #   `Mass resolution` <chr>, `Number of scans` <chr>,
      #   `Number of ms2 scans` <chr>, `Scan range` <chr>, `Time range` <chr>,
      #   `Mass range` <chr>, `Scan filter (first scan)` <chr>, ...
      
      $scans
      # A tibble: 11 x 14
         uid          scan time.min    tic it.ms resolution basePeakIntensity rawOvFtT
         <fct>       <int>    <dbl>  <dbl> <dbl>      <dbl>             <dbl>    <dbl>
       1 nitrate_te~     1  0.00454 4.34e6  68.3      60000           4046979  391227.
       2 nitrate_te~     2  0.00675 3.39e6  80.6      60000           3213242  366513.
       3 nitrate_te~     3  0.00897 3.67e6  79.5      60000           3406449  385482.
       4 nitrate_te~     4  0.0112  5.97e6 100.       60000           5644106  680678.
       5 nitrate_te~     5  0.0134  2.60e6  94.3      60000           2455685  334772.
       6 nitrate_te~     6  0.0156  4.27e6  55.3      60000           4068725  325443.
       7 nitrate_te~     7  0.0181  3.13e6 131.       60000           2968120  508191.
       8 nitrate_te~     8  0.0203  3.52e6  78.7      60000           3345269  371384.
       9 nitrate_te~     9  0.0225  4.32e6 109.       60000           4084736  566818.
      10 nitrate_te~    10  0.0247  3.55e6  95.7      60000           3329268  439627.
      11 nitrate_te~     1  0.00399 6.38e6  56.6      60000           6029180  460998.
      # i 6 more variables: intensCompFactor <dbl>, agc <chr>, agcTarget <int>,
      #   microscans <int>, numberLockmassesFound <int>, analyzerTemperature <dbl>
      
      $peaks
      # A tibble: 138 x 5
         uid                       scan mzMeasured intensity peakNoise
         <fct>                    <int>      <dbl>     <dbl>     <dbl>
       1 nitrate_test_10scans.raw     1       62.0     1211.      513.
       2 nitrate_test_10scans.raw     1       62.0     1463.      513.
       3 nitrate_test_10scans.raw     1       62.0     1172.      513.
       4 nitrate_test_10scans.raw     1       62.0     1116.      513.
       5 nitrate_test_10scans.raw     1       62.0  4046979       513.
       6 nitrate_test_10scans.raw     1       62.0     1798.      513.
       7 nitrate_test_10scans.raw     1       62.0     1444.      513.
       8 nitrate_test_10scans.raw     1       62.0     1346.      513.
       9 nitrate_test_10scans.raw     1       62.0     1469.      513.
      10 nitrate_test_10scans.raw     1       62.1     1043.      513.
      # i 128 more rows
      
      $spectra
      # A tibble: 0 x 4
      # i 4 variables: uid <fct>, scan <int>, mz <dbl>, intensity <dbl>
      
      $problems
      # A tibble: 0 x 5
      # i 5 variables: uid <fct>, type <chr>, call <chr>, message <chr>,
      #   condition <list>
      

---

    Code
      withr::with_options({
        x <- orbi_read_raw(orbi_find_raw(system.file("extdata", package = "isoorbi")),
        cache = FALSE, include_spectra = 1)
        select(x, -"file_path")
      }, new = list(show_exec_times = FALSE))
    Message
      v orbi_read_raw() read 'nitrate_test_10scans.raw' from cache, included the
      spectrum from 1 scan
      v orbi_read_raw() read 'nitrate_test_1scan.raw' from cache, included the
      spectrum from 1 scan
      v orbi_read_raw() finished reading 2 files
    Output
      # A tibble: 2 x 5
        file_info         scans              peaks              spectra  problems
        <list>            <list>             <list>             <list>   <list>  
      1 <tibble [1 x 37]> <tibble [10 x 89]> <tibble [126 x 5]> <tibble> <tibble>
      2 <tibble [1 x 37]> <tibble [1 x 89]>  <tibble [12 x 5]>  <tibble> <tibble>

---

    Code
      withr::with_options({
        y <- orbi_aggregate_raw(x, show_progress = FALSE)
      }, new = list(show_exec_times = FALSE))
    Message
      v aggregate_files() aggregated file_info (2), scans (11), peaks (138), and
      spectra (675) from 2 files
        > file_info: uid, file_path, RAW file, RAW file version, Creation date,
        Operator, Number of instruments, Description, Instrument model, Instrument
        name, Instrument method, Serial number, Software version, Firmware version,
        Units, Mass resolution, Number of scans, Number of ms2 scans, ..., User text
        3, and User text 4
        > scans: uid, scan, time.min, tic, it.ms, resolution, basePeakIntensity,
        rawOvFtT, intensCompFactor, agc, agcTarget, microscans,
        numberLockmassesFound, and analyzerTemperature
        > peaks: uid, scan, mzMeasured, intensity, and peakNoise
        > spectra: uid, scan, mz, and intensity
        > problems: uid, type, call, message, and condition

---

    Code
      y
    Output
      $file_info
      # A tibble: 2 x 37
        uid              `RAW file` `RAW file version` Operator Number of instrument~1
        <fct>            <chr>      <chr>              <chr>    <chr>                 
      1 nitrate_test_10~ nitrate_t~ 66                 SYSTEM   1                     
      2 nitrate_test_1s~ nitrate_t~ 66                 SYSTEM   1                     
      # i abbreviated name: 1: `Number of instruments`
      # i 32 more variables: Description <chr>, `Instrument model` <chr>,
      #   `Instrument name` <chr>, `Instrument method` <chr>, `Serial number` <chr>,
      #   `Software version` <chr>, `Firmware version` <chr>, Units <chr>,
      #   `Mass resolution` <chr>, `Number of scans` <chr>,
      #   `Number of ms2 scans` <chr>, `Scan range` <chr>, `Time range` <chr>,
      #   `Mass range` <chr>, `Scan filter (first scan)` <chr>, ...
      
      $scans
      # A tibble: 11 x 14
         uid          scan time.min    tic it.ms resolution basePeakIntensity rawOvFtT
         <fct>       <int>    <dbl>  <dbl> <dbl>      <dbl>             <dbl>    <dbl>
       1 nitrate_te~     1  0.00454 4.34e6  68.3      60000           4046979  391227.
       2 nitrate_te~     2  0.00675 3.39e6  80.6      60000           3213242  366513.
       3 nitrate_te~     3  0.00897 3.67e6  79.5      60000           3406449  385482.
       4 nitrate_te~     4  0.0112  5.97e6 100.       60000           5644106  680678.
       5 nitrate_te~     5  0.0134  2.60e6  94.3      60000           2455685  334772.
       6 nitrate_te~     6  0.0156  4.27e6  55.3      60000           4068725  325443.
       7 nitrate_te~     7  0.0181  3.13e6 131.       60000           2968120  508191.
       8 nitrate_te~     8  0.0203  3.52e6  78.7      60000           3345269  371384.
       9 nitrate_te~     9  0.0225  4.32e6 109.       60000           4084736  566818.
      10 nitrate_te~    10  0.0247  3.55e6  95.7      60000           3329268  439627.
      11 nitrate_te~     1  0.00399 6.38e6  56.6      60000           6029180  460998.
      # i 6 more variables: intensCompFactor <dbl>, agc <chr>, agcTarget <int>,
      #   microscans <int>, numberLockmassesFound <int>, analyzerTemperature <dbl>
      
      $peaks
      # A tibble: 138 x 5
         uid                       scan mzMeasured intensity peakNoise
         <fct>                    <int>      <dbl>     <dbl>     <dbl>
       1 nitrate_test_10scans.raw     1       62.0     1211.      513.
       2 nitrate_test_10scans.raw     1       62.0     1463.      513.
       3 nitrate_test_10scans.raw     1       62.0     1172.      513.
       4 nitrate_test_10scans.raw     1       62.0     1116.      513.
       5 nitrate_test_10scans.raw     1       62.0  4046979       513.
       6 nitrate_test_10scans.raw     1       62.0     1798.      513.
       7 nitrate_test_10scans.raw     1       62.0     1444.      513.
       8 nitrate_test_10scans.raw     1       62.0     1346.      513.
       9 nitrate_test_10scans.raw     1       62.0     1469.      513.
      10 nitrate_test_10scans.raw     1       62.1     1043.      513.
      # i 128 more rows
      
      $spectra
      # A tibble: 675 x 4
         uid                       scan    mz intensity
         <fct>                    <int> <dbl>     <dbl>
       1 nitrate_test_10scans.raw     1  60.9        0 
       2 nitrate_test_10scans.raw     1  60.9        0 
       3 nitrate_test_10scans.raw     1  60.9        0 
       4 nitrate_test_10scans.raw     1  60.9        0 
       5 nitrate_test_10scans.raw     1  62.0        0 
       6 nitrate_test_10scans.raw     1  62.0        0 
       7 nitrate_test_10scans.raw     1  62.0        0 
       8 nitrate_test_10scans.raw     1  62.0        0 
       9 nitrate_test_10scans.raw     1  62.0      496.
      10 nitrate_test_10scans.raw     1  62.0      935.
      # i 665 more rows
      
      $problems
      # A tibble: 0 x 5
      # i 5 variables: uid <fct>, type <chr>, call <chr>, message <chr>,
      #   condition <list>
      

