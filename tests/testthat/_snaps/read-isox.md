# orbi_read_isox() [plain]

    Code
      orbi_read_isox(file.path(base_dir, "test_files", "missing_column.isox"))
    Message
      !  loaded 6 peaks for 1 compound (NO3-) with 6 isotopocules (15N, 17O, 18O,
      15N18O, 17O18O, and 18O18O) from 'missing_column.isox' but encountered 1
      warning > The following named parsers don't match the column names: tic
    Condition
      Error in `orbi_read_isox()`:
      ! missing required column: tic

---

    Code
      df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox",
        package = "isoorbi"))
    Message
      v loaded 5184 peaks for 1 compound (NO3-) with 6 isotopocules (15N, 17O, 18O,
      15N18O, 17O18O, and 18O18O) from 'testfile_dual_inlet.isox'
      v orbi_read_isox() read '.isox' data from 1 file

---

    Code
      df2 <- orbi_read_isox(c(system.file("extdata", "testfile_dual_inlet.isox",
        package = "isoorbi"), system.file("extdata", "testfile_flow.isox", package = "isoorbi")))
    Message
      v loaded 5184 peaks for 1 compound (NO3-) with 6 isotopocules (15N, 17O, 18O,
      15N18O, 17O18O, and 18O18O) from 'testfile_dual_inlet.isox'
      v loaded 6449 peaks for 1 compound (HSO4-) with 5 isotopocules (M0, 33S, 17O,
      34S, and 18O) from 'testfile_flow.isox'
      v orbi_read_isox() read '.isox' data from 2 files
    Code
      expect_equal(nrow(df2), 11633)

# orbi_read_isox() [fancy]

    Code
      orbi_read_isox(file.path(base_dir, "test_files", "missing_column.isox"))
    Message
      [33m![39m loaded 6 peaks for 1 compound ([32mNO3-[39m) with 6 isotopocules ([32m15N[39m, [32m17O[39m, [32m18O[39m,
      [32m15N18O[39m, [32m17O18O[39m, and [32m18O18O[39m) from [34mmissing_column.isox[39m but encountered [33m1[39m
      [33mwarning[39m → The following named parsers don't match the column names: tic
    Condition
      [1m[33mError[39m in `orbi_read_isox()`:[22m
      [1m[22m[33m![39m missing required column: [32mtic[39m

---

    Code
      df <- orbi_read_isox(system.file("extdata", "testfile_dual_inlet.isox",
        package = "isoorbi"))
    Message
      [32m✔[39m loaded 5184 peaks for 1 compound ([32mNO3-[39m) with 6 isotopocules ([32m15N[39m, [32m17O[39m, [32m18O[39m,
      [32m15N18O[39m, [32m17O18O[39m, and [32m18O18O[39m) from [34mtestfile_dual_inlet.isox[39m
      [32m✔[39m [1morbi_read_isox()[22m read [34m.isox[39m data from 1 file

---

    Code
      df2 <- orbi_read_isox(c(system.file("extdata", "testfile_dual_inlet.isox",
        package = "isoorbi"), system.file("extdata", "testfile_flow.isox", package = "isoorbi")))
    Message
      [32m✔[39m loaded 5184 peaks for 1 compound ([32mNO3-[39m) with 6 isotopocules ([32m15N[39m, [32m17O[39m, [32m18O[39m,
      [32m15N18O[39m, [32m17O18O[39m, and [32m18O18O[39m) from [34mtestfile_dual_inlet.isox[39m
      [32m✔[39m loaded 6449 peaks for 1 compound ([32mHSO4-[39m) with 5 isotopocules ([32mM0[39m, [32m33S[39m, [32m17O[39m,
      [32m34S[39m, and [32m18O[39m) from [34mtestfile_flow.isox[39m
      [32m✔[39m [1morbi_read_isox()[22m read [34m.isox[39m data from 2 files
    Code
      expect_equal(nrow(df2), 11633)

