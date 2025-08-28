# orbi_flag_weak_isotopocules() [plain]

    Code
      out <- orbi_flag_weak_isotopocules(mutate(df, block = as.factor("block1"),
      segment = as.factor("segment2"), injection = as.factor("injection3")),
      min_percent = 1)
    Message
      v orbi_flag_weak_isotopocules() confirmed there are no weak isotopocules: all
      are detected in at least 1% of scans in each of the 6 data groups (based on
      filename, compound, block, segment, injection, and isotopocule)

---

    Code
      out <- orbi_flag_weak_isotopocules(filter(df, !(isotopocule %in% c("15N", "17O") &
        scan.no > 10)), min_percent = 50)
    Message
      v orbi_flag_weak_isotopocules() flagged 2 of 6 isotopocules as weak because
      they were NOT present in at least 50% of scans in each of the 6 data groups
      (based on filename, compound, and isotopocule) > use
      orbi_plot_isotopocule_coverage() to visualize them

# orbi_flag_weak_isotopocules() [fancy]

    Code
      out <- orbi_flag_weak_isotopocules(mutate(df, block = as.factor("block1"),
      segment = as.factor("segment2"), injection = as.factor("injection3")),
      min_percent = 1)
    Message
      [32m✔[39m [1morbi_flag_weak_isotopocules()[22m confirmed there are no weak isotopocules: all
      are detected in at least 1% of scans in each of the 6 data groups (based on
      [32mfilename[39m, [32mcompound[39m, [32mblock[39m, [32msegment[39m, [32minjection[39m, and [32misotopocule[39m)

---

    Code
      out <- orbi_flag_weak_isotopocules(filter(df, !(isotopocule %in% c("15N", "17O") &
        scan.no > 10)), min_percent = 50)
    Message
      [32m✔[39m [1morbi_flag_weak_isotopocules()[22m flagged 2 of 6 isotopocules as weak because
      they were NOT present in at least 50% of scans in each of the 6 data groups
      (based on [32mfilename[39m, [32mcompound[39m, and [32misotopocule[39m) → use
      [1morbi_plot_isotopocule_coverage()[22m to visualize them

# orbi_flag_outliers() [plain]

    Code
      out <- orbi_flag_outliers(df, agc_window = c(10, 90))
    Message
      v orbi_flag_outliers() flagged 172/864 scans (19.9%) as outliers based on AGC
      window (10% to 90%) cutoff, i.e. based on scans whose number of ions tic *
      it.ms in the Orbitrap analyzer fall into the lowest (<10%) or highest (>90%)
      quantiles > use orbi_plot_raw_data(y = tic * it.ms) to visualize them

---

    Code
      out <- orbi_flag_outliers(df, agc_fold_cutoff = 1.01)
    Message
      v orbi_flag_outliers() flagged 146/864 scans (16.9%) as outliers based on 1.01
      fold AGC cutoff, i.e. based on scans below 1/1.01 and above 1.01 times the
      average number of ions tic * it.ms in the Orbitrap analyzer > use
      orbi_plot_raw_data(y = tic * it.ms) to visualize them

---

    Code
      out <- orbi_flag_outliers(filter(df, row_number() < 5), agc_window = c(10, 90))
    Message
      v orbi_flag_outliers() confirmed that none of the 1 scans are outliers based on
      AGC window (10% to 90%) cutoff, i.e. based on scans whose number of ions tic *
      it.ms in the Orbitrap analyzer fall into the lowest (<10%) or highest (>90%)
      quantiles

---

    Code
      out <- orbi_flag_outliers(df, agc_fold_cutoff = 2)
    Message
      v orbi_flag_outliers() confirmed that none of the 864 scans are outliers based
      on 2 fold AGC cutoff, i.e. based on scans below 1/2 and above 2 times the
      average number of ions tic * it.ms in the Orbitrap analyzer

# orbi_flag_outliers() [fancy]

    Code
      out <- orbi_flag_outliers(df, agc_window = c(10, 90))
    Message
      [32m✔[39m [1morbi_flag_outliers()[22m flagged 172/864 scans (19.9%) as outliers based on [32mAGC[39m
      [32mwindow (10% to 90%) cutoff[39m, i.e. based on [3mscans whose number of ions [32mtic[39m *[23m
      [3m[32mit.ms[39m in the Orbitrap analyzer fall into the lowest (<10%) or highest (>90%)[23m
      [3mquantiles[23m → use [1morbi_plot_raw_data(y = tic * it.ms)[22m to visualize them

---

    Code
      out <- orbi_flag_outliers(df, agc_fold_cutoff = 1.01)
    Message
      [32m✔[39m [1morbi_flag_outliers()[22m flagged 146/864 scans (16.9%) as outliers based on [32m1.01[39m
      [32mfold AGC cutoff[39m, i.e. based on [3mscans below 1/1.01 and above 1.01 times the[23m
      [3maverage number of ions [32mtic[39m * [32mit.ms[39m in the Orbitrap analyzer[23m → use
      [1morbi_plot_raw_data(y = tic * it.ms)[22m to visualize them

---

    Code
      out <- orbi_flag_outliers(filter(df, row_number() < 5), agc_window = c(10, 90))
    Message
      [32m✔[39m [1morbi_flag_outliers()[22m confirmed that none of the 1 scans are outliers based on
      [32mAGC window (10% to 90%) cutoff[39m, i.e. based on [3mscans whose number of ions [32mtic[39m *[23m
      [3m[32mit.ms[39m in the Orbitrap analyzer fall into the lowest (<10%) or highest (>90%)[23m
      [3mquantiles[23m

---

    Code
      out <- orbi_flag_outliers(df, agc_fold_cutoff = 2)
    Message
      [32m✔[39m [1morbi_flag_outliers()[22m confirmed that none of the 864 scans are outliers based
      on [32m2 fold AGC cutoff[39m, i.e. based on [3mscans below 1/2 and above 2 times the[23m
      [3maverage number of ions [32mtic[39m * [32mit.ms[39m in the Orbitrap analyzer[23m

# orbi_define_basepeak() [plain]

    Code
      out <- orbi_define_basepeak(dataset = df, basepeak_def = "M0")
    Message
      v orbi_define_basepeak() set M0 as the ratio denominator and calculated 8
      ratios for 4 isotopocules/base peak (17O, 18O, 33S, and 34S)

# orbi_define_basepeak() [fancy]

    Code
      out <- orbi_define_basepeak(dataset = df, basepeak_def = "M0")
    Message
      [32m✔[39m [1morbi_define_basepeak()[22m set [32mM0[39m as the ratio denominator and calculated 8
      ratios for 4 isotopocules/base peak ([32m17O[39m, [32m18O[39m, [32m33S[39m, and [32m34S[39m)

---

    structure(list(filename = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L), levels = "s3744", class = "factor"), scan.no = c(1L, 
    1L, 1L, 1L, 2L, 2L, 2L, 2L), time.min = c(0.002, 0.002, 0.002, 
    0.002, 0.004, 0.004, 0.004, 0.004), compound = structure(c(1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L), levels = "HSO4-", class = "factor"), 
        isotopocule = structure(c(3L, 1L, 4L, 2L, 3L, 1L, 4L, 2L), levels = c("17O", 
        "18O", "33S", "34S"), class = "factor"), ions.incremental = c(325.81442443, 
        61.8857132, 2215.16076727, 435.53885752, 339.43909506, 66.64145661, 
        2315.07031223, 438.49763227), ratio = c(0.00861869636381228, 
        0.00163704898044919, 0.0585971541423617, 0.0115212123409627, 
        0.00801589660021069, 0.00157374631634215, 0.0546706744011703, 
        0.0103551763213728), tic = c(41034496L, 41034496L, 41034496L, 
        41034496L, 46151496L, 46151496L, 46151496L, 46151496L), it.ms = c(4.969, 
        4.969, 4.969, 4.969, 5.16, 5.16, 5.16, 5.16), intensity = c(314948.125, 
        59823.066406, 2146602.75, 422056.40625, 331462.75, 65077.691406, 
        2270958.5, 430145), resolution = c(35000L, 35000L, 35000L, 
        35000L, 35000L, 35000L, 35000L, 35000L), peakResolution = c(55802L, 
        51800L, 57202L, 56102L, 56102L, 51900L, 57302L, 56202L), 
        peakNoise = c(7593.842, 7594.009, 7612.71, 7612.66, 7671.243, 
        7671.504, 7706.157, 7706.204), mzMeasured = c(97.95937, 97.96421, 
        98.95579, 98.96426, 97.95939, 97.96426, 98.95579, 98.96427
        ), basePeakIntensity = c(36376320L, 36376320L, 36376320L, 
        36376320L, 41061572L, 41061572L, 41061572L, 41061572L), rawOvFtT = c(288195.8, 
        288195.8, 288195.8, 288195.8, 329721.5, 329721.5, 329721.5, 
        329721.5), intensCompFactor = c(0.7153, 0.7153, 0.7153, 0.7153, 
        0.7319, 0.7319, 0.7319, 0.7319), agc = c(0L, 0L, 0L, 0L, 
        0L, 0L, 0L, 0L), agcTarget = c(2e+05, 2e+05, 2e+05, 2e+05, 
        2e+05, 2e+05, 2e+05, 2e+05), microscans = c(1L, 1L, 1L, 1L, 
        1L, 1L, 1L, 1L), numberLockmassesFound = c(1L, 1L, 1L, 1L, 
        1L, 1L, 1L, 1L), analyzerTemperature = c(27.39, 27.39, 27.39, 
        27.39, 27.39, 27.39, 27.39, 27.39), basepeak = c("M0", "M0", 
        "M0", "M0", "M0", "M0", "M0", "M0"), basepeak_ions = c(37803.21416102, 
        37803.21416102, 37803.21416102, 37803.21416102, 42345.74271468, 
        42345.74271468, 42345.74271468, 42345.74271468)), row.names = c(NA, 
    -8L), class = c("tbl_df", "tbl", "data.frame"))

