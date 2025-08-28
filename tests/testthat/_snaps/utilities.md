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

