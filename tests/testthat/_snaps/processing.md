# orbi_flag_satellite_peaks() [plain]

    Code
      agg_out <- orbi_flag_satellite_peaks(agg_data)
    Message
      v orbi_flag_satellite_peaks() flagged 1/14 peaks in 1 isotopocule (15N) as
      satellite peaks (7.1%)

---

    Code
      agg_out
    Message
      -------- aggregated data from 1 raw file - retrieve with orbi_get_data() -------
      > file_info (1): uidx, filename
      > scans (3): uidx, scan.no, tic, it.ms
      > peaks (14): uidx, scan.no, tic (2 NA), it.ms (2 NA), mzMeasured (2 NA),
      intensity (2 NA), compound (1 NA), itc_uidx (1 NA), isotopocule (1 NA), mzExact
      (1 NA), charge (1 NA), is_satellite_peak

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["uidx", "scan.no", "tic", "it.ms", "mzMeasured", "intensity", "compound", "itc_uidx", "isotopocule", "mzExact", "charge", "is_satellite_peak"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, "NA", 2, 2, 2, 2, 3, 3, 3, "NA", 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, "NA", 2, 2, 2, 2, 3, 3, 3, "NA", 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [61.9873, 62.9845, 62.9917, 63.9917, "NA", 62.9852, 62.9855, 62.9924, 63.9924, 61.9878, 62.985, 62.9922, "NA", 63.9934]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, "NA", 5, 8, 6, 7, 9, 10, 11, "NA", 12]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["nitrate"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NA"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 1, 2, 2, 3, 4, 1, 2, 3, 4, "NA"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["M0", "15N", "17O", "18O"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 2, 3, 4, 1, 2, 2, 3, 4, 1, 2, 3, 4, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [61.9878, 62.985, 62.9922, 63.9922, 61.9878, 62.985, 62.985, 62.9922, 63.9922, 61.9878, 62.985, 62.9922, 63.9922, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NA"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false, false, false, false, false, true, false, false, false, false, false, false, false, false]
        }
      ]
    }

---

    Code
      out <- orbi_flag_satellite_peaks(test_file)
    Message
      v orbi_flag_satellite_peaks() confirmed there are no satellite peaks

# orbi_flag_satellite_peaks() [fancy]

    Code
      agg_out <- orbi_flag_satellite_peaks(agg_data)
    Message
      [32m✔[39m [1morbi_flag_satellite_peaks()[22m flagged 1/14 peaks in 1 isotopocule ([32m15N[39m) as
      [33msatellite[39m peaks (7.1%)

---

    Code
      agg_out
    Message
      ──────── [1maggregated data from 1 raw file - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (1): [32muidx[39m, [32mfilename[39m
      → [34mscans[39m (3): [32muidx[39m, [32mscan.no[39m, [32mtic[39m, [32mit.ms[39m
      → [34mpeaks[39m (14): [32muidx[39m, [32mscan.no[39m, [32mtic[39m ([33m2 NA[39m), [32mit.ms[39m ([33m2 NA[39m), [32mmzMeasured[39m ([33m2 NA[39m),
      [32mintensity[39m ([33m2 NA[39m), [32mcompound[39m ([33m1 NA[39m), [32mitc_uidx[39m ([33m1 NA[39m), [32misotopocule[39m ([33m1 NA[39m), [32mmzExact[39m
      ([33m1 NA[39m), [32mcharge[39m ([33m1 NA[39m), [32mis_satellite_peak[39m

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["uidx", "scan.no", "tic", "it.ms", "mzMeasured", "intensity", "compound", "itc_uidx", "isotopocule", "mzExact", "charge", "is_satellite_peak"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, "NA", 2, 2, 2, 2, 3, 3, 3, "NA", 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, "NA", 2, 2, 2, 2, 3, 3, 3, "NA", 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [61.9873, 62.9845, 62.9917, 63.9917, "NA", 62.9852, 62.9855, 62.9924, 63.9924, 61.9878, 62.985, 62.9922, "NA", 63.9934]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, "NA", 5, 8, 6, 7, 9, 10, 11, "NA", 12]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["nitrate"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NA"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 1, 2, 2, 3, 4, 1, 2, 3, 4, "NA"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["M0", "15N", "17O", "18O"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 2, 3, 4, 1, 2, 2, 3, 4, 1, 2, 3, 4, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [61.9878, 62.985, 62.9922, 63.9922, 61.9878, 62.985, 62.985, 62.9922, 63.9922, 61.9878, 62.985, 62.9922, 63.9922, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NA"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false, false, false, false, false, true, false, false, false, false, false, false, false, false]
        }
      ]
    }

---

    Code
      out <- orbi_flag_satellite_peaks(test_file)
    Message
      [32m✔[39m [1morbi_flag_satellite_peaks()[22m confirmed there are no [33msatellite[39m peaks

# orbi_flag_weak_isotopocules() [plain]

    Code
      agg_out <- orbi_flag_weak_isotopocules(agg_data, 90)
    Message
      v orbi_flag_weak_isotopocules() flagged 2 of 5 isotopocules as weak because
      they were NOT present in at least 90% of scans in each of the 5 data groups
      (based on uidx, compound, and isotopocule) > use
      orbi_plot_isotopocule_coverage() to visualize them

---

    Code
      agg_out
    Message
      -------- aggregated data from 1 raw file - retrieve with orbi_get_data() -------
      > file_info (1): uidx, filename
      > scans (3): uidx, scan.no, tic, it.ms
      > peaks (14): uidx, scan.no, tic (2 NA), it.ms (2 NA), mzMeasured (2 NA),
      intensity (2 NA), compound (1 NA), itc_uidx (1 NA), isotopocule (1 NA), mzExact
      (1 NA), charge (1 NA), is_weak_isotopocule

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["uidx", "scan.no", "tic", "it.ms", "mzMeasured", "intensity", "compound", "itc_uidx", "isotopocule", "mzExact", "charge", "is_weak_isotopocule"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, "NA", 2, 2, 2, 2, 3, 3, 3, "NA", 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, "NA", 2, 2, 2, 2, 3, 3, 3, "NA", 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [61.9873, 62.9845, 62.9917, 63.9917, "NA", 62.9852, 62.9855, 62.9924, 63.9924, 61.9878, 62.985, 62.9922, "NA", 63.9934]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, "NA", 5, 8, 6, 7, 9, 10, 11, "NA", 12]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["nitrate"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NA"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 1, 2, 2, 3, 4, 1, 2, 3, 4, "NA"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["M0", "15N", "17O", "18O"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 2, 3, 4, 1, 2, 2, 3, 4, 1, 2, 3, 4, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [61.9878, 62.985, 62.9922, 63.9922, 61.9878, 62.985, 62.985, 62.9922, 63.9922, 61.9878, 62.985, 62.9922, 63.9922, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NA"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, false, false, true, true, false, false, false, true, true, false, false, true, false]
        }
      ]
    }

---

    Code
      out <- orbi_flag_weak_isotopocules(test_file, 90)
    Message
      v orbi_flag_weak_isotopocules() confirmed there are no weak isotopocules: all
      are detected in at least 90% of scans in each of the 6 data groups (based on
      filename, compound, and isotopocule)

---

    Code
      out <- orbi_flag_weak_isotopocules(test_file, 99.999)
    Message
      v orbi_flag_weak_isotopocules() confirmed there are no weak isotopocules: all
      are detected in at least 99.999% of scans in each of the 6 data groups (based
      on filename, compound, and isotopocule)

# orbi_flag_weak_isotopocules() [fancy]

    Code
      agg_out <- orbi_flag_weak_isotopocules(agg_data, 90)
    Message
      [32m✔[39m [1morbi_flag_weak_isotopocules()[22m flagged 2 of 5 isotopocules as [33mweak[39m because
      they were NOT present in at least 90% of scans in each of the 5 data groups
      (based on [32muidx[39m, [32mcompound[39m, and [32misotopocule[39m) → use
      [1morbi_plot_isotopocule_coverage()[22m to visualize them

---

    Code
      agg_out
    Message
      ──────── [1maggregated data from 1 raw file - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (1): [32muidx[39m, [32mfilename[39m
      → [34mscans[39m (3): [32muidx[39m, [32mscan.no[39m, [32mtic[39m, [32mit.ms[39m
      → [34mpeaks[39m (14): [32muidx[39m, [32mscan.no[39m, [32mtic[39m ([33m2 NA[39m), [32mit.ms[39m ([33m2 NA[39m), [32mmzMeasured[39m ([33m2 NA[39m),
      [32mintensity[39m ([33m2 NA[39m), [32mcompound[39m ([33m1 NA[39m), [32mitc_uidx[39m ([33m1 NA[39m), [32misotopocule[39m ([33m1 NA[39m), [32mmzExact[39m
      ([33m1 NA[39m), [32mcharge[39m ([33m1 NA[39m), [32mis_weak_isotopocule[39m

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["uidx", "scan.no", "tic", "it.ms", "mzMeasured", "intensity", "compound", "itc_uidx", "isotopocule", "mzExact", "charge", "is_weak_isotopocule"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, "NA", 2, 2, 2, 2, 3, 3, 3, "NA", 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, "NA", 2, 2, 2, 2, 3, 3, 3, "NA", 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [61.9873, 62.9845, 62.9917, 63.9917, "NA", 62.9852, 62.9855, 62.9924, 63.9924, 61.9878, 62.985, 62.9922, "NA", 63.9934]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, "NA", 5, 8, 6, 7, 9, 10, 11, "NA", 12]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["nitrate"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NA"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 1, 2, 2, 3, 4, 1, 2, 3, 4, "NA"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["M0", "15N", "17O", "18O"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 2, 3, 4, 1, 2, 2, 3, 4, 1, 2, 3, 4, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [61.9878, 62.985, 62.9922, 63.9922, 61.9878, 62.985, 62.985, 62.9922, 63.9922, 61.9878, 62.985, 62.9922, 63.9922, "NA"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, "NA"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true, false, false, true, true, false, false, false, true, true, false, false, true, false]
        }
      ]
    }

---

    Code
      out <- orbi_flag_weak_isotopocules(test_file, 90)
    Message
      [32m✔[39m [1morbi_flag_weak_isotopocules()[22m confirmed there are no [33mweak[39m isotopocules: all
      are detected in at least 90% of scans in each of the 6 data groups (based on
      [32mfilename[39m, [32mcompound[39m, and [32misotopocule[39m)

---

    Code
      out <- orbi_flag_weak_isotopocules(test_file, 99.999)
    Message
      [32m✔[39m [1morbi_flag_weak_isotopocules()[22m confirmed there are no [33mweak[39m isotopocules: all
      are detected in at least 99.999% of scans in each of the 6 data groups (based
      on [32mfilename[39m, [32mcompound[39m, and [32misotopocule[39m)

# orbi_define_basepeak() [plain]

    Code
      out <- orbi_define_basepeak(dataset = df, basepeak_def = "M0")
    Message
      v orbi_define_basepeak() set M0 as the ratio denominator and calculated 8 ratio
      values for 4 isotopocules (17O, 18O, 33S, and 34S)

# orbi_define_basepeak() [fancy]

    Code
      out <- orbi_define_basepeak(dataset = df, basepeak_def = "M0")
    Message
      [32m✔[39m [1morbi_define_basepeak()[22m set [32mM0[39m as the ratio denominator and calculated 8 [32mratio[39m
      values for 4 isotopocules ([32m17O[39m, [32m18O[39m, [32m33S[39m, and [32m34S[39m)

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
    -8L), class = "data.frame")

