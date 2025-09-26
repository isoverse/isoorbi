# orbi_identify_isotopocules() [plain]

    Code
      out <- orbi_identify_isotopocules(peaks, isotopologs)
    Message
      !  orbi_identify_isotopocules() identified 11/12 peaks (92%) as isotopcules M0,
      15N, 17O, and 18O but encountered 2 warnings
        > !  isotopocule 15N matches multiple peaks in some same scans (1
        multi-matched peak in total) - make sure to run orbi_flag_satellite_peaks()
        and orbi_plot_satellite_peak()
        > ! isotopocules M0 and 18O are missing from some scans (2 missing peaks in
        total) - make sure to evaluate coverage with e.g.
        orbi_plot_isotopocule_coverage()

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["uidx", "scan.no", "mzMeasured", "intensity", "compound", "isotopocule", "mzExact", "charge"]
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
          "value": [61.9873, 62.9845, 62.9917, 63.9917, "NA", 62.9852, 62.9855, 62.9924, 63.9924, 61.9878, 62.985, 62.9922, "NA", 63.9934]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, "NA", 5, 8, 6, 7, 9, 10, 11, "NA", 12]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["M0", "15N", "17O", "18O", "M0", "15N", "15N", "17O", "18O", "M0", "15N", "17O", "18O", null]
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
        }
      ]
    }

# orbi_identify_isotopocules() [fancy]

    Code
      out <- orbi_identify_isotopocules(peaks, isotopologs)
    Message
      [33m![39m [1morbi_identify_isotopocules()[22m identified 11/12 peaks (92%) as isotopcules [32mM0[39m,
      [32m15N[39m, [32m17O[39m, and [32m18O[39m but encountered [33m2 warnings[39m
        → [33m![39m isotopocule [32m15N[39m matches multiple peaks in some same scans (1
        multi-matched peak in total) - make sure to run [1morbi_flag_satellite_peaks()[22m
        and [1morbi_plot_satellite_peak()[22m
        → [33m![39m isotopocules [32mM0[39m and [32m18O[39m are missing from some scans (2 missing peaks in
        total) - make sure to evaluate coverage with e.g.
        [1morbi_plot_isotopocule_coverage()[22m

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["uidx", "scan.no", "mzMeasured", "intensity", "compound", "isotopocule", "mzExact", "charge"]
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
          "value": [61.9873, 62.9845, 62.9917, 63.9917, "NA", 62.9852, 62.9855, 62.9924, 63.9924, 61.9878, 62.985, 62.9922, "NA", 63.9934]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, "NA", 5, 8, 6, 7, 9, 10, 11, "NA", 12]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate", null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["M0", "15N", "17O", "18O", "M0", "15N", "15N", "17O", "18O", "M0", "15N", "17O", "18O", null]
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
        }
      ]
    }

# orbi_filter_isotopocules() [plain]

    Code
      dataset
    Message
      -------- aggregated data from  raw files - retrieve with orbi_get_data() -------
      > peaks (14): uidx, scan.no, mzMeasured (2 NA), intensity (2 NA), compound (1
      NA), isotopocule (1 NA), mzExact (1 NA), charge (1 NA)

---

    Code
      orbi_filter_isotopocules(dataset)
    Message
      v orbi_filter_isotopocules() removed 3 / 14 peaks (21%) because they were
      missing isotopocules (2), or unidentified peaks (1). Remaining isotopocules:
      M0, 15N, 17O, and 18O.
      -------- aggregated data from  raw files - retrieve with orbi_get_data() -------
      > peaks (11): uidx, scan.no, mzMeasured, intensity, compound, isotopocule,
      mzExact, charge

---

    Code
      out <- orbi_filter_isotopocules(dataset, keep_missing = TRUE)
    Message
      v orbi_filter_isotopocules() removed 1 / 14 peaks (7%) because they were
      unidentified peaks (1). Remaining isotopocules: M0, 15N, 17O, and 18O.

---

    Code
      out <- orbi_filter_isotopocules(dataset, keep_unidentified = TRUE)
    Message
      v orbi_filter_isotopocules() removed 2 / 14 peaks (14%) because they were
      missing isotopocules (2). Remaining isotopocules: M0, 15N, 17O, 18O, and NA.

---

    Code
      out <- orbi_filter_isotopocules(dataset, c("15N", "18O"))
    Message
      v orbi_filter_isotopocules() removed 8 / 14 peaks (57%) because they were
      missing isotopocules (2), unidentified peaks (1), or not the selected
      isotopocule 15N and 18O (5).

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["uidx", "scan.no", "mzMeasured", "intensity", "compound", "isotopocule", "mzExact", "charge"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6]
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
          "value": [1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 2, 2, 2, 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [62.9845, 63.9917, 62.9852, 62.9855, 63.9924, 62.985]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2, 4, 5, 8, 7, 10]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["15N", "18O", "15N", "15N", "18O", "15N"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [62.985, 63.9922, 62.985, 62.985, 63.9922, 62.985]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1]
        }
      ]
    }

# orbi_filter_isotopocules() [fancy]

    Code
      dataset
    Message
      ──────── [1maggregated data from  raw files - retrieve with orbi_get_data()[22m ───────
      → [34mpeaks[39m (14): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m ([33m2 NA[39m), [32mintensity[39m ([33m2 NA[39m), [32mcompound[39m ([33m1[39m
      [33mNA[39m), [32misotopocule[39m ([33m1 NA[39m), [32mmzExact[39m ([33m1 NA[39m), [32mcharge[39m ([33m1 NA[39m)

---

    Code
      orbi_filter_isotopocules(dataset)
    Message
      [32m✔[39m [1morbi_filter_isotopocules()[22m removed 3 / 14 peaks (21%) because they were
      [33mmissing[39m isotopocules (2), or [33munidentified[39m peaks (1). Remaining isotopocules:
      [32mM0[39m, [32m15N[39m, [32m17O[39m, and [32m18O[39m.
      ──────── [1maggregated data from  raw files - retrieve with orbi_get_data()[22m ───────
      → [34mpeaks[39m (11): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mcompound[39m, [32misotopocule[39m,
      [32mmzExact[39m, [32mcharge[39m

---

    Code
      out <- orbi_filter_isotopocules(dataset, keep_missing = TRUE)
    Message
      [32m✔[39m [1morbi_filter_isotopocules()[22m removed 1 / 14 peaks (7%) because they were
      [33munidentified[39m peaks (1). Remaining isotopocules: [32mM0[39m, [32m15N[39m, [32m17O[39m, and [32m18O[39m.

---

    Code
      out <- orbi_filter_isotopocules(dataset, keep_unidentified = TRUE)
    Message
      [32m✔[39m [1morbi_filter_isotopocules()[22m removed 2 / 14 peaks (14%) because they were
      [33mmissing[39m isotopocules (2). Remaining isotopocules: [32mM0[39m, [32m15N[39m, [32m17O[39m, [32m18O[39m, and [32mNA[39m.

---

    Code
      out <- orbi_filter_isotopocules(dataset, c("15N", "18O"))
    Message
      [32m✔[39m [1morbi_filter_isotopocules()[22m removed 8 / 14 peaks (57%) because they were
      [33mmissing[39m isotopocules (2), [33munidentified[39m peaks (1), or [33mnot[39m the [33mselected[39m
      isotopocule [32m15N[39m and [32m18O[39m (5).

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["uidx", "scan.no", "mzMeasured", "intensity", "compound", "isotopocule", "mzExact", "charge"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6]
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
          "value": [1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 2, 2, 2, 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [62.9845, 63.9917, 62.9852, 62.9855, 63.9924, 62.985]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2, 4, 5, 8, 7, 10]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["nitrate", "nitrate", "nitrate", "nitrate", "nitrate", "nitrate"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["15N", "18O", "15N", "15N", "18O", "15N"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [62.985, 63.9922, 62.985, 62.985, 63.9922, 62.985]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1]
        }
      ]
    }

