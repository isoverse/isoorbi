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
          "value": ["uidx", "scan.no", "mzMeasured", "intensity", "compound", "isotopocule", "mzNominal", "charge"]
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
          "value": ["uidx", "scan.no", "mzMeasured", "intensity", "compound", "isotopocule", "mzNominal", "charge"]
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

