# cli [plain]

    Code
      res <- orbi_summarize_results(df, ratio_method = "sum")
    Message
      v orbi_summarize_results() summarized ratios from 4320 peaks using the sum
      method and grouping the data by filename, compound, basepeak, and isotopocule

# cli [fancy]

    Code
      res <- orbi_summarize_results(df, ratio_method = "sum")
    Message
      [32m✔[39m [1morbi_summarize_results()[22m summarized ratios from 4320 peaks using the [1m[3msum[23m[22m
      method and grouping the data by [32mfilename[39m, [32mcompound[39m, [32mbasepeak[39m, and [32misotopocule[39m

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["compound", "basepeak", "isotopocule", "start_scan.no", "end_scan.no", "start_time.min", "mean_time.min", "end_time.min", "ratio", "ratio_relative_sem_permil", "shot_noise_permil", "ratio_sem", "minutes_to_1e6_ions", "number_of_scans"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["NO3-"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["15N", "15N", "15N", "15N", "15N"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["17O", "18O", "15N18O", "17O18O", "18O18O"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 2, 3, 4, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [864, 864, 864, 864, 864]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.005, 0.005, 0.005, 0.005, 0.005]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.50208565, 1.50208565, 1.50208565, 1.50208565, 1.50208565]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [3, 3, 3, 3, 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.30092586, 2.24491413, 0.01046472, 0.00203254, 0.00567774]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.032, 1.678, 1.784, 5.183, 2.6]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.256, 0.148, 1.208, 2.729, 1.636]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.00061158, 0.00376668, 0.00001867, 0.00001053, 0.00001476]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.15, 0.02, 4.32, 22.26, 7.97]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [864, 864, 864, 864, 864]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["filename", "compound", "basepeak", "isotopocule", "block", "sample_name", "segment", "injection", "start_scan.no", "end_scan.no", "start_time.min", "mean_time.min", "end_time.min", "ratio", "ratio_relative_sem_permil", "shot_noise_permil", "ratio_sem", "minutes_to_1e6_ions", "number_of_scans"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["20220125_01"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["NO3-"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["15N", "15N", "15N", "15N", "15N"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["17O", "18O", "15N18O", "17O18O", "18O18O"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 2, 3, 4, 5]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["block1"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["sample_name4"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["segment2"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["injection3"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [864, 864, 864, 864, 864]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.005, 0.005, 0.005, 0.005, 0.005]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.50208565, 1.50208565, 1.50208565, 1.50208565, 1.50208565]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [3, 3, 3, 3, 3]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.30162744, 2.2492349, 0.01045674, 0.00203519, 0.00568482]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.028, 1.675, 1.786, 5.176, 2.597]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.256, 0.148, 1.208, 2.729, 1.636]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.00061158, 0.00376668, 0.00001867, 0.00001053, 0.00001476]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.15, 0.02, 4.32, 22.26, 7.97]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [864, 864, 864, 864, 864]
        }
      ]
    }

