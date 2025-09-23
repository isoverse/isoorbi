# orbi_start_aggregator() [plain]

    Code
      orbi_add_to_aggregator(orbi_add_to_aggregator(orbi_add_to_aggregator(
        orbi_add_to_aggregator(orbi_add_to_aggregator(orbi_start_aggregator("test"),
        "data", "col"), "data", "num", cast = "as.integer"), "data", "new", source = c(
          "def", "alt def"), default = 4), "data", "w\\1_\\2", "(\\d+)-(.*)", regexp = TRUE),
      "data", "from_fun", cast = "as.integer", source = list(c("a", "b"), "x"), func = "mean")
    Message
      -------------------------------- Aggregator test -------------------------------
      Dataset data:
       > col = as.character(col)
       > num = as.integer(num)
       > new = as.character(one_of(def, `alt def`)) - if source is missing: new =
      as.character(4)
       > w(\\d+)_(.*) = as.character(all_matches("(\\d+)-(.*)"))
       > from_fun = as.integer(mean(one_of(a, b), x))

# orbi_start_aggregator() [fancy]

    Code
      orbi_add_to_aggregator(orbi_add_to_aggregator(orbi_add_to_aggregator(
        orbi_add_to_aggregator(orbi_add_to_aggregator(orbi_start_aggregator("test"),
        "data", "col"), "data", "num", cast = "as.integer"), "data", "new", source = c(
          "def", "alt def"), default = 4), "data", "w\\1_\\2", "(\\d+)-(.*)", regexp = TRUE),
      "data", "from_fun", cast = "as.integer", source = list(c("a", "b"), "x"), func = "mean")
    Message
      ──────────────────────────────── [1mAggregator [3mtest[23m[22m ───────────────────────────────
      [1mDataset[22m [34mdata[39m:
       → [32mcol[39m = [3mas.character(col)[23m
       → [32mnum[39m = [3mas.integer(num)[23m
       → [32mnew[39m = [3mas.character(one_of(def, `alt def`))[23m - [33mif source is missing[39m: [32mnew[39m =
      [3mas.character(4)[23m
       → [35mw(\\d+)_(.*)[39m = [3mas.character(all_matches("(\\d+)-(.*)"))[23m
       → [32mfrom_fun[39m = [3mas.integer(mean(one_of(a, b), x))[23m

