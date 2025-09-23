# orbi_get_options() [plain]

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
      
      $aggregators
      $aggregators$minimal
    Message
      ------------------------------ Aggregator minimal ------------------------------
      Dataset file_info:
       > filename = as.character(sub(FileName, pattern = ".raw", replacement = "",
      fixed = TRUE))
       > creation_date = as.POSIXct(CreationDate)
      Dataset scans:
       > scan.no = as.integer(scan.no)
       > time.min = as.numeric(StartTime)
       > tic = as.numeric(TIC)
       > it.ms = as.numeric(`Ion Injection Time (ms)`)
       > resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
      Dataset peaks:
       > scan.no = as.integer(scan.no)
       > mzMeasured = as.numeric(mass)
       > intensity = as.numeric(intensity)
       > baseline = as.numeric(baseline)
       > peakNoise = as.numeric(noise)
       > peakResolution = as.numeric(resolution)
       > isRefPeak = as.logical(is_ref)
       > isLockPeak = as.logical(is_lock_peak)
      Dataset spectra:
       > scan.no = as.integer(scan.no)
       > mz = as.numeric(mass)
       > intensity = as.numeric(intensity)
    Output
      
      $aggregators$standard
    Message
      ------------------------------ Aggregator standard -----------------------------
      Dataset file_info:
       > filename = as.character(sub(FileName, pattern = ".raw", replacement = "",
      fixed = TRUE))
       > creation_date = as.POSIXct(CreationDate)
       > (.*) = as.character(all_matches("(.*)"))
      Dataset scans:
       > scan.no = as.integer(scan.no)
       > time.min = as.numeric(StartTime)
       > tic = as.numeric(TIC)
       > it.ms = as.numeric(`Ion Injection Time (ms)`)
       > resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
       > basePeakMz = as.numeric(BasePeakMass)
       > basePeakIntensity = as.numeric(BasePeakIntensity)
       > lowMass = as.numeric(LowMass)
       > highMass = as.numeric(HighMass)
       > rawOvFtT = as.numeric(RawOvFtT)
       > intensCompFactor = as.numeric(`OT Intens Comp Factor`)
       > agc = as.character(AGC)
       > agcTarget = as.integer(`AGC Target`)
       > microscans = as.integer(`Micro Scan Count`)
       > numberLockmassesFound = as.integer(`Number of LM Found`)
       > analyzerTemperature = as.numeric(`Analyzer Temperature`)
      Dataset peaks:
       > scan.no = as.integer(scan.no)
       > mzMeasured = as.numeric(mass)
       > intensity = as.numeric(intensity)
       > baseline = as.numeric(baseline)
       > peakNoise = as.numeric(noise)
       > peakResolution = as.numeric(resolution)
       > isRefPeak = as.logical(is_ref)
       > isLockPeak = as.logical(is_lock_peak)
      Dataset spectra:
       > scan.no = as.integer(scan.no)
       > mz = as.numeric(mass)
       > intensity = as.numeric(intensity)
    Output
      
      $aggregators$extended
    Message
      ------------------------------ Aggregator extended -----------------------------
      Dataset file_info:
       > filename = as.character(sub(FileName, pattern = ".raw", replacement = "",
      fixed = TRUE))
       > creation_date = as.POSIXct(CreationDate)
       > (.*) = as.character(all_matches("(.*)"))
      Dataset scans:
       > scan.no = as.integer(scan.no)
       > time.min = as.numeric(StartTime)
       > tic = as.numeric(TIC)
       > it.ms = as.numeric(`Ion Injection Time (ms)`)
       > resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
       > basePeakMz = as.numeric(BasePeakMass)
       > basePeakIntensity = as.numeric(BasePeakIntensity)
       > lowMass = as.numeric(LowMass)
       > highMass = as.numeric(HighMass)
       > rawOvFtT = as.numeric(RawOvFtT)
       > intensCompFactor = as.numeric(`OT Intens Comp Factor`)
       > agc = as.character(AGC)
       > agcTarget = as.integer(`AGC Target`)
       > microscans = as.integer(`Micro Scan Count`)
       > numberLockmassesFound = as.integer(`Number of LM Found`)
       > analyzerTemperature = as.numeric(`Analyzer Temperature`)
       > (.*) = as.character(all_matches("(.*)"))
      Dataset peaks:
       > scan.no = as.integer(scan.no)
       > mzMeasured = as.numeric(mass)
       > intensity = as.numeric(intensity)
       > baseline = as.numeric(baseline)
       > peakNoise = as.numeric(noise)
       > peakResolution = as.numeric(resolution)
       > isRefPeak = as.logical(is_ref)
       > isLockPeak = as.logical(is_lock_peak)
      Dataset spectra:
       > scan.no = as.integer(scan.no)
       > mz = as.numeric(mass)
       > intensity = as.numeric(intensity)
    Output
      
      
      $debug
      [1] FALSE
      
      $auto_use_ansi
      [1] TRUE
      

# orbi_get_options() [fancy]

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
      
      $aggregators
      $aggregators$minimal
    Message
      ────────────────────────────── [1mAggregator [3mminimal[23m[22m ──────────────────────────────
      [1mDataset[22m [34mfile_info[39m:
       → [32mfilename[39m = [3mas.character(sub(FileName, pattern = ".raw", replacement = "",[23m
      [3mfixed = TRUE))[23m
       → [32mcreation_date[39m = [3mas.POSIXct(CreationDate)[23m
      [1mDataset[22m [34mscans[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mtime.min[39m = [3mas.numeric(StartTime)[23m
       → [32mtic[39m = [3mas.numeric(TIC)[23m
       → [32mit.ms[39m = [3mas.numeric(`Ion Injection Time (ms)`)[23m
       → [32mresolution[39m = [3mas.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))[23m
      [1mDataset[22m [34mpeaks[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mmzMeasured[39m = [3mas.numeric(mass)[23m
       → [32mintensity[39m = [3mas.numeric(intensity)[23m
       → [32mbaseline[39m = [3mas.numeric(baseline)[23m
       → [32mpeakNoise[39m = [3mas.numeric(noise)[23m
       → [32mpeakResolution[39m = [3mas.numeric(resolution)[23m
       → [32misRefPeak[39m = [3mas.logical(is_ref)[23m
       → [32misLockPeak[39m = [3mas.logical(is_lock_peak)[23m
      [1mDataset[22m [34mspectra[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mmz[39m = [3mas.numeric(mass)[23m
       → [32mintensity[39m = [3mas.numeric(intensity)[23m
    Output
      
      $aggregators$standard
    Message
      ────────────────────────────── [1mAggregator [3mstandard[23m[22m ─────────────────────────────
      [1mDataset[22m [34mfile_info[39m:
       → [32mfilename[39m = [3mas.character(sub(FileName, pattern = ".raw", replacement = "",[23m
      [3mfixed = TRUE))[23m
       → [32mcreation_date[39m = [3mas.POSIXct(CreationDate)[23m
       → [35m(.*)[39m = [3mas.character(all_matches("(.*)"))[23m
      [1mDataset[22m [34mscans[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mtime.min[39m = [3mas.numeric(StartTime)[23m
       → [32mtic[39m = [3mas.numeric(TIC)[23m
       → [32mit.ms[39m = [3mas.numeric(`Ion Injection Time (ms)`)[23m
       → [32mresolution[39m = [3mas.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))[23m
       → [32mbasePeakMz[39m = [3mas.numeric(BasePeakMass)[23m
       → [32mbasePeakIntensity[39m = [3mas.numeric(BasePeakIntensity)[23m
       → [32mlowMass[39m = [3mas.numeric(LowMass)[23m
       → [32mhighMass[39m = [3mas.numeric(HighMass)[23m
       → [32mrawOvFtT[39m = [3mas.numeric(RawOvFtT)[23m
       → [32mintensCompFactor[39m = [3mas.numeric(`OT Intens Comp Factor`)[23m
       → [32magc[39m = [3mas.character(AGC)[23m
       → [32magcTarget[39m = [3mas.integer(`AGC Target`)[23m
       → [32mmicroscans[39m = [3mas.integer(`Micro Scan Count`)[23m
       → [32mnumberLockmassesFound[39m = [3mas.integer(`Number of LM Found`)[23m
       → [32manalyzerTemperature[39m = [3mas.numeric(`Analyzer Temperature`)[23m
      [1mDataset[22m [34mpeaks[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mmzMeasured[39m = [3mas.numeric(mass)[23m
       → [32mintensity[39m = [3mas.numeric(intensity)[23m
       → [32mbaseline[39m = [3mas.numeric(baseline)[23m
       → [32mpeakNoise[39m = [3mas.numeric(noise)[23m
       → [32mpeakResolution[39m = [3mas.numeric(resolution)[23m
       → [32misRefPeak[39m = [3mas.logical(is_ref)[23m
       → [32misLockPeak[39m = [3mas.logical(is_lock_peak)[23m
      [1mDataset[22m [34mspectra[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mmz[39m = [3mas.numeric(mass)[23m
       → [32mintensity[39m = [3mas.numeric(intensity)[23m
    Output
      
      $aggregators$extended
    Message
      ────────────────────────────── [1mAggregator [3mextended[23m[22m ─────────────────────────────
      [1mDataset[22m [34mfile_info[39m:
       → [32mfilename[39m = [3mas.character(sub(FileName, pattern = ".raw", replacement = "",[23m
      [3mfixed = TRUE))[23m
       → [32mcreation_date[39m = [3mas.POSIXct(CreationDate)[23m
       → [35m(.*)[39m = [3mas.character(all_matches("(.*)"))[23m
      [1mDataset[22m [34mscans[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mtime.min[39m = [3mas.numeric(StartTime)[23m
       → [32mtic[39m = [3mas.numeric(TIC)[23m
       → [32mit.ms[39m = [3mas.numeric(`Ion Injection Time (ms)`)[23m
       → [32mresolution[39m = [3mas.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))[23m
       → [32mbasePeakMz[39m = [3mas.numeric(BasePeakMass)[23m
       → [32mbasePeakIntensity[39m = [3mas.numeric(BasePeakIntensity)[23m
       → [32mlowMass[39m = [3mas.numeric(LowMass)[23m
       → [32mhighMass[39m = [3mas.numeric(HighMass)[23m
       → [32mrawOvFtT[39m = [3mas.numeric(RawOvFtT)[23m
       → [32mintensCompFactor[39m = [3mas.numeric(`OT Intens Comp Factor`)[23m
       → [32magc[39m = [3mas.character(AGC)[23m
       → [32magcTarget[39m = [3mas.integer(`AGC Target`)[23m
       → [32mmicroscans[39m = [3mas.integer(`Micro Scan Count`)[23m
       → [32mnumberLockmassesFound[39m = [3mas.integer(`Number of LM Found`)[23m
       → [32manalyzerTemperature[39m = [3mas.numeric(`Analyzer Temperature`)[23m
       → [35m(.*)[39m = [3mas.character(all_matches("(.*)"))[23m
      [1mDataset[22m [34mpeaks[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mmzMeasured[39m = [3mas.numeric(mass)[23m
       → [32mintensity[39m = [3mas.numeric(intensity)[23m
       → [32mbaseline[39m = [3mas.numeric(baseline)[23m
       → [32mpeakNoise[39m = [3mas.numeric(noise)[23m
       → [32mpeakResolution[39m = [3mas.numeric(resolution)[23m
       → [32misRefPeak[39m = [3mas.logical(is_ref)[23m
       → [32misLockPeak[39m = [3mas.logical(is_lock_peak)[23m
      [1mDataset[22m [34mspectra[39m:
       → [32mscan.no[39m = [3mas.integer(scan.no)[23m
       → [32mmz[39m = [3mas.numeric(mass)[23m
       → [32mintensity[39m = [3mas.numeric(intensity)[23m
    Output
      
      
      $debug
      [1] FALSE
      
      $auto_use_ansi
      [1] TRUE
      

