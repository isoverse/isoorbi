# orbi_read_raw() / cli [plain]

    Code
      x <- orbi_read_raw(orbi_find_raw(system.file("extdata", package = "isoorbi"),
      pattern = "nitrate"), read_cache = TRUE, cache = FALSE)
    Message
      v orbi_read_raw() read 'nitrate_test_10scans.raw' from cache
      v orbi_read_raw() read 'nitrate_test_1scan.raw' from cache
      v orbi_read_raw() finished reading 2 files

---

    Code
      x
    Message
      ---------------- 2 raw files - combine with orbi_aggregate_raw() ---------------
      1. nitrate_test_10scans.raw has 10 scans with 307 peaks and 1 status log entry;
      no spectra were loaded
      2. nitrate_test_1scan.raw   has  1 scans with  30 peaks and 1 status log entry;
      no spectra were loaded

---

    Code
      y <- orbi_aggregate_raw(x)
    Message
      v orbi_aggregate_raw() aggregated file_info (2), scans (11), peaks (337),
      spectra (0), and status_log (0) from 2 files using the standard aggregator

---

    Code
      y
    Message
      ------- aggregated data from 2 raw files - retrieve with orbi_get_data() -------
      > file_info (2): uidx, filepath, filename, creation_date, in_aquisition,
      Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
      LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
      InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
      HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
      SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
      InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
      InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
      UserText1, UserText2, UserText3, UserText4
      > scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
      basePeakMz, basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor,
      agc, agcTarget, numberLockmassesFound, analyzerTemperature; not aggregated: 65
      columns > to show all use: print(x, show_all = TRUE)
      > peaks (337): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, centroiderFlags
      > spectra (0): uidx, scan.no, mz, intensity
      > status_log (0): nothing aggregated; not aggregated: 198 columns > to show all
      use: print(x, show_all = TRUE)
      > problems: has no issues

---

    Code
      out <- orbi_get_data(y, scans = everything(), spectra = everything())
    Message
      v orbi_get_data() retrieved 0 records from the combination of file_info (2),
      scans (11), and spectra (0) via uidx and scan.no
      ! Warning: there are no spectra in the data, make sure to include them when
      reading the raw files e.g. with orbi_read_raw(include_spectra = c(1, 10, 100))

---

    Code
      x <- orbi_read_raw(orbi_find_raw(system.file("extdata", package = "isoorbi"),
      pattern = "nitrate"), read_cache = TRUE, cache = FALSE, include_spectra = 1)
    Message
      v orbi_read_raw() read 'nitrate_test_10scans.raw' from cache, included the
      spectrum from 1 scan
      v orbi_read_raw() read 'nitrate_test_1scan.raw' from cache, included the
      spectrum from 1 scan
      v orbi_read_raw() finished reading 2 files

---

    Code
      x
    Message
      ---------------- 2 raw files - combine with orbi_aggregate_raw() ---------------
      1. nitrate_test_10scans.raw has 10 scans with 307 peaks and 1 status log entry;
      + loaded 1 spectrum (350 points)
      2. nitrate_test_1scan.raw   has  1 scans with  30 peaks and 1 status log entry;
      + loaded 1 spectrum (325 points)

---

    Code
      y <- orbi_aggregate_raw(x, aggregator = "extended")
    Message
      v orbi_aggregate_raw() aggregated file_info (2), scans (11), peaks (337),
      spectra (675), and status_log (0) from 2 files using the extended aggregator

---

    Code
      y
    Message
      ------- aggregated data from 2 raw files - retrieve with orbi_get_data() -------
      > file_info (2): uidx, filepath, filename, creation_date, in_aquisition,
      Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
      LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
      InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
      HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
      SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
      InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
      InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
      UserText1, UserText2, UserText3, UserText4
      > scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
      basePeakMz, basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor,
      agc, agcTarget, numberLockmassesFound, analyzerTemperature, IsCentroidScan,
      ScanType, Scan Description, Multiple Injection, Multi Inject Info, Scan
      Segment, Scan Event, Master Index, Master Scan Number, Charge State,
      Monoisotopic M/Z, Error in isotopic envelope fit, Max. Ion Time (ms), MS2
      Isolation Width, MS2 Isolation Offset, HCD Energy, HCD Energy V, === Mass
      Calibration: ===, Conversion Parameter B, Conversion Parameter C, Temperature
      Comp. (ppm), RF Comp. (ppm), Space Charge Comp. (ppm), Resolution Comp. (ppm),
      Number of Lock Masses, Lock Mass #1 (m/z), Lock Mass #2 (m/z), Lock Mass #3
      (m/z), LM Search Window (ppm), LM Search Window (mmu), Last Locking (sec), LM
      m/z-Correction (ppm), === Ion Optics Settings: ===, S-Lens RF Level, ====
      Diagnostic Data: ====, Application Mode, Mild Trapping Mode, APD, Res. Dep.
      Intens, Q Trans Comp, PrOSA NumF, PrOSA Comp, PrOSA ScScr, Dynamic RT Shift
      (min), Analytical OT usage (%), LC FWHM parameter, PS Inj. Time (ms), AGC PS
      Mode, AGC PS Diag, AGC Target Adjust, AGC Diag 1, AGC Diag 2, HCD abs. Offset,
      Source CID eV, AGC Fill, Injection t0, t0 FLP, Iso Para R, Inj Para R, Access
      Id, Analog In A (V), Analog In B (V), FAIMS Attached, FAIMS Voltage On, FAIMS
      CV
      > peaks (337): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, centroiderFlags
      > spectra (675): uidx, scan.no, mz, intensity
      > status_log (0): nothing aggregated; not aggregated: 198 columns > to show all
      use: print(x, show_all = TRUE)
      > problems: has no issues

---

    Code
      y <- orbi_aggregate_raw(x, aggregator = "minimal")
    Message
      v orbi_aggregate_raw() aggregated file_info (2), scans (11), peaks (337),
      spectra (675), and status_log (0) from 2 files using the minimal aggregator

---

    Code
      y
    Message
      ------- aggregated data from 2 raw files - retrieve with orbi_get_data() -------
      > file_info (2): uidx, filepath, filename, creation_date, in_aquisition; not
      aggregated: 39 columns > to show all use: print(x, show_all = TRUE)
      > scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, microscans; not
      aggregated: 75 columns > to show all use: print(x, show_all = TRUE)
      > peaks (337): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, centroiderFlags
      > spectra (675): uidx, scan.no, mz, intensity
      > status_log (0): nothing aggregated; not aggregated: 198 columns > to show all
      use: print(x, show_all = TRUE)
      > problems: has no issues

---

    Code
      z <- orbi_identify_isotopocules(y, isotopologs)
    Message
      !  orbi_identify_isotopocules() identified 50/337 peaks (15%) representing 96%
      of the total ion current (TIC) as isotopocules M0, 15N, 17O, and 18O but
      encountered 1 warning
        > !  isotopocule M0 matches multiple peaks in some same scans (4
        multi-matched peaks in total) - make sure to run orbi_flag_satellite_peaks()
        and orbi_plot_satellite_peak()

---

    Code
      out <- orbi_get_data(y, scans = everything(), spectra = everything())
    Message
      v orbi_get_data() retrieved 675 records from the combination of file_info (2),
      scans (11), and spectra (675) via uidx and scan.no

# orbi_read_raw() / cli [fancy]

    Code
      x <- orbi_read_raw(orbi_find_raw(system.file("extdata", package = "isoorbi"),
      pattern = "nitrate"), read_cache = TRUE, cache = FALSE)
    Message
      [32m✔[39m [1morbi_read_raw()[22m read [34mnitrate_test_10scans.raw[39m from cache
      [32m✔[39m [1morbi_read_raw()[22m read [34mnitrate_test_1scan.raw[39m from cache
      [32m✔[39m [1morbi_read_raw()[22m finished reading 2 files

---

    Code
      x
    Message
      ──────────────── [1m2 raw files - combine with orbi_aggregate_raw()[22m ───────────────
      1. [34mnitrate_test_10scans.raw[39m has 10 [32mscans[39m with 307 [32mpeaks[39m and 1 [32mstatus log[39m entry;
      no [32mspectra[39m were loaded
      2. [34mnitrate_test_1scan.raw[39m   has  1 [32mscans[39m with  30 [32mpeaks[39m and 1 [32mstatus log[39m entry;
      no [32mspectra[39m were loaded

---

    Code
      y <- orbi_aggregate_raw(x)
    Message
      [32m✔[39m [1morbi_aggregate_raw()[22m aggregated [34mfile_info[39m (2), [34mscans[39m (11), [34mpeaks[39m (337),
      [34mspectra[39m (0), and [34mstatus_log[39m (0) from 2 files using the [1m[3mstandard[23m[22m aggregator

---

    Code
      y
    Message
      ─────── [1maggregated data from 2 raw files - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (2): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32min_aquisition[39m,
      [32mOperator[39m, [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m,
      [32mLastSpectrum[39m, [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m,
      [32mInstrumentModel[39m, [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m,
      [32mHardwareVersion[39m, [32mRawFileVersion[39m, [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m,
      [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m, [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m,
      [32mInjectionVolume[39m, [32mDilutionFactor[39m, [32mIstdAmount[39m, [32mCalibrationLevel[39m,
      [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m, [32mProcessingMethodFile[39m, [32mUserText0[39m,
      [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (11): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mmicroscans[39m,
      [32mbasePeakMz[39m, [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m,
      [32magc[39m, [32magcTarget[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m; [3mnot aggregated[23m: [33m65[39m
      [33mcolumns[39m → to show all use: [3mprint(x, show_all = TRUE)[23m
      → [34mpeaks[39m (337): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32mcentroiderFlags[39m
      → [34mspectra[39m (0): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mstatus_log[39m (0): [3mnothing aggregated[23m; [3mnot aggregated[23m: [33m198 columns[39m → to show all
      use: [3mprint(x, show_all = TRUE)[23m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      out <- orbi_get_data(y, scans = everything(), spectra = everything())
    Message
      [32m✔[39m [1morbi_get_data()[22m retrieved 0 records from the combination of [34mfile_info[39m (2),
      [34mscans[39m (11), and [34mspectra[39m (0) via [32muidx[39m and [32mscan.no[39m
      [33m![39m [1mWarning[22m: there are no [32mspectra[39m in the data, make sure to include them when
      reading the raw files e.g. with [1morbi_read_raw(include_spectra = c(1, 10, 100))[22m

---

    Code
      x <- orbi_read_raw(orbi_find_raw(system.file("extdata", package = "isoorbi"),
      pattern = "nitrate"), read_cache = TRUE, cache = FALSE, include_spectra = 1)
    Message
      [32m✔[39m [1morbi_read_raw()[22m read [34mnitrate_test_10scans.raw[39m from cache, included the
      [32mspectrum[39m from 1 [32mscan[39m
      [32m✔[39m [1morbi_read_raw()[22m read [34mnitrate_test_1scan.raw[39m from cache, included the [32mspectrum[39m
      from 1 [32mscan[39m
      [32m✔[39m [1morbi_read_raw()[22m finished reading 2 files

---

    Code
      x
    Message
      ──────────────── [1m2 raw files - combine with orbi_aggregate_raw()[22m ───────────────
      1. [34mnitrate_test_10scans.raw[39m has 10 [32mscans[39m with 307 [32mpeaks[39m and 1 [32mstatus log[39m entry;
      + loaded 1 [32mspectrum[39m (350 points)
      2. [34mnitrate_test_1scan.raw[39m   has  1 [32mscans[39m with  30 [32mpeaks[39m and 1 [32mstatus log[39m entry;
      + loaded 1 [32mspectrum[39m (325 points)

---

    Code
      y <- orbi_aggregate_raw(x, aggregator = "extended")
    Message
      [32m✔[39m [1morbi_aggregate_raw()[22m aggregated [34mfile_info[39m (2), [34mscans[39m (11), [34mpeaks[39m (337),
      [34mspectra[39m (675), and [34mstatus_log[39m (0) from 2 files using the [1m[3mextended[23m[22m aggregator

---

    Code
      y
    Message
      ─────── [1maggregated data from 2 raw files - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (2): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32min_aquisition[39m,
      [32mOperator[39m, [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m,
      [32mLastSpectrum[39m, [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m,
      [32mInstrumentModel[39m, [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m,
      [32mHardwareVersion[39m, [32mRawFileVersion[39m, [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m,
      [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m, [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m,
      [32mInjectionVolume[39m, [32mDilutionFactor[39m, [32mIstdAmount[39m, [32mCalibrationLevel[39m,
      [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m, [32mProcessingMethodFile[39m, [32mUserText0[39m,
      [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (11): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mmicroscans[39m,
      [32mbasePeakMz[39m, [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m,
      [32magc[39m, [32magcTarget[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m, [32mIsCentroidScan[39m,
      [32mScanType[39m, [32mScan Description[39m, [32mMultiple Injection[39m, [32mMulti Inject Info[39m, [32mScan[39m
      [32mSegment[39m, [32mScan Event[39m, [32mMaster Index[39m, [32mMaster Scan Number[39m, [32mCharge State[39m,
      [32mMonoisotopic M/Z[39m, [32mError in isotopic envelope fit[39m, [32mMax. Ion Time (ms)[39m, [32mMS2[39m
      [32mIsolation Width[39m, [32mMS2 Isolation Offset[39m, [32mHCD Energy[39m, [32mHCD Energy V[39m, [32m=== Mass[39m
      [32mCalibration: ===[39m, [32mConversion Parameter B[39m, [32mConversion Parameter C[39m, [32mTemperature[39m
      [32mComp. (ppm)[39m, [32mRF Comp. (ppm)[39m, [32mSpace Charge Comp. (ppm)[39m, [32mResolution Comp. (ppm)[39m,
      [32mNumber of Lock Masses[39m, [32mLock Mass #1 (m/z)[39m, [32mLock Mass #2 (m/z)[39m, [32mLock Mass #3[39m
      [32m(m/z)[39m, [32mLM Search Window (ppm)[39m, [32mLM Search Window (mmu)[39m, [32mLast Locking (sec)[39m, [32mLM[39m
      [32mm/z-Correction (ppm)[39m, [32m=== Ion Optics Settings: ===[39m, [32mS-Lens RF Level[39m, [32m====[39m
      [32mDiagnostic Data: ====[39m, [32mApplication Mode[39m, [32mMild Trapping Mode[39m, [32mAPD[39m, [32mRes. Dep.[39m
      [32mIntens[39m, [32mQ Trans Comp[39m, [32mPrOSA NumF[39m, [32mPrOSA Comp[39m, [32mPrOSA ScScr[39m, [32mDynamic RT Shift[39m
      [32m(min)[39m, [32mAnalytical OT usage (%)[39m, [32mLC FWHM parameter[39m, [32mPS Inj. Time (ms)[39m, [32mAGC PS[39m
      [32mMode[39m, [32mAGC PS Diag[39m, [32mAGC Target Adjust[39m, [32mAGC Diag 1[39m, [32mAGC Diag 2[39m, [32mHCD abs. Offset[39m,
      [32mSource CID eV[39m, [32mAGC Fill[39m, [32mInjection t0[39m, [32mt0 FLP[39m, [32mIso Para R[39m, [32mInj Para R[39m, [32mAccess[39m
      [32mId[39m, [32mAnalog In A (V)[39m, [32mAnalog In B (V)[39m, [32mFAIMS Attached[39m, [32mFAIMS Voltage On[39m, [32mFAIMS[39m
      [32mCV[39m
      → [34mpeaks[39m (337): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32mcentroiderFlags[39m
      → [34mspectra[39m (675): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mstatus_log[39m (0): [3mnothing aggregated[23m; [3mnot aggregated[23m: [33m198 columns[39m → to show all
      use: [3mprint(x, show_all = TRUE)[23m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      y <- orbi_aggregate_raw(x, aggregator = "minimal")
    Message
      [32m✔[39m [1morbi_aggregate_raw()[22m aggregated [34mfile_info[39m (2), [34mscans[39m (11), [34mpeaks[39m (337),
      [34mspectra[39m (675), and [34mstatus_log[39m (0) from 2 files using the [1m[3mminimal[23m[22m aggregator

---

    Code
      y
    Message
      ─────── [1maggregated data from 2 raw files - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (2): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32min_aquisition[39m; [3mnot[23m
      [3maggregated[23m: [33m39 columns[39m → to show all use: [3mprint(x, show_all = TRUE)[23m
      → [34mscans[39m (11): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mmicroscans[39m; [3mnot[23m
      [3maggregated[23m: [33m75 columns[39m → to show all use: [3mprint(x, show_all = TRUE)[23m
      → [34mpeaks[39m (337): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32mcentroiderFlags[39m
      → [34mspectra[39m (675): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mstatus_log[39m (0): [3mnothing aggregated[23m; [3mnot aggregated[23m: [33m198 columns[39m → to show all
      use: [3mprint(x, show_all = TRUE)[23m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      z <- orbi_identify_isotopocules(y, isotopologs)
    Message
      [33m![39m [1morbi_identify_isotopocules()[22m identified 50/337 peaks (15%) representing 96%
      of the total ion current (TIC) as isotopocules [32mM0[39m, [32m15N[39m, [32m17O[39m, and [32m18O[39m but
      encountered [33m1 warning[39m
        → [33m![39m isotopocule [32mM0[39m matches multiple peaks in some same scans (4 multi-matched
        peaks in total) - make sure to run [1morbi_flag_satellite_peaks()[22m and
        [1morbi_plot_satellite_peak()[22m

---

    Code
      out <- orbi_get_data(y, scans = everything(), spectra = everything())
    Message
      [32m✔[39m [1morbi_get_data()[22m retrieved 675 records from the combination of [34mfile_info[39m (2),
      [34mscans[39m (11), and [34mspectra[39m (675) via [32muidx[39m and [32mscan.no[39m

# orbi_read_raw() and orbi_aggregate_raw() with a status log / cli [plain]

    Code
      print(x)
    Message
      ---------------- 1 raw file - process with orbi_aggregate_raw() ----------------
      1. nitrate_test_1scan.raw has 1 scans with 30 peaks and 3 status log entries;
      no spectra were loaded

---

    Code
      print(y)
    Message
      -------- aggregated data from 1 raw file - retrieve with orbi_get_data() -------
      > file_info (1): uidx, filepath, filename, creation_date, in_aquisition,
      Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
      LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
      InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
      HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
      SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
      InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
      InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
      UserText1, UserText2, UserText3, UserText4
      > scans (1): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
      basePeakMz, basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor,
      agc, agcTarget, numberLockmassesFound, analyzerTemperature; not aggregated: 65
      columns > to show all use: print(y, show_all = TRUE)
      > peaks (30): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, centroiderFlags
      > spectra (0): uidx, scan.no, mz, intensity
      > status_log (0): nothing aggregated; not aggregated: 6 columns > to show all
      use: print(y, show_all = TRUE)
      > problems: has no issues

---

    Code
      print(y2)
    Message
      -------- aggregated data from 1 raw file - retrieve with orbi_get_data() -------
      > file_info (1): uidx, filepath, filename, creation_date, in_aquisition,
      Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
      LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
      InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
      HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
      SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
      InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
      InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
      UserText1, UserText2, UserText3, UserText4
      > scans (1): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
      basePeakMz, basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor,
      agc, agcTarget, numberLockmassesFound, analyzerTemperature; not aggregated: 65
      columns > to show all use: print(y2, show_all = TRUE)
      > peaks (30): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, centroiderFlags
      > spectra (0): uidx, scan.no, mz, intensity
      > status_log (3): uidx, log.no, time.min, ambientTemperature; not aggregated: 3
      columns > to show all use: print(y2, show_all = TRUE)
      > problems: has no issues

---

    Code
      print(y2, show_all = TRUE)
    Message
      -------- aggregated data from 1 raw file - retrieve with orbi_get_data() -------
      > file_info (1): uidx, filepath, filename, creation_date, in_aquisition,
      Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
      LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
      InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
      HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
      SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
      InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
      InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
      UserText1, UserText2, UserText3, UserText4
      > scans (1): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
      basePeakMz, basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor,
      agc, agcTarget, numberLockmassesFound, analyzerTemperature; not aggregated:
      IsCentroidScan, ScanType, Scan Description, Multiple Injection, Multi Inject
      Info, Scan Segment, Scan Event, Master Index, Master Scan Number, Charge State,
      Monoisotopic M/Z, Error in isotopic envelope fit, Max. Ion Time (ms), MS2
      Isolation Width, MS2 Isolation Offset, HCD Energy, HCD Energy V, === Mass
      Calibration: ===, Conversion Parameter B, Conversion Parameter C, Temperature
      Comp. (ppm), RF Comp. (ppm), Space Charge Comp. (ppm), Resolution Comp. (ppm),
      Number of Lock Masses, Lock Mass #1 (m/z), Lock Mass #2 (m/z), Lock Mass #3
      (m/z), LM Search Window (ppm), LM Search Window (mmu), Last Locking (sec), LM
      m/z-Correction (ppm), === Ion Optics Settings: ===, S-Lens RF Level, ====
      Diagnostic Data: ====, Application Mode, Mild Trapping Mode, APD, Res. Dep.
      Intens, Q Trans Comp, PrOSA NumF, PrOSA Comp, PrOSA ScScr, Dynamic RT Shift
      (min), Analytical OT usage (%), LC FWHM parameter, PS Inj. Time (ms), AGC PS
      Mode, AGC PS Diag, AGC Target Adjust, AGC Diag 1, AGC Diag 2, HCD abs. Offset,
      Source CID eV, AGC Fill, Injection t0, t0 FLP, Iso Para R, Inj Para R, Access
      Id, Analog In A (V), Analog In B (V), FAIMS Attached, FAIMS Voltage On, FAIMS
      CV
      > peaks (30): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, centroiderFlags
      > spectra (0): uidx, scan.no, mz, intensity
      > status_log (3): uidx, log.no, time.min, ambientTemperature; not aggregated:
      ===== Temperatures: =====, Orbitrap block temp. (°C), ICB: UHV pres. (mbar)
      > problems: has no issues

---

    Code
      print(identity(y2))
    Message
      -------- aggregated data from 1 raw file - retrieve with orbi_get_data() -------
      > file_info (1): uidx, filepath, filename, creation_date, in_aquisition,
      Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
      LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
      InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
      HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
      SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
      InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
      InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
      UserText1, UserText2, UserText3, UserText4
      > scans (1): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
      basePeakMz, basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor,
      agc, agcTarget, numberLockmassesFound, analyzerTemperature; not aggregated: 65
      columns > to show all use: print(x, show_all = TRUE)
      > peaks (30): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, centroiderFlags
      > spectra (0): uidx, scan.no, mz, intensity
      > status_log (3): uidx, log.no, time.min, ambientTemperature; not aggregated: 3
      columns > to show all use: print(x, show_all = TRUE)
      > problems: has no issues

---

    Code
      y2
    Message
      -------- aggregated data from 1 raw file - retrieve with orbi_get_data() -------
      > file_info (1): uidx, filepath, filename, creation_date, in_aquisition,
      Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
      LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
      InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
      HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
      SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
      InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
      InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
      UserText1, UserText2, UserText3, UserText4
      > scans (1): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
      basePeakMz, basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor,
      agc, agcTarget, numberLockmassesFound, analyzerTemperature; not aggregated: 65
      columns > to show all use: print(agg_data_in_global_env, show_all = TRUE)
      > peaks (30): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, centroiderFlags
      > spectra (0): uidx, scan.no, mz, intensity
      > status_log (3): uidx, log.no, time.min, ambientTemperature; not aggregated: 3
      columns > to show all use: print(agg_data_in_global_env, show_all = TRUE)
      > problems: has no issues

# orbi_read_raw() and orbi_aggregate_raw() with a status log / cli [fancy]

    Code
      print(x)
    Message
      ──────────────── [1m1 raw file - process with orbi_aggregate_raw()[22m ────────────────
      1. [34mnitrate_test_1scan.raw[39m has 1 [32mscans[39m with 30 [32mpeaks[39m and 3 [32mstatus log[39m entries;
      no [32mspectra[39m were loaded

---

    Code
      print(y)
    Message
      ──────── [1maggregated data from 1 raw file - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (1): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32min_aquisition[39m,
      [32mOperator[39m, [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m,
      [32mLastSpectrum[39m, [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m,
      [32mInstrumentModel[39m, [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m,
      [32mHardwareVersion[39m, [32mRawFileVersion[39m, [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m,
      [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m, [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m,
      [32mInjectionVolume[39m, [32mDilutionFactor[39m, [32mIstdAmount[39m, [32mCalibrationLevel[39m,
      [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m, [32mProcessingMethodFile[39m, [32mUserText0[39m,
      [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (1): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mmicroscans[39m,
      [32mbasePeakMz[39m, [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m,
      [32magc[39m, [32magcTarget[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m; [3mnot aggregated[23m: [33m65[39m
      [33mcolumns[39m → to show all use: [3mprint(y, show_all = TRUE)[23m
      → [34mpeaks[39m (30): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32mcentroiderFlags[39m
      → [34mspectra[39m (0): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mstatus_log[39m (0): [3mnothing aggregated[23m; [3mnot aggregated[23m: [33m6 columns[39m → to show all
      use: [3mprint(y, show_all = TRUE)[23m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      print(y2)
    Message
      ──────── [1maggregated data from 1 raw file - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (1): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32min_aquisition[39m,
      [32mOperator[39m, [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m,
      [32mLastSpectrum[39m, [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m,
      [32mInstrumentModel[39m, [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m,
      [32mHardwareVersion[39m, [32mRawFileVersion[39m, [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m,
      [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m, [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m,
      [32mInjectionVolume[39m, [32mDilutionFactor[39m, [32mIstdAmount[39m, [32mCalibrationLevel[39m,
      [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m, [32mProcessingMethodFile[39m, [32mUserText0[39m,
      [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (1): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mmicroscans[39m,
      [32mbasePeakMz[39m, [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m,
      [32magc[39m, [32magcTarget[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m; [3mnot aggregated[23m: [33m65[39m
      [33mcolumns[39m → to show all use: [3mprint(y2, show_all = TRUE)[23m
      → [34mpeaks[39m (30): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32mcentroiderFlags[39m
      → [34mspectra[39m (0): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mstatus_log[39m (3): [32muidx[39m, [32mlog.no[39m, [32mtime.min[39m, [32mambientTemperature[39m; [3mnot aggregated[23m: [33m3[39m
      [33mcolumns[39m → to show all use: [3mprint(y2, show_all = TRUE)[23m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      print(y2, show_all = TRUE)
    Message
      ──────── [1maggregated data from 1 raw file - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (1): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32min_aquisition[39m,
      [32mOperator[39m, [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m,
      [32mLastSpectrum[39m, [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m,
      [32mInstrumentModel[39m, [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m,
      [32mHardwareVersion[39m, [32mRawFileVersion[39m, [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m,
      [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m, [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m,
      [32mInjectionVolume[39m, [32mDilutionFactor[39m, [32mIstdAmount[39m, [32mCalibrationLevel[39m,
      [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m, [32mProcessingMethodFile[39m, [32mUserText0[39m,
      [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (1): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mmicroscans[39m,
      [32mbasePeakMz[39m, [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m,
      [32magc[39m, [32magcTarget[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m; [3mnot aggregated[23m:
      [3m[33mIsCentroidScan[39m[23m, [3m[33mScanType[39m[23m, [3m[33mScan Description[39m[23m, [3m[33mMultiple Injection[39m[23m, [3m[33mMulti Inject[39m[23m
      [3m[33mInfo[39m[23m, [3m[33mScan Segment[39m[23m, [3m[33mScan Event[39m[23m, [3m[33mMaster Index[39m[23m, [3m[33mMaster Scan Number[39m[23m, [3m[33mCharge State[39m[23m,
      [3m[33mMonoisotopic M/Z[39m[23m, [3m[33mError in isotopic envelope fit[39m[23m, [3m[33mMax. Ion Time (ms)[39m[23m, [3m[33mMS2[39m[23m
      [3m[33mIsolation Width[39m[23m, [3m[33mMS2 Isolation Offset[39m[23m, [3m[33mHCD Energy[39m[23m, [3m[33mHCD Energy V[39m[23m, [3m[33m=== Mass[39m[23m
      [3m[33mCalibration: ===[39m[23m, [3m[33mConversion Parameter B[39m[23m, [3m[33mConversion Parameter C[39m[23m, [3m[33mTemperature[39m[23m
      [3m[33mComp. (ppm)[39m[23m, [3m[33mRF Comp. (ppm)[39m[23m, [3m[33mSpace Charge Comp. (ppm)[39m[23m, [3m[33mResolution Comp. (ppm)[39m[23m,
      [3m[33mNumber of Lock Masses[39m[23m, [3m[33mLock Mass #1 (m/z)[39m[23m, [3m[33mLock Mass #2 (m/z)[39m[23m, [3m[33mLock Mass #3[39m[23m
      [3m[33m(m/z)[39m[23m, [3m[33mLM Search Window (ppm)[39m[23m, [3m[33mLM Search Window (mmu)[39m[23m, [3m[33mLast Locking (sec)[39m[23m, [3m[33mLM[39m[23m
      [3m[33mm/z-Correction (ppm)[39m[23m, [3m[33m=== Ion Optics Settings: ===[39m[23m, [3m[33mS-Lens RF Level[39m[23m, [3m[33m====[39m[23m
      [3m[33mDiagnostic Data: ====[39m[23m, [3m[33mApplication Mode[39m[23m, [3m[33mMild Trapping Mode[39m[23m, [3m[33mAPD[39m[23m, [3m[33mRes. Dep.[39m[23m
      [3m[33mIntens[39m[23m, [3m[33mQ Trans Comp[39m[23m, [3m[33mPrOSA NumF[39m[23m, [3m[33mPrOSA Comp[39m[23m, [3m[33mPrOSA ScScr[39m[23m, [3m[33mDynamic RT Shift[39m[23m
      [3m[33m(min)[39m[23m, [3m[33mAnalytical OT usage (%)[39m[23m, [3m[33mLC FWHM parameter[39m[23m, [3m[33mPS Inj. Time (ms)[39m[23m, [3m[33mAGC PS[39m[23m
      [3m[33mMode[39m[23m, [3m[33mAGC PS Diag[39m[23m, [3m[33mAGC Target Adjust[39m[23m, [3m[33mAGC Diag 1[39m[23m, [3m[33mAGC Diag 2[39m[23m, [3m[33mHCD abs. Offset[39m[23m,
      [3m[33mSource CID eV[39m[23m, [3m[33mAGC Fill[39m[23m, [3m[33mInjection t0[39m[23m, [3m[33mt0 FLP[39m[23m, [3m[33mIso Para R[39m[23m, [3m[33mInj Para R[39m[23m, [3m[33mAccess[39m[23m
      [3m[33mId[39m[23m, [3m[33mAnalog In A (V)[39m[23m, [3m[33mAnalog In B (V)[39m[23m, [3m[33mFAIMS Attached[39m[23m, [3m[33mFAIMS Voltage On[39m[23m, [3m[33mFAIMS[39m[23m
      [3m[33mCV[39m[23m
      → [34mpeaks[39m (30): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32mcentroiderFlags[39m
      → [34mspectra[39m (0): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mstatus_log[39m (3): [32muidx[39m, [32mlog.no[39m, [32mtime.min[39m, [32mambientTemperature[39m; [3mnot aggregated[23m:
      [3m[33m===== Temperatures: =====[39m[23m, [3m[33mOrbitrap block temp. (°C)[39m[23m, [3m[33mICB: UHV pres. (mbar)[39m[23m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      print(identity(y2))
    Message
      ──────── [1maggregated data from 1 raw file - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (1): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32min_aquisition[39m,
      [32mOperator[39m, [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m,
      [32mLastSpectrum[39m, [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m,
      [32mInstrumentModel[39m, [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m,
      [32mHardwareVersion[39m, [32mRawFileVersion[39m, [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m,
      [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m, [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m,
      [32mInjectionVolume[39m, [32mDilutionFactor[39m, [32mIstdAmount[39m, [32mCalibrationLevel[39m,
      [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m, [32mProcessingMethodFile[39m, [32mUserText0[39m,
      [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (1): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mmicroscans[39m,
      [32mbasePeakMz[39m, [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m,
      [32magc[39m, [32magcTarget[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m; [3mnot aggregated[23m: [33m65[39m
      [33mcolumns[39m → to show all use: [3mprint(x, show_all = TRUE)[23m
      → [34mpeaks[39m (30): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32mcentroiderFlags[39m
      → [34mspectra[39m (0): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mstatus_log[39m (3): [32muidx[39m, [32mlog.no[39m, [32mtime.min[39m, [32mambientTemperature[39m; [3mnot aggregated[23m: [33m3[39m
      [33mcolumns[39m → to show all use: [3mprint(x, show_all = TRUE)[23m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      y2
    Message
      ──────── [1maggregated data from 1 raw file - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (1): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32min_aquisition[39m,
      [32mOperator[39m, [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m,
      [32mLastSpectrum[39m, [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m,
      [32mInstrumentModel[39m, [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m,
      [32mHardwareVersion[39m, [32mRawFileVersion[39m, [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m,
      [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m, [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m,
      [32mInjectionVolume[39m, [32mDilutionFactor[39m, [32mIstdAmount[39m, [32mCalibrationLevel[39m,
      [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m, [32mProcessingMethodFile[39m, [32mUserText0[39m,
      [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (1): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mmicroscans[39m,
      [32mbasePeakMz[39m, [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m,
      [32magc[39m, [32magcTarget[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m; [3mnot aggregated[23m: [33m65[39m
      [33mcolumns[39m → to show all use: [3mprint(agg_data_in_global_env, show_all = TRUE)[23m
      → [34mpeaks[39m (30): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32mcentroiderFlags[39m
      → [34mspectra[39m (0): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mstatus_log[39m (3): [32muidx[39m, [32mlog.no[39m, [32mtime.min[39m, [32mambientTemperature[39m; [3mnot aggregated[23m: [33m3[39m
      [33mcolumns[39m → to show all use: [3mprint(agg_data_in_global_env, show_all = TRUE)[23m
      → [34mproblems[39m: has [32mno issues[39m

