# orbi_read_raw() works

    Code
      x <- orbi_read_raw(orbi_find_raw(system.file("extdata", package = "isoorbi")),
      cache = FALSE, read_cache = FALSE)
    Message
      v orbi_read_raw() read 'nitrate_test_10scans.raw' (124.68 kB)
      v orbi_read_raw() read 'nitrate_test_1scan.raw' (84.10 kB)
      v orbi_read_raw() finished reading 2 files
    Code
      select(as_tibble(x), -"filepath")
    Output
      # A tibble: 2 x 5
        file_info         scans              peaks              spectra  problems
        <list>            <list>             <list>             <list>   <list>  
      1 <tibble [1 x 41]> <tibble [10 x 81]> <tibble [126 x 8]> <tibble> <tibble>
      2 <tibble [1 x 41]> <tibble [1 x 81]>  <tibble [12 x 8]>  <tibble> <tibble>

# orbi_read_raw() [plain]

    Code
      x
    Message
      ---------------- 2 raw files - combine with orbi_aggregate_raw() ---------------
      1. nitrate_test_10scans.raw has 10 scans with 126 peaks; no spectra were loaded
      2. nitrate_test_1scan.raw   has  1 scans with  12 peaks; no spectra were loaded

---

    Code
      x
    Message
      ---------------- 2 raw files - combine with orbi_aggregate_raw() ---------------
      1. nitrate_test_10scans.raw has 10 scans with 126 peaks; + loaded 1 spectrum
      (350 points)
      2. nitrate_test_1scan.raw   has  1 scans with  12 peaks; + loaded 1 spectrum
      (325 points)

# orbi_read_raw() [fancy]

    Code
      x
    Message
      ──────────────── [1m2 raw files - combine with orbi_aggregate_raw()[22m ───────────────
      1. [34mnitrate_test_10scans.raw[39m has 10 [32mscans[39m with 126 [32mpeaks[39m; no [32mspectra[39m were loaded
      2. [34mnitrate_test_1scan.raw[39m   has  1 [32mscans[39m with  12 [32mpeaks[39m; no [32mspectra[39m were loaded

---

    Code
      y <- orbi_aggregate_raw(x)
    Message
      v aggregate_files() aggregated file_info (2), scans (11), peaks (138), and
      spectra (0) from 2 files using the standard aggregator
    Code
      as_list(y)
    Condition
      Warning:
      `as_list()` is deprecated as of rlang 0.4.0
      Please use `vctrs::vec_cast()` instead.
      Warning:
      `switch_type()` is soft-deprecated as of rlang 0.4.0.
      Please use `switch(typeof())` or `switch(my_typeof())` instead.
      This warning is displayed once every 8 hours.
    Output
      $file_info
      # A tibble: 2 x 43
         uidx filepath           filename creation_date       Operator FileDescription
        <int> <chr>              <chr>    <dttm>              <chr>    <chr>          
      1     1 /Users/seko0922/D~ nitrate~ 2025-01-30 06:57:12 SYSTEM   ""             
      2     2 /Users/seko0922/D~ nitrate~ 2025-01-30 07:01:04 SYSTEM   ""             
      # i 37 more variables: MassResolution <chr>, SpectraCount <chr>,
      #   FirstSpectrum <chr>, LastSpectrum <chr>, StartTime <chr>, EndTime <chr>,
      #   LowMass <chr>, HighMass <chr>, InstrumentCount <chr>,
      #   InstrumentModel <chr>, InstrumentName <chr>, SerialNumber <chr>,
      #   SoftwareVersion <chr>, HardwareVersion <chr>, RawFileVersion <chr>,
      #   InstrumentUnits <chr>, Comment <chr>, SampleId <chr>, SampleName <chr>,
      #   SampleType <chr>, SampleWeight <chr>, SampleVolume <chr>, ...
      
      $scans
      # A tibble: 11 x 17
          uidx scan.no time.min      tic it.ms resolution basePeakMz basePeakIntensity
         <int>   <int>    <dbl>    <dbl> <dbl>      <dbl>      <dbl>             <dbl>
       1     1       1  0.00454 4336653   68.3      60000       62.0          4046979 
       2     1       2  0.00675 3391426.  80.6      60000       62.0          3213242.
       3     1       3  0.00897 3665948.  79.5      60000       62.0          3406449.
       4     1       4  0.0112  5965333  100.       60000       62.0          5644106.
       5     1       5  0.0134  2595905.  94.3      60000       62.0          2455685 
       6     1       6  0.0156  4273768.  55.3      60000       62.0          4068725.
       7     1       7  0.0181  3134818. 131.       60000       62.0          2968120.
       8     1       8  0.0203  3522451.  78.7      60000       62.0          3345269 
       9     1       9  0.0225  4324210. 109.       60000       62.0          4084736.
      10     1      10  0.0247  3553078.  95.7      60000       62.0          3329268.
      11     2       1  0.00399 6382695   56.6      60000       62.0          6029180 
      # i 9 more variables: lowMass <dbl>, highMass <dbl>, rawOvFtT <dbl>,
      #   intensCompFactor <dbl>, agc <chr>, agcTarget <int>, microscans <int>,
      #   numberLockmassesFound <int>, analyzerTemperature <dbl>
      
      $peaks
      # A tibble: 138 x 9
          uidx scan.no mzMeasured intensity baseline peakNoise peakResolution
         <int>   <int>      <dbl>     <dbl>    <dbl>     <dbl>          <dbl>
       1     1       1       62.0     1211.     8.32      513.          70900
       2     1       1       62.0     1463.     8.32      513.          94100
       3     1       1       62.0     1172.     8.31      513.          80300
       4     1       1       62.0     1116.     8.30      513.          87900
       5     1       1       62.0  4046979      8.28      513.         114902
       6     1       1       62.0     1798.     8.26      513.          86300
       7     1       1       62.0     1444.     8.26      513.          93200
       8     1       1       62.0     1346.     8.25      513.          84700
       9     1       1       62.0     1469.     8.25      513.          92400
      10     1       1       62.1     1043.     8.16      513.          89900
      # i 128 more rows
      # i 2 more variables: isRefPeak <lgl>, isLockPeak <lgl>
      
      $spectra
      # A tibble: 0 x 4
      # i 4 variables: uidx <int>, scan.no <int>, mz <dbl>, intensity <dbl>
      
      $problems
      # A tibble: 0 x 5
      # i 5 variables: uidx <int>, type <chr>, call <chr>, message <chr>,
      #   condition <list>
      

---

    Code
      x
    Message
      ──────────────── [1m2 raw files - combine with orbi_aggregate_raw()[22m ───────────────
      1. [34mnitrate_test_10scans.raw[39m has 10 [32mscans[39m with 126 [32mpeaks[39m; + loaded 1 [32mspectrum[39m
      (350 points)
      2. [34mnitrate_test_1scan.raw[39m   has  1 [32mscans[39m with  12 [32mpeaks[39m; + loaded 1 [32mspectrum[39m
      (325 points)

# orbi_aggregate_raw() [plain]

    Code
      y
    Message
      ------- aggregated data from 2 raw files - retrieve with orbi_get_data() -------
      > file_info (2): uidx, filepath, filename, creation_date, Operator,
      FileDescription, MassResolution, SpectraCount, FirstSpectrum, LastSpectrum,
      StartTime, EndTime, LowMass, HighMass, InstrumentCount, InstrumentModel,
      InstrumentName, SerialNumber, SoftwareVersion, HardwareVersion, RawFileVersion,
      InstrumentUnits, Comment, SampleId, SampleName, SampleType, SampleWeight,
      SampleVolume, Barcode, RowNumber, Vial, InjectionVolume, DilutionFactor,
      IstdAmount, CalibrationLevel, InstrumentMethodFile, CalibrationFile,
      ProcessingMethodFile, UserText0, UserText1, UserText2, UserText3, UserText4
      > scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, basePeakMz,
      basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor, agc,
      agcTarget, microscans, numberLockmassesFound, analyzerTemperature; (not
      aggregated: IsCentroidScan, ScanType, Scan Description, Multiple Injection,
      Multi Inject Info, Scan Segment, Scan Event, Master Index, Master Scan Number,
      Charge State, Monoisotopic M/Z, Error in isotopic envelope fit, Max. Ion Time
      (ms), MS2 Isolation Width, MS2 Isolation Offset, HCD Energy, HCD Energy V, ===
      Mass Calibration: ===, Conversion Parameter B, Conversion Parameter C,
      Temperature Comp. (ppm), RF Comp. (ppm), Space Charge Comp. (ppm), Resolution
      Comp. (ppm), Number of Lock Masses, Lock Mass #1 (m/z), Lock Mass #2 (m/z),
      Lock Mass #3 (m/z), LM Search Window (ppm), LM Search Window (mmu), Last
      Locking (sec), LM m/z-Correction (ppm), === Ion Optics Settings: ===, S-Lens RF
      Level, ==== Diagnostic Data: ====, Application Mode, Mild Trapping Mode, APD,
      Res. Dep. Intens, Q Trans Comp, PrOSA NumF, PrOSA Comp, PrOSA ScScr, Dynamic RT
      Shift (min), Analytical OT usage (%), LC FWHM parameter, PS Inj. Time (ms), AGC
      PS Mode, AGC PS Diag, AGC Target Adjust, AGC Diag 1, AGC Diag 2, HCD abs.
      Offset, Source CID eV, AGC Fill, Injection t0, t0 FLP, Iso Para R, Inj Para R,
      Access Id, Analog In A (V), Analog In B (V), FAIMS Attached, FAIMS Voltage On,
      FAIMS CV)
      > peaks (138): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, isRefPeak, isLockPeak
      > spectra (0): uidx, scan.no, mz, intensity
      > problems: has no issues

---

    Code
      y
    Message
      ------- aggregated data from 2 raw files - retrieve with orbi_get_data() -------
      > file_info (2): uidx, filepath, filename, creation_date, Operator,
      FileDescription, MassResolution, SpectraCount, FirstSpectrum, LastSpectrum,
      StartTime, EndTime, LowMass, HighMass, InstrumentCount, InstrumentModel,
      InstrumentName, SerialNumber, SoftwareVersion, HardwareVersion, RawFileVersion,
      InstrumentUnits, Comment, SampleId, SampleName, SampleType, SampleWeight,
      SampleVolume, Barcode, RowNumber, Vial, InjectionVolume, DilutionFactor,
      IstdAmount, CalibrationLevel, InstrumentMethodFile, CalibrationFile,
      ProcessingMethodFile, UserText0, UserText1, UserText2, UserText3, UserText4
      > scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, basePeakMz,
      basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor, agc,
      agcTarget, microscans, numberLockmassesFound, analyzerTemperature,
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
      > peaks (138): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, isRefPeak, isLockPeak
      > spectra (675): uidx, scan.no, mz, intensity
      > problems: has no issues

---

    Code
      y
    Message
      ------- aggregated data from 2 raw files - retrieve with orbi_get_data() -------
      > file_info (2): uidx, filepath, filename, creation_date; (not aggregated:
      Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
      LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
      InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
      HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
      SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
      InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
      InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
      UserText1, UserText2, UserText3, UserText4)
      > scans (11): uidx, scan.no, time.min, tic, it.ms, resolution; (not aggregated:
      BasePeakIntensity, BasePeakMass, HighMass, IsCentroidScan, LowMass, ScanType,
      Scan Description, Multiple Injection, Multi Inject Info, AGC, Micro Scan Count,
      Scan Segment, Scan Event, Master Index, Master Scan Number, Charge State,
      Monoisotopic M/Z, Error in isotopic envelope fit, Max. Ion Time (ms), MS2
      Isolation Width, MS2 Isolation Offset, AGC Target, HCD Energy, HCD Energy V,
      Analyzer Temperature, === Mass Calibration: ===, Conversion Parameter B,
      Conversion Parameter C, Temperature Comp. (ppm), RF Comp. (ppm), Space Charge
      Comp. (ppm), Resolution Comp. (ppm), Number of Lock Masses, Lock Mass #1 (m/z),
      Lock Mass #2 (m/z), Lock Mass #3 (m/z), LM Search Window (ppm), LM Search
      Window (mmu), Number of LM Found, Last Locking (sec), LM m/z-Correction (ppm),
      === Ion Optics Settings: ===, S-Lens RF Level, ==== Diagnostic Data: ====,
      Application Mode, Mild Trapping Mode, APD, OT Intens Comp Factor, Res. Dep.
      Intens, Q Trans Comp, PrOSA NumF, PrOSA Comp, PrOSA ScScr, RawOvFtT, Dynamic RT
      Shift (min), Analytical OT usage (%), LC FWHM parameter, PS Inj. Time (ms), AGC
      PS Mode, AGC PS Diag, AGC Target Adjust, AGC Diag 1, AGC Diag 2, HCD abs.
      Offset, Source CID eV, AGC Fill, Injection t0, t0 FLP, Iso Para R, Inj Para R,
      Access Id, Analog In A (V), Analog In B (V), FAIMS Attached, FAIMS Voltage On,
      FAIMS CV)
      > peaks (138): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
      peakResolution, isRefPeak, isLockPeak
      > spectra (675): uidx, scan.no, mz, intensity
      > problems: has no issues

# orbi_aggregate_raw() [fancy]

    Code
      y
    Message
      ─────── [1maggregated data from 2 raw files - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (2): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32mOperator[39m,
      [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m, [32mLastSpectrum[39m,
      [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m, [32mInstrumentModel[39m,
      [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m, [32mHardwareVersion[39m, [32mRawFileVersion[39m,
      [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m, [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m,
      [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m, [32mInjectionVolume[39m, [32mDilutionFactor[39m,
      [32mIstdAmount[39m, [32mCalibrationLevel[39m, [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m,
      [32mProcessingMethodFile[39m, [32mUserText0[39m, [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (11): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mbasePeakMz[39m,
      [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m, [32magc[39m,
      [32magcTarget[39m, [32mmicroscans[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m; ([3mnot[23m
      [3maggregated[23m: [3m[33mIsCentroidScan[39m[23m, [3m[33mScanType[39m[23m, [3m[33mScan Description[39m[23m, [3m[33mMultiple Injection[39m[23m,
      [3m[33mMulti Inject Info[39m[23m, [3m[33mScan Segment[39m[23m, [3m[33mScan Event[39m[23m, [3m[33mMaster Index[39m[23m, [3m[33mMaster Scan Number[39m[23m,
      [3m[33mCharge State[39m[23m, [3m[33mMonoisotopic M/Z[39m[23m, [3m[33mError in isotopic envelope fit[39m[23m, [3m[33mMax. Ion Time[39m[23m
      [3m[33m(ms)[39m[23m, [3m[33mMS2 Isolation Width[39m[23m, [3m[33mMS2 Isolation Offset[39m[23m, [3m[33mHCD Energy[39m[23m, [3m[33mHCD Energy V[39m[23m, [3m[33m===[39m[23m
      [3m[33mMass Calibration: ===[39m[23m, [3m[33mConversion Parameter B[39m[23m, [3m[33mConversion Parameter C[39m[23m,
      [3m[33mTemperature Comp. (ppm)[39m[23m, [3m[33mRF Comp. (ppm)[39m[23m, [3m[33mSpace Charge Comp. (ppm)[39m[23m, [3m[33mResolution[39m[23m
      [3m[33mComp. (ppm)[39m[23m, [3m[33mNumber of Lock Masses[39m[23m, [3m[33mLock Mass #1 (m/z)[39m[23m, [3m[33mLock Mass #2 (m/z)[39m[23m,
      [3m[33mLock Mass #3 (m/z)[39m[23m, [3m[33mLM Search Window (ppm)[39m[23m, [3m[33mLM Search Window (mmu)[39m[23m, [3m[33mLast[39m[23m
      [3m[33mLocking (sec)[39m[23m, [3m[33mLM m/z-Correction (ppm)[39m[23m, [3m[33m=== Ion Optics Settings: ===[39m[23m, [3m[33mS-Lens RF[39m[23m
      [3m[33mLevel[39m[23m, [3m[33m==== Diagnostic Data: ====[39m[23m, [3m[33mApplication Mode[39m[23m, [3m[33mMild Trapping Mode[39m[23m, [3m[33mAPD[39m[23m,
      [3m[33mRes. Dep. Intens[39m[23m, [3m[33mQ Trans Comp[39m[23m, [3m[33mPrOSA NumF[39m[23m, [3m[33mPrOSA Comp[39m[23m, [3m[33mPrOSA ScScr[39m[23m, [3m[33mDynamic RT[39m[23m
      [3m[33mShift (min)[39m[23m, [3m[33mAnalytical OT usage (%)[39m[23m, [3m[33mLC FWHM parameter[39m[23m, [3m[33mPS Inj. Time (ms)[39m[23m, [3m[33mAGC[39m[23m
      [3m[33mPS Mode[39m[23m, [3m[33mAGC PS Diag[39m[23m, [3m[33mAGC Target Adjust[39m[23m, [3m[33mAGC Diag 1[39m[23m, [3m[33mAGC Diag 2[39m[23m, [3m[33mHCD abs.[39m[23m
      [3m[33mOffset[39m[23m, [3m[33mSource CID eV[39m[23m, [3m[33mAGC Fill[39m[23m, [3m[33mInjection t0[39m[23m, [3m[33mt0 FLP[39m[23m, [3m[33mIso Para R[39m[23m, [3m[33mInj Para R[39m[23m,
      [3m[33mAccess Id[39m[23m, [3m[33mAnalog In A (V)[39m[23m, [3m[33mAnalog In B (V)[39m[23m, [3m[33mFAIMS Attached[39m[23m, [3m[33mFAIMS Voltage On[39m[23m,
      [3m[33mFAIMS CV[39m[23m)
      → [34mpeaks[39m (138): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32misRefPeak[39m, [32misLockPeak[39m
      → [34mspectra[39m (0): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      y
    Message
      ─────── [1maggregated data from 2 raw files - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (2): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m, [32mOperator[39m,
      [32mFileDescription[39m, [32mMassResolution[39m, [32mSpectraCount[39m, [32mFirstSpectrum[39m, [32mLastSpectrum[39m,
      [32mStartTime[39m, [32mEndTime[39m, [32mLowMass[39m, [32mHighMass[39m, [32mInstrumentCount[39m, [32mInstrumentModel[39m,
      [32mInstrumentName[39m, [32mSerialNumber[39m, [32mSoftwareVersion[39m, [32mHardwareVersion[39m, [32mRawFileVersion[39m,
      [32mInstrumentUnits[39m, [32mComment[39m, [32mSampleId[39m, [32mSampleName[39m, [32mSampleType[39m, [32mSampleWeight[39m,
      [32mSampleVolume[39m, [32mBarcode[39m, [32mRowNumber[39m, [32mVial[39m, [32mInjectionVolume[39m, [32mDilutionFactor[39m,
      [32mIstdAmount[39m, [32mCalibrationLevel[39m, [32mInstrumentMethodFile[39m, [32mCalibrationFile[39m,
      [32mProcessingMethodFile[39m, [32mUserText0[39m, [32mUserText1[39m, [32mUserText2[39m, [32mUserText3[39m, [32mUserText4[39m
      → [34mscans[39m (11): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m, [32mbasePeakMz[39m,
      [32mbasePeakIntensity[39m, [32mlowMass[39m, [32mhighMass[39m, [32mrawOvFtT[39m, [32mintensCompFactor[39m, [32magc[39m,
      [32magcTarget[39m, [32mmicroscans[39m, [32mnumberLockmassesFound[39m, [32manalyzerTemperature[39m,
      [32mIsCentroidScan[39m, [32mScanType[39m, [32mScan Description[39m, [32mMultiple Injection[39m, [32mMulti Inject[39m
      [32mInfo[39m, [32mScan Segment[39m, [32mScan Event[39m, [32mMaster Index[39m, [32mMaster Scan Number[39m, [32mCharge State[39m,
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
      → [34mpeaks[39m (138): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32misRefPeak[39m, [32misLockPeak[39m
      → [34mspectra[39m (675): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mproblems[39m: has [32mno issues[39m

---

    Code
      y
    Message
      ─────── [1maggregated data from 2 raw files - retrieve with orbi_get_data()[22m ───────
      → [34mfile_info[39m (2): [32muidx[39m, [32mfilepath[39m, [32mfilename[39m, [32mcreation_date[39m; ([3mnot aggregated[23m:
      [3m[33mOperator[39m[23m, [3m[33mFileDescription[39m[23m, [3m[33mMassResolution[39m[23m, [3m[33mSpectraCount[39m[23m, [3m[33mFirstSpectrum[39m[23m,
      [3m[33mLastSpectrum[39m[23m, [3m[33mStartTime[39m[23m, [3m[33mEndTime[39m[23m, [3m[33mLowMass[39m[23m, [3m[33mHighMass[39m[23m, [3m[33mInstrumentCount[39m[23m,
      [3m[33mInstrumentModel[39m[23m, [3m[33mInstrumentName[39m[23m, [3m[33mSerialNumber[39m[23m, [3m[33mSoftwareVersion[39m[23m,
      [3m[33mHardwareVersion[39m[23m, [3m[33mRawFileVersion[39m[23m, [3m[33mInstrumentUnits[39m[23m, [3m[33mComment[39m[23m, [3m[33mSampleId[39m[23m,
      [3m[33mSampleName[39m[23m, [3m[33mSampleType[39m[23m, [3m[33mSampleWeight[39m[23m, [3m[33mSampleVolume[39m[23m, [3m[33mBarcode[39m[23m, [3m[33mRowNumber[39m[23m, [3m[33mVial[39m[23m,
      [3m[33mInjectionVolume[39m[23m, [3m[33mDilutionFactor[39m[23m, [3m[33mIstdAmount[39m[23m, [3m[33mCalibrationLevel[39m[23m,
      [3m[33mInstrumentMethodFile[39m[23m, [3m[33mCalibrationFile[39m[23m, [3m[33mProcessingMethodFile[39m[23m, [3m[33mUserText0[39m[23m,
      [3m[33mUserText1[39m[23m, [3m[33mUserText2[39m[23m, [3m[33mUserText3[39m[23m, [3m[33mUserText4[39m[23m)
      → [34mscans[39m (11): [32muidx[39m, [32mscan.no[39m, [32mtime.min[39m, [32mtic[39m, [32mit.ms[39m, [32mresolution[39m; ([3mnot aggregated[23m:
      [3m[33mBasePeakIntensity[39m[23m, [3m[33mBasePeakMass[39m[23m, [3m[33mHighMass[39m[23m, [3m[33mIsCentroidScan[39m[23m, [3m[33mLowMass[39m[23m, [3m[33mScanType[39m[23m,
      [3m[33mScan Description[39m[23m, [3m[33mMultiple Injection[39m[23m, [3m[33mMulti Inject Info[39m[23m, [3m[33mAGC[39m[23m, [3m[33mMicro Scan Count[39m[23m,
      [3m[33mScan Segment[39m[23m, [3m[33mScan Event[39m[23m, [3m[33mMaster Index[39m[23m, [3m[33mMaster Scan Number[39m[23m, [3m[33mCharge State[39m[23m,
      [3m[33mMonoisotopic M/Z[39m[23m, [3m[33mError in isotopic envelope fit[39m[23m, [3m[33mMax. Ion Time (ms)[39m[23m, [3m[33mMS2[39m[23m
      [3m[33mIsolation Width[39m[23m, [3m[33mMS2 Isolation Offset[39m[23m, [3m[33mAGC Target[39m[23m, [3m[33mHCD Energy[39m[23m, [3m[33mHCD Energy V[39m[23m,
      [3m[33mAnalyzer Temperature[39m[23m, [3m[33m=== Mass Calibration: ===[39m[23m, [3m[33mConversion Parameter B[39m[23m,
      [3m[33mConversion Parameter C[39m[23m, [3m[33mTemperature Comp. (ppm)[39m[23m, [3m[33mRF Comp. (ppm)[39m[23m, [3m[33mSpace Charge[39m[23m
      [3m[33mComp. (ppm)[39m[23m, [3m[33mResolution Comp. (ppm)[39m[23m, [3m[33mNumber of Lock Masses[39m[23m, [3m[33mLock Mass #1 (m/z)[39m[23m,
      [3m[33mLock Mass #2 (m/z)[39m[23m, [3m[33mLock Mass #3 (m/z)[39m[23m, [3m[33mLM Search Window (ppm)[39m[23m, [3m[33mLM Search[39m[23m
      [3m[33mWindow (mmu)[39m[23m, [3m[33mNumber of LM Found[39m[23m, [3m[33mLast Locking (sec)[39m[23m, [3m[33mLM m/z-Correction (ppm)[39m[23m,
      [3m[33m=== Ion Optics Settings: ===[39m[23m, [3m[33mS-Lens RF Level[39m[23m, [3m[33m==== Diagnostic Data: ====[39m[23m,
      [3m[33mApplication Mode[39m[23m, [3m[33mMild Trapping Mode[39m[23m, [3m[33mAPD[39m[23m, [3m[33mOT Intens Comp Factor[39m[23m, [3m[33mRes. Dep.[39m[23m
      [3m[33mIntens[39m[23m, [3m[33mQ Trans Comp[39m[23m, [3m[33mPrOSA NumF[39m[23m, [3m[33mPrOSA Comp[39m[23m, [3m[33mPrOSA ScScr[39m[23m, [3m[33mRawOvFtT[39m[23m, [3m[33mDynamic RT[39m[23m
      [3m[33mShift (min)[39m[23m, [3m[33mAnalytical OT usage (%)[39m[23m, [3m[33mLC FWHM parameter[39m[23m, [3m[33mPS Inj. Time (ms)[39m[23m, [3m[33mAGC[39m[23m
      [3m[33mPS Mode[39m[23m, [3m[33mAGC PS Diag[39m[23m, [3m[33mAGC Target Adjust[39m[23m, [3m[33mAGC Diag 1[39m[23m, [3m[33mAGC Diag 2[39m[23m, [3m[33mHCD abs.[39m[23m
      [3m[33mOffset[39m[23m, [3m[33mSource CID eV[39m[23m, [3m[33mAGC Fill[39m[23m, [3m[33mInjection t0[39m[23m, [3m[33mt0 FLP[39m[23m, [3m[33mIso Para R[39m[23m, [3m[33mInj Para R[39m[23m,
      [3m[33mAccess Id[39m[23m, [3m[33mAnalog In A (V)[39m[23m, [3m[33mAnalog In B (V)[39m[23m, [3m[33mFAIMS Attached[39m[23m, [3m[33mFAIMS Voltage On[39m[23m,
      [3m[33mFAIMS CV[39m[23m)
      → [34mpeaks[39m (138): [32muidx[39m, [32mscan.no[39m, [32mmzMeasured[39m, [32mintensity[39m, [32mbaseline[39m, [32mpeakNoise[39m,
      [32mpeakResolution[39m, [32misRefPeak[39m, [32misLockPeak[39m
      → [34mspectra[39m (675): [32muidx[39m, [32mscan.no[39m, [32mmz[39m, [32mintensity[39m
      → [34mproblems[39m: has [32mno issues[39m

# orbi_get_data() [plain]

    Code
      out <- orbi_get_data(y, scans = everything(), spectra = everything())
    Message
      v orbi_get_data() retrieved 0 records from the combination of file_info (2),
      scans (11), and spectra (0) via uidx and scan.no
      ! Warning: there are no spectra in the data, make sure to include them when
      reading the raw files e.g. with orbi_read_raw(include_spectra = c(1, 10, 100))

---

    Code
      out <- orbi_get_data(y, scans = everything(), spectra = everything())
    Message
      v orbi_get_data() retrieved 0 records from the combination of file_info (2),
      scans (11), and spectra (0) via uidx and scan.no
      ! Warning: there are no spectra in the data, make sure to include them when
      reading the raw files e.g. with orbi_read_raw(include_spectra = c(1, 10, 100))

# orbi_get_data() [fancy]

    Code
      out <- orbi_get_data(y, scans = everything(), spectra = everything())
    Message
      [32m✔[39m [1morbi_get_data()[22m retrieved 0 records from the combination of [34mfile_info[39m (2),
      [34mscans[39m (11), and [34mspectra[39m (0) via [32muidx[39m and [32mscan.no[39m
      [33m![39m [1mWarning[22m: there are no [32mspectra[39m in the data, make sure to include them when
      reading the raw files e.g. with [1morbi_read_raw(include_spectra = c(1, 10, 100))[22m

---

    Code
      x <- orbi_read_raw(orbi_find_raw(system.file("extdata", package = "isoorbi")),
      cache = FALSE, read_cache = FALSE, include_spectra = 1)
    Message
      v orbi_read_raw() read 'nitrate_test_10scans.raw' (124.68 kB), included the
      spectrum from 1 scan
      v orbi_read_raw() read 'nitrate_test_1scan.raw' (84.10 kB), included the
      spectrum from 1 scan
      v orbi_read_raw() finished reading 2 files
    Code
      select(as_tibble(x), -"filepath")
    Output
      # A tibble: 2 x 5
        file_info         scans              peaks              spectra  problems
        <list>            <list>             <list>             <list>   <list>  
      1 <tibble [1 x 41]> <tibble [10 x 81]> <tibble [126 x 8]> <tibble> <tibble>
      2 <tibble [1 x 41]> <tibble [1 x 81]>  <tibble [12 x 8]>  <tibble> <tibble>

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
      orbi_get_data(y, scans = everything(), spectra = everything())
    Message
      v orbi_get_data() retrieved 0 records from the combination of file_info (2),
      scans (11), and spectra (0) via uidx and scan.no
      ! Warning: there are no spectra in the data, make sure to include them when
      reading the raw files e.g. with orbi_read_raw(include_spectra = c(1, 10, 100))
    Output
      # A tibble: 0 x 20
      # i 20 variables: uidx <int>, filename <chr>, scan.no <int>, time.min <dbl>,
      #   tic <dbl>, it.ms <dbl>, resolution <dbl>, basePeakMz <dbl>,
      #   basePeakIntensity <dbl>, lowMass <dbl>, highMass <dbl>, rawOvFtT <dbl>,
      #   intensCompFactor <dbl>, agc <chr>, agcTarget <int>, microscans <int>,
      #   numberLockmassesFound <int>, analyzerTemperature <dbl>, mz <dbl>,
      #   intensity <dbl>

