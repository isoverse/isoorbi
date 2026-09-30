# Functionality Guide

> This step-by-step functionality guide is still in development.
> Eventually all functions in the [package structure
> flowchart](https://isoorbi.isoverse.org/index.html#package-structure)
> will be covered with detailed examples. All functions below labelled
> with an `*` are required steps of the standard data processing flow.
> Everything else is optional. Rarely used additional features that are
> mentioned here but not part of the standard flowchart are labeled as
> `bonus`.

``` r

# libraries
library(isoorbi) #load isoorbi R package
library(dplyr) # for mutating data frames
library(ggplot2) # for data visualization
```

## Reading raw files

First step is reading in your .raw data files.

### `orbi_find_raw()`

``` r

# path to your data folder
data_folder <- file.path("data")

# finding raw files with "nitrate" in the name in the data folder
file_paths <- data_folder |> orbi_find_raw(pattern = "nitrate")

# show what was found
file_paths
```

    [1] "data/nitrate_test_10scans.raw.cache.zip"
    [2] "data/nitrate_test_1scan.raw.cache.zip"  

### `orbi_read_raw()` \*

``` r

# read files (simplest)
raw_files <- file_paths |> orbi_read_raw()
```

``` fansi
✔ [204ms] orbi_read_raw() read nitrate_test_10scans.raw from cache
```

``` fansi
✔ [52ms] orbi_read_raw() read nitrate_test_1scan.raw from cache
```

``` fansi
✔ [336ms] orbi_read_raw() finished reading 2 files
```

``` r

# read files including some raw spectra
raw_files <-
  file_paths |>
  # load the spectra from scans 1, 10, and 100
  orbi_read_raw(include_spectra = c(1, 10, 100)) |>
  # you can quiet any call with suppressMessages
  # that way it won't print the info message
  suppressMessages()

# show summary for the files that were read
raw_files
```

``` fansi
──────────────── 2 raw files - combine with orbi_aggregate_raw() ───────────────
```

``` fansi
1. nitrate_test_10scans.raw has 10 scans with 307 peaks and 1 status log entry;
+ loaded 2 spectra (618 points)
2. nitrate_test_1scan.raw   has  1 scans with  30 peaks and 1 status log entry;
+ loaded 1 spectrum (325 points)
```

### `orbi_aggregate_data()` \*

Combine (aggregate) the data from the raw files.

``` r

# aggregate raw data
agg_data <- raw_files |> orbi_aggregate_raw()
```

``` fansi
✔ [422ms] orbi_aggregate_raw() aggregated file_info (2), scans (11), peaks
(337), spectra (943), and status_log (0) from 2 files using the standard
aggregator
```

``` r

# show all that was recovered
agg_data
```

``` fansi
─────── aggregated data from 2 raw files - retrieve with orbi_get_data() ───────
```

``` fansi
→ file_info (2): uidx, filepath, filename, creation_date, in_aquisition,
Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
UserText1, UserText2, UserText3, UserText4
```

``` fansi
→ scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
basePeakMz, basePeakIntensity, lowMass, highMass, rawOvFtT, intensCompFactor,
agc, agcTarget, numberLockmassesFound, analyzerTemperature; not aggregated: 65
columns → to show all use: print(agg_data, show_all = TRUE)
```

``` fansi
→ peaks (337): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
peakResolution, centroiderFlags
```

``` fansi
→ spectra (943): uidx, scan.no, mz, intensity
```

``` fansi
→ status_log (0): nothing aggregated; not aggregated: 198 columns → to show all
use: print(agg_data, show_all = TRUE)
```

``` fansi
→ problems: has no issues
```

``` r

# also show all that was ignored/not aggregated
agg_data |> print(show_all = TRUE)
```

``` fansi
─────── aggregated data from 2 raw files - retrieve with orbi_get_data() ───────
```

``` fansi
→ file_info (2): uidx, filepath, filename, creation_date, in_aquisition,
Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
UserText1, UserText2, UserText3, UserText4
```

``` fansi
→ scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
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
```

``` fansi
→ peaks (337): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
peakResolution, centroiderFlags
```

``` fansi
→ spectra (943): uidx, scan.no, mz, intensity
```

``` fansi
→ status_log (0): nothing aggregated; not aggregated: log.no, StartTime, ====
FAIMS Device: ====, ==== Overall Status: ====, Status, Performance, ====== Ion
Source: ======, Spray Voltage (V), Spray Current (µA), Spray Current std. dev.
(µA), Ion Transfer Tube Temperature (°C), Sheath gas pressure (psi), Aux gas
pressure (psi), Sweep gas pressure (psi), Vaporizer Temperature (°C), ======
Ion Optics: ======, CTB: C-trap RF freq. (MHz), CTB: C-trap RF amp. (MHz), CTB:
Quad. exit lens (V), CTB: Beam split lens (V), CTB: Tr. multipole DC (V), CTB:
C-trap entr. lens (V), CTB: C-trap exit lens (V), CTB: Z lens (V), CTB: HCD ax.
field entr. (V), CTB: HCD ax. field exit (V), CTB: HCD exit lens (V), QS: Quad.
RF amp. (Vpp), QS: Quad. MSeg rod A DC (V), QS: Quad. MSeg rod B DC (V), SB:
Spray HV voltage (V), SB: Spray HV current (uA), IOB: Source DC (V), IOB:
S-Lens RF amp. (Vpp), IOB: S-Lens RF freq. (kHz), IOB: Inj. fl. phase A DC (V),
IOB: Inj. fl. phase B DC (V), IOB: Inj. fl. RF amp. (Vpp), IOB: Inj. fl. RF
freq. (kHz), IOB: Inj. fl. RF curr. (A), IOB: Fl. focus lens (V), IOB: Bent fl.
DC (V), IOB: Bent fl. ax. f. e. (V), IOB: Bent fl. exit lens (V), IOB: Bent fl.
RF amp. (Vpp), IOB: Bent fl. RF freq. (kHz), PCBAOS: C-trap inj. o. + (V),
PCBAOS: C-trap inj. o. - (V), PCBAOS: HV focus lens (V), PCBAOS: V lens (V),
PCBAOS: Defl. measure + (V), PCBAOS: Defl. measure - (V), PCBAOS: CE inject +
(V), PCBAOS: CE inject - (V), ===== Temperatures: =====, Ambient temp. (°C),
Orbitrap block temp. (°C), Detector temp. (°C), Ion tr. tube temp. (°C),
Vaporizer temp. (°C), CPU core temp. (°C), PCB temp. top (°C), PCB temp. center
(°C), PCB temp. bottom (°C), ==== Diagnostic Data: ====, Performance ld,
Performance me, Performance cy, PrOSA counts, PrOSA blank, PrOSA time (us),
PrOSA curr (pA), ICB: Up-time (sec), ICB: +24 V, ICB: +5 V, ICB: +3.3 V, ICB:
+2.5 V, ICB: +1.2 V, ICB: Power-on time (h), ICB: Acc. bakeout time (h), ICB:
Turbopump speed (Hz), ICB: Turbopump power (W), ICB: Turbopump error state,
ICB: Turbopump last error, ICB: UHV pres. (mbar), ICB: HCD cell pres. (mbar),
ICB: Fore-vac. pres. (mbar), ICB: IF region pres. (mbar), ICB: R-f fan speed
(rpm), ICB: L-b fan speed (rpm), ICB: L-f fan speed (rpm), ICB: Dig. I/O start
0, ICB: Dig. I/O start 1, ICB: OSPI ERR queue usage, DAQ: Voltage monitor, DAQ:
Status register, CTB: FAN1 speed (rpm), CTB: FAN2 speed (rpm), CTB: ICD integr.
active, CTB: ICD signal count (ct), CTB: ICD blank count (ct), CTB: ICD ion
inj. time (us), CTB: C-trap RF curr. (mA), CTB: C-trap RF s. volt. (V), CTB: HV
positive (V), CTB: HV negative (V), QS: HV negative (V), QS: HV positive (V),
SB: +24 V supply, SB: +15 V analog supply, SB: -8 V supply, SB: +5 V analog
supply, SB: +5 V digital supply, SB: -5 V analog supply, SB: +3.3 V digital
supply, SB: +2.5 V digital supply, SB: +1.2 V digital supply, SB: Ion tr. tube
fuse, SB: Ion tr. tube temp. OOR, SB: Ion tr. tube voltage (V), SB: Vaporizer
fuse, SB: Vaporizer temp. OOR, SB: 8 kV const. volt./c.m., SB: Valve fuse, SB:
Aux. gas pres. (psi), SB: Sheath gas pres. (psi), SB: Sweep gas pres. (psi),
SB: Interlock fuse, SB: Switch 1 closed, SB: Relay 1 closed, SB: Switch 2
closed, SB: Relay 2 closed, SB: Interlock switch, SB: Cover is taken off, SB:
ICS/ETD available, IOB: HV fuse, IOB: RF fuse, IOB: HVV positive (V), IOB: HVV
negative (V), IOB: HV positive (V), IOB: HV negative (V), FTPCU: CPU load
average, FTPCU: Board free RAM, PCBAOS: HV monitor (kV), PCBAOS: R_Pulser_CE
(V), PCBAOS: R_Pulser_DE (V), ==== FAIMS Device: ==== (2), Attached, Dispersion
Voltage (DV), Compensation Voltage (CV), Entrance Plate Voltage (V), Total
Carrier Gas Flow (lpm), Cooling Gas Flow (lpm), Gas Pressure (psi), Cooling Gas
Pressure (psi), Input Gas Pressure (psi), Inner Electrode Temp. (°C), Outer
Electrode 1 Temp. (°C), Outer Electrode 2 Temp. (°C), MCB Ambient Temp. (°C),
MCB Atmosph. Pressure (kPa), DV High Frequency Amp. (V), DV Low Frequency Amp.
(V), DV Phase (°), DV Frequency (Hz), System Voltage (V), System Current (A),
Relays (+24V), +6.25D (V), -6.25D (V), +5A (V), -5A (V), +3.0D (V), +1.8A (V),
+3.3D (V), MCB PCB Temp. (°C), RTD Reference (+3V), System Relay Voltage (V),
MCB Fan 1 Speed (Hz), MCB Fan 2 Speed (Hz), TXB Fan Speed (Hz), Outer Elec.
Bias Volt. (V), HF RF Amp Output Current (A), HF RF Supply Voltage (V), HF RF
Supply Current (A), LF RF Amp Output Current (A), LF RF Supply Voltage (V), LF
RF Supply Current (A), HF RF Amp Output Voltage (V), LF RF Amp Output Voltage
(V), TX PCB temperature (°C), DV High Frequency RF DAC (V), DV Low Frequency RF
DAC (V), MS Interlock 1 (V), MS Interlock 2 (V), Gate Drive Supply (V), ==
Collaborator Interface =, Custom Interface
```

``` fansi
→ problems: has no issues
```

#### bonus `orbi_get_aggregator()`

You can optionally use a different aggregator. The `minimal` aggregator
contains a smaller set of columns to aggregate. The `extended`
aggregator is more elaborate, providing access to additional columns
from the raw data files.

``` r

# example: minimal vs. extended aggregator
orbi_get_aggregator("minimal")
```

``` fansi
────────────────────────────── Aggregator minimal ──────────────────────────────
```

``` fansi
Dataset file_info:
 → filename = as.character(sub(FileName, pattern = "\\.raw$", replacement = "",
ignore.case = TRUE))
 → creation_date = as.POSIXct(CreationDate)
 → in_aquisition = as.logical(InAquisition)
Dataset scans:
 → scan.no = as.integer(scan.no)
 → time.min = as.numeric(StartTime)
 → tic = as.numeric(TIC)
 → it.ms = as.numeric(`Ion Injection Time (ms)`)
 → resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
 → microscans = as.integer(`Micro Scan Count`)
Dataset peaks:
 → scan.no = as.integer(scan.no)
 → mzMeasured = as.numeric(mass)
 → intensity = as.numeric(intensity)
 → baseline = as.numeric(baseline)
 → peakNoise = as.numeric(noise)
 → peakResolution = as.numeric(resolution)
 → centroiderFlags = as.factor(orbi_peak_flags_to_text(flags))
Dataset spectra:
 → scan.no = as.integer(scan.no)
 → mz = as.numeric(mass)
 → intensity = as.numeric(intensity)
```

``` r

orbi_get_aggregator("extended")
```

``` fansi
────────────────────────────── Aggregator extended ─────────────────────────────
```

``` fansi
Dataset file_info:
 → filename = as.character(sub(FileName, pattern = "\\.raw$", replacement = "",
ignore.case = TRUE))
 → creation_date = as.POSIXct(CreationDate)
 → in_aquisition = as.logical(InAquisition)
 → (.*) = as.character(all_matches("(.*)"))
Dataset scans:
 → scan.no = as.integer(scan.no)
 → time.min = as.numeric(StartTime)
 → tic = as.numeric(TIC)
 → it.ms = as.numeric(`Ion Injection Time (ms)`)
 → resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
 → microscans = as.integer(`Micro Scan Count`)
 → basePeakMz = as.numeric(BasePeakMass)
 → basePeakIntensity = as.numeric(BasePeakIntensity)
 → lowMass = as.numeric(LowMass)
 → highMass = as.numeric(HighMass)
 → rawOvFtT = as.numeric(RawOvFtT)
 → intensCompFactor = as.numeric(`OT Intens Comp Factor`)
 → agc = as.character(AGC)
 → agcTarget = as.integer(`AGC Target`)
 → numberLockmassesFound = as.integer(`Number of LM Found`)
 → analyzerTemperature = as.numeric(`Analyzer Temperature`)
 → (.*) = as.character(all_matches("(.*)"))
Dataset peaks:
 → scan.no = as.integer(scan.no)
 → mzMeasured = as.numeric(mass)
 → intensity = as.numeric(intensity)
 → baseline = as.numeric(baseline)
 → peakNoise = as.numeric(noise)
 → peakResolution = as.numeric(resolution)
 → centroiderFlags = as.factor(orbi_peak_flags_to_text(flags))
Dataset spectra:
 → scan.no = as.integer(scan.no)
 → mz = as.numeric(mass)
 → intensity = as.numeric(intensity)
```

``` r

# using the extended aggregator instead of the default (standard)
raw_files |> orbi_aggregate_raw(aggregator = "extended")
```

``` fansi
✔ [745ms] orbi_aggregate_raw() aggregated file_info (2), scans (11), peaks
(337), spectra (943), and status_log (0) from 2 files using the extended
aggregator
```

``` fansi
─────── aggregated data from 2 raw files - retrieve with orbi_get_data() ───────
```

``` fansi
→ file_info (2): uidx, filepath, filename, creation_date, in_aquisition,
Operator, FileDescription, MassResolution, SpectraCount, FirstSpectrum,
LastSpectrum, StartTime, EndTime, LowMass, HighMass, InstrumentCount,
InstrumentModel, InstrumentName, SerialNumber, SoftwareVersion,
HardwareVersion, RawFileVersion, InstrumentUnits, Comment, SampleId,
SampleName, SampleType, SampleWeight, SampleVolume, Barcode, RowNumber, Vial,
InjectionVolume, DilutionFactor, IstdAmount, CalibrationLevel,
InstrumentMethodFile, CalibrationFile, ProcessingMethodFile, UserText0,
UserText1, UserText2, UserText3, UserText4
```

``` fansi
→ scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
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
```

``` fansi
→ peaks (337): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
peakResolution, centroiderFlags
```

``` fansi
→ spectra (943): uidx, scan.no, mz, intensity
```

``` fansi
→ status_log (0): nothing aggregated; not aggregated: 198 columns → to show all
use: print(x, show_all = TRUE)
```

``` fansi
→ problems: has no issues
```

#### bonus `orbi_register_aggregator()`

Or even build your own aggregator with
[`orbi_start_aggregator()`](https://isoorbi.isoverse.org/reference/orbi_aggregator.md)
and/or expand an existing one with `orgi_add_to_aggregator()` and then
register it via
[`orbi_register_aggregator()`](https://isoorbi.isoverse.org/reference/orbi_aggregator.md).
This funnctionality is rarely needed and thus not part of the package
structure flowchart.

``` r

my_agg <-
  orbi_get_aggregator("minimal") |>
  # pull out the S-Lens RF Level information from the scans and store it as a number
  orbi_add_to_aggregator(
    "scans",
    "slens_rf",
    source = "S-Lens RF Level",
    cast = "as.numeric"
  ) |>
  orbi_register_aggregator(name = "test")

# show my agg summary
my_agg
```

``` fansi
────────────────────────────── Aggregator minimal ──────────────────────────────
```

``` fansi
Dataset file_info:
 → filename = as.character(sub(FileName, pattern = "\\.raw$", replacement = "",
ignore.case = TRUE))
 → creation_date = as.POSIXct(CreationDate)
 → in_aquisition = as.logical(InAquisition)
Dataset scans:
 → scan.no = as.integer(scan.no)
 → time.min = as.numeric(StartTime)
 → tic = as.numeric(TIC)
 → it.ms = as.numeric(`Ion Injection Time (ms)`)
 → resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
 → microscans = as.integer(`Micro Scan Count`)
 → slens_rf = as.numeric(`S-Lens RF Level`)
Dataset peaks:
 → scan.no = as.integer(scan.no)
 → mzMeasured = as.numeric(mass)
 → intensity = as.numeric(intensity)
 → baseline = as.numeric(baseline)
 → peakNoise = as.numeric(noise)
 → peakResolution = as.numeric(resolution)
 → centroiderFlags = as.factor(orbi_peak_flags_to_text(flags))
Dataset spectra:
 → scan.no = as.integer(scan.no)
 → mz = as.numeric(mass)
 → intensity = as.numeric(intensity)
```

``` r

# use it
raw_files |> orbi_aggregate_raw(aggregator = "test")
```

``` fansi
✔ [182ms] orbi_aggregate_raw() aggregated file_info (2), scans (11), peaks
(337), spectra (943), and status_log (0) from 2 files using the test aggregator
```

``` fansi
─────── aggregated data from 2 raw files - retrieve with orbi_get_data() ───────
```

``` fansi
→ file_info (2): uidx, filepath, filename, creation_date, in_aquisition; not
aggregated: 39 columns → to show all use: print(x, show_all = TRUE)
```

``` fansi
→ scans (11): uidx, scan.no, time.min, tic, it.ms, resolution, microscans,
slens_rf; not aggregated: 74 columns → to show all use: print(x, show_all =
TRUE)
```

``` fansi
→ peaks (337): uidx, scan.no, mzMeasured, intensity, baseline, peakNoise,
peakResolution, centroiderFlags
```

``` fansi
→ spectra (943): uidx, scan.no, mz, intensity
```

``` fansi
→ status_log (0): nothing aggregated; not aggregated: 198 columns → to show all
use: print(x, show_all = TRUE)
```

``` fansi
→ problems: has no issues
```

#### bonus instrument status log

Besides the scans, raw files hold the instrument’s **status log**:
readbacks that the instrument records independently of the scans
(typically every few seconds) such as temperatures, vacuum pressures,
voltages, and ion source settings.
[`orbi_read_raw()`](https://isoorbi.isoverse.org/reference/orbi_read_raw.md)
returns it as the `status_log` dataset. Which channels a status log
contains depends entirely on the instrument, so none of the included
aggregators take anything from it. Instead,
[`orbi_aggregate_raw()`](https://isoorbi.isoverse.org/reference/orbi_aggregate_raw.md)
reports the available status log channels as *not aggregated* so you can
pick the ones you are interested in. Printing the aggregated data only
says how many there are, use `print(agg_data, show_all = TRUE)` to list
all of them.

Here we use two example files from two different instruments (an
Orbitrap Exploris 240 and a Q Exactive Focus) to show how to aggregate a
few channels of interest.

``` r

# two example files from different instruments
status_files <-
  orbi_get_example_files(c("dual_inlet.raw", "s3744.RAW")) |>
  orbi_read_raw()
```

``` fansi
✔ [92ms] orbi_read_raw() read dual_inlet.raw from cache
```

``` fansi
✔ [75ms] orbi_read_raw() read s3744.RAW from cache
```

``` fansi
✔ [216ms] orbi_read_raw() finished reading 2 files
```

``` r

# the raw status log of the first file (one row per log entry, one column per channel)
status_files$status_log[[1]]
```

``` fansi
# A tibble: 441 × 198
   log.no StartTime `====  FAIMS Device:  ====` ====  Overall Status:  …¹ Status
    <int>     <dbl> <chr>                       <chr>                     <chr> 
 1      1   0.00693 ""                          ""                        Instr…
 2      2   0.177   ""                          ""                        Instr…
 3      3   0.347   ""                          ""                        Instr…
 4      4   0.517   ""                          ""                        Instr…
 5      5   0.687   ""                          ""                        Instr…
 6      6   0.858   ""                          ""                        Instr…
 7      7   1.03    ""                          ""                        Instr…
 8      8   1.20    ""                          ""                        Instr…
 9      9   1.37    ""                          ""                        Instr…
10     10   1.54    ""                          ""                        Instr…
# ℹ 431 more rows
# ℹ abbreviated name: ¹​`====  Overall Status:  ====`
# ℹ 193 more variables: Performance <chr>, `======  Ion Source:  ======` <chr>,
#   `Spray Voltage (V)` <chr>, `Spray Current (µA)` <chr>,
#   `Spray Current std. dev. (µA)` <chr>,
#   `Ion Transfer Tube Temperature (°C)` <chr>,
#   `Sheath gas pressure (psi)` <chr>, `Aux gas pressure (psi)` <chr>, …
```

The status log stores all channel values as text (exactly as the
instrument reports them), so the aggregator needs to `cast` the channels
you want to use to numbers. Because the two instruments call the same
channels by different names, you can provide the alternative names as
the `source` and the aggregator uses whichever one exists in each file.

``` r

status_agg <-
  orbi_get_aggregator("standard") |>
  # when each status log entry was recorded
  orbi_add_to_aggregator(
    "status_log",
    "time.min",
    source = "StartTime",
    cast = "as.numeric"
  ) |>
  # temperatures (the channel names differ between the instruments)
  orbi_add_to_aggregator(
    "status_log",
    "ambient_temp.C",
    source = c("Ambient temp. (°C)", "Ambient Temperature (°C)"),
    cast = "as.numeric"
  ) |>
  orbi_add_to_aggregator(
    "status_log",
    "analyzer_temp.C",
    source = c("Orbitrap block temp. (°C)", "Analyzer Temperature (°C)"),
    cast = "as.numeric"
  ) |>
  # ultra high vacuum pressure
  orbi_add_to_aggregator(
    "status_log",
    "uhv_pressure.mbar",
    source = c("ICB: UHV pres. (mbar)", "UHV Sensor (mbar)"),
    cast = "as.numeric"
  ) |>
  orbi_register_aggregator("standard with status logs")

# show summary
status_agg
```

``` fansi
────────────────────────────── Aggregator standard ─────────────────────────────
```

``` fansi
Dataset file_info:
 → filename = as.character(sub(FileName, pattern = "\\.raw$", replacement = "",
ignore.case = TRUE))
 → creation_date = as.POSIXct(CreationDate)
 → in_aquisition = as.logical(InAquisition)
 → (.*) = as.character(all_matches("(.*)"))
Dataset scans:
 → scan.no = as.integer(scan.no)
 → time.min = as.numeric(StartTime)
 → tic = as.numeric(TIC)
 → it.ms = as.numeric(`Ion Injection Time (ms)`)
 → resolution = as.numeric(one_of(`FT Resolution`, `Orbitrap Resolution`))
 → microscans = as.integer(`Micro Scan Count`)
 → basePeakMz = as.numeric(BasePeakMass)
 → basePeakIntensity = as.numeric(BasePeakIntensity)
 → lowMass = as.numeric(LowMass)
 → highMass = as.numeric(HighMass)
 → rawOvFtT = as.numeric(RawOvFtT)
 → intensCompFactor = as.numeric(`OT Intens Comp Factor`)
 → agc = as.character(AGC)
 → agcTarget = as.integer(`AGC Target`)
 → numberLockmassesFound = as.integer(`Number of LM Found`)
 → analyzerTemperature = as.numeric(`Analyzer Temperature`)
Dataset peaks:
 → scan.no = as.integer(scan.no)
 → mzMeasured = as.numeric(mass)
 → intensity = as.numeric(intensity)
 → baseline = as.numeric(baseline)
 → peakNoise = as.numeric(noise)
 → peakResolution = as.numeric(resolution)
 → centroiderFlags = as.factor(orbi_peak_flags_to_text(flags))
Dataset spectra:
 → scan.no = as.integer(scan.no)
 → mz = as.numeric(mass)
 → intensity = as.numeric(intensity)
Dataset status_log:
 → time.min = as.numeric(StartTime)
 → ambient_temp.C = as.numeric(one_of(`Ambient temp. (°C)`, `Ambient
Temperature (°C)`))
 → analyzer_temp.C = as.numeric(one_of(`Orbitrap block temp. (°C)`, `Analyzer
Temperature (°C)`))
 → uhv_pressure.mbar = as.numeric(one_of(`ICB: UHV pres. (mbar)`, `UHV Sensor
(mbar)`))
```

``` r

# aggregate with the status log aggregator
status_agg_data <- status_files |>
  orbi_aggregate_raw(aggregator = "standard with status logs")
```

``` fansi
✔ [482ms] orbi_aggregate_raw() aggregated file_info (2), scans (17.83k), peaks
(340.17k), spectra (0), and status_log (520) from 2 files using the standard
with status logs aggregator
```

Status log data is a great way to check on the instrument conditions
during an analysis, for example to see whether the temperatures were
stable or whether there were any vacuum issues.

``` r

# retrieve the status log channels
status_log <- status_agg_data |>
  orbi_get_data(file_info = "filename", status_log = everything())
```

``` fansi
✔ [8ms] orbi_get_data() retrieved 520 records from the combination of file_info
(2) and status_log (520) via uidx
```

``` r

# bring the channels into long format for plotting them in panels
status_log |>
  tidyr::pivot_longer(
    cols = c("ambient_temp.C", "analyzer_temp.C", "uhv_pressure.mbar"),
    names_to = "channel"
  ) |>
  ggplot() +
  aes(x = time.min, y = value) +
  geom_line() +
  facet_grid(channel ~ filename, scales = "free") +
  labs(x = "time [min]", y = NULL) +
  theme_bw()
```

![Instrument temperatures and vacuum pressure recorded in the status
logs of the two
files.](functionality_guide_files/figure-html/fig-status-log-1.png)

Instrument temperatures and vacuum pressure recorded in the status logs
of the two files.

#### bonus `orbi_get_problems()`

There were no problems reading and/or aggregating the raw data so these
are empty but this can be very helpful to see what went wrong during
reading or aggregation.

``` r

raw_files |> orbi_get_problems()
```

``` fansi
# A tibble: 0 × 6
# ℹ 6 variables: uidx <int>, file <chr>, type <chr>, call <chr>, message <chr>,
#   condition <list>
```

``` r

agg_data |> orbi_get_problems()
```

``` fansi
# A tibble: 0 × 6
# ℹ 6 variables: uidx <int>, file <chr>, type <chr>, call <chr>, message <chr>,
#   condition <list>
```

### `orbi_get_data()`

At this point (and any later point), you can always extract the data of
interest from the aggregated data set using
[`orbi_get_data()`](https://isoorbi.isoverse.org/reference/orbi_get_data.md).
If you prefer working with a data frame tibble from
[`orbi_get_data()`](https://isoorbi.isoverse.org/reference/orbi_get_data.md)
instead of the aggregated data structure, you can switch to that at any
point and use the resulting data frame tibble in subsequent functions.

``` r

# direct access to the data stored in the aggregated dataset
agg_data$file_info
```

``` fansi
# A tibble: 2 × 44
   uidx filepath             filename creation_date       in_aquisition Operator
  <int> <chr>                <chr>    <dttm>              <lgl>         <chr>   
1     1 data/nitrate_test_1… nitrate… 2025-01-30 13:57:12 FALSE         SYSTEM  
2     2 data/nitrate_test_1… nitrate… 2025-01-30 14:01:04 FALSE         SYSTEM  
# ℹ 38 more variables: FileDescription <chr>, MassResolution <chr>,
#   SpectraCount <chr>, FirstSpectrum <chr>, LastSpectrum <chr>,
#   StartTime <chr>, EndTime <chr>, LowMass <chr>, HighMass <chr>,
#   InstrumentCount <chr>, InstrumentModel <chr>, InstrumentName <chr>,
#   SerialNumber <chr>, SoftwareVersion <chr>, HardwareVersion <chr>,
#   RawFileVersion <chr>, InstrumentUnits <chr>, Comment <chr>, SampleId <chr>,
#   SampleName <chr>, SampleType <chr>, SampleWeight <chr>, …
```

``` r

agg_data$scans
```

``` fansi
# A tibble: 11 × 17
    uidx scan.no time.min      tic it.ms resolution microscans basePeakMz
   <int>   <int>    <dbl>    <dbl> <dbl>      <dbl>      <int>      <dbl>
 1     1       1  0.00454 4336653   68.3      60000          1       62.0
 2     1       2  0.00675 3391426.  80.6      60000          1       62.0
 3     1       3  0.00897 3665948.  79.5      60000          1       62.0
 4     1       4  0.0112  5965333  100.       60000          1       62.0
 5     1       5  0.0134  2595905.  94.3      60000          1       62.0
 6     1       6  0.0156  4273768.  55.3      60000          1       62.0
 7     1       7  0.0181  3134818. 131.       60000          1       62.0
 8     1       8  0.0203  3522451.  78.7      60000          1       62.0
 9     1       9  0.0225  4324210. 109.       60000          1       62.0
10     1      10  0.0247  3553078.  95.7      60000          1       62.0
11     2       1  0.00399 6382695   56.6      60000          1       62.0
# ℹ 9 more variables: basePeakIntensity <dbl>, lowMass <dbl>, highMass <dbl>,
#   rawOvFtT <dbl>, intensCompFactor <dbl>, agc <chr>, agcTarget <int>,
#   numberLockmassesFound <int>, analyzerTemperature <dbl>
```

``` r

agg_data$peaks
```

``` fansi
# A tibble: 337 × 8
    uidx scan.no mzMeasured intensity baseline peakNoise peakResolution
   <int>   <int>      <dbl>     <dbl>    <dbl>     <dbl>          <dbl>
 1     1       1       62.0     1211.     8.32      513.          70900
 2     1       1       62.0     1463.     8.32      513.          94100
 3     1       1       62.0     1172.     8.31      513.          80300
 4     1       1       62.0     1116.     8.30      513.          87900
 5     1       1       62.0     1264.     8.30      513.          71108
 6     1       1       62.0     1265.     8.30      513.          70208
 7     1       1       62.0     2320.     8.30      513.         102308
 8     1       1       62.0     1931.     8.29      513.          91808
 9     1       1       62.0     1731.     8.29      513.         106708
10     1       1       62.0     1683.     8.29      513.         123008
# ℹ 327 more rows
# ℹ 1 more variable: centroiderFlags <fct>
```

``` r

agg_data$spectra
```

``` fansi
# A tibble: 943 × 4
    uidx scan.no    mz intensity
   <int>   <int> <dbl>     <dbl>
 1     1       1  60.9        0 
 2     1       1  60.9        0 
 3     1       1  60.9        0 
 4     1       1  60.9        0 
 5     1       1  62.0        0 
 6     1       1  62.0        0 
 7     1       1  62.0        0 
 8     1       1  62.0        0 
 9     1       1  62.0      496.
10     1       1  62.0      935.
# ℹ 933 more rows
```

``` r

# better way to retrieve+combine the data with dplyr select syntax:
agg_data |>
  orbi_get_data(
    file_info = c(
      "filename",
      "creation_date",
      "instrument" = "InstrumentModel"
    ),
    scans = c("time.min", "tic", "resolution"),
    peaks = c("mz" = "mzMeasured", starts_with("peak"))
  )
```

``` fansi
✔ [12ms] orbi_get_data() retrieved 337 records from the combination of
file_info (2), scans (11), and peaks (337) via uidx and scan.no
```

``` fansi
# A tibble: 337 × 11
    uidx filename         creation_date       instrument scan.no time.min    tic
   <int> <chr>            <dttm>              <chr>        <int>    <dbl>  <dbl>
 1     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
 2     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
 3     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
 4     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
 5     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
 6     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
 7     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
 8     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
 9     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
10     1 nitrate_test_10… 2025-01-30 13:57:12 Orbitrap …       1  0.00454 4.34e6
# ℹ 327 more rows
# ℹ 4 more variables: resolution <dbl>, mz <dbl>, peakNoise <dbl>,
#   peakResolution <dbl>
```

## Identifying isotopocules

The next step is identifying isotpocules.

### `orbi_identify_isotopocules()` \*

``` r

# list of isotopocules (can alternatively be in a tsv/csv/xlsx file)
isotopocules <- tibble(
  compound = "nitrate",
  isotopolog = c("M0", "15N", "17O", "18O"),
  mass = c(61.9878, 62.9850, 62.9922, 63.9922),
  tolerance = 1,
  charge = 1
)

# identify
data <- agg_data |> orbi_identify_isotopocules(isotopocules)
```

``` fansi
! [39ms] orbi_identify_isotopocules() identified 50/337 peaks (15%)
representing 96% of the total ion current (TIC) as isotopocules M0, 15N, 17O,
and 18O but encountered 1 warning
  → ! isotopocule M0 matches multiple peaks in some same scans (4 multi-matched
  peaks in total) - make sure to run orbi_flag_satellite_peaks() and
  orbi_plot_satellite_peak()
```

### `orbi_plot_spectra()`

## Data checks

### `orbi_flag_satellite_peaks()` \*

``` r

# this can happen here or later on in the workflow
# in the case of these files there are no satellite peaks
data |>
  orbi_filter_files("nitrate_test_10scans") |>
  orbi_flag_satellite_peaks() |>
  orbi_plot_satellite_peaks()
```

``` fansi
✔ [9ms] orbi_filter_files() filtered the dataset by filenames
(nitrate_test_10scans) and removed a total of 30/337 peaks (8.9%)
```

``` fansi
✔ [7ms] orbi_flag_satellite_peaks() flagged 6/307 peaks in 1 isotopocule (M0)
as satellite peaks (2%)
```

![Satellite peaks in the 10 scan test
file.](functionality_guide_files/figure-html/fig-satellite-peaks-1.png)

Satellite peaks in the 10 scan test file.

### `orbi_plot_isotopocule_coverage()`

``` r

# this can happen here or later on in the workflow
data |> orbi_get_isotopocule_coverage()
```

``` fansi
# A tibble: 12 × 11
    uidx filename     compound isotopocule centroiderFlags data_stretch n_points
   <int> <fct>        <fct>    <fct>       <fct>                  <int>    <int>
 1     1 nitrate_tes… nitrate  M0          exception                  0        1
 2     1 nitrate_tes… nitrate  M0          none                       0       10
 3     1 nitrate_tes… nitrate  M0          exception                  1        2
 4     1 nitrate_tes… nitrate  M0          exception                  2        2
 5     1 nitrate_tes… nitrate  M0          exception                  3        1
 6     1 nitrate_tes… nitrate  15N         none                       0       10
 7     1 nitrate_tes… nitrate  17O         none                       0       10
 8     1 nitrate_tes… nitrate  18O         none                       0       10
 9     2 nitrate_tes… nitrate  M0          none                       0        1
10     2 nitrate_tes… nitrate  15N         none                       0        1
11     2 nitrate_tes… nitrate  17O         none                       0        1
12     2 nitrate_tes… nitrate  18O         none                       0        1
# ℹ 4 more variables: start_scan.no <int>, end_scan.no <int>,
#   start_time.min <dbl>, end_time.min <dbl>
```

``` r

data |>
  orbi_filter_files("nitrate_test_10scans") |>
  orbi_plot_isotopocule_coverage()
```

``` fansi
✔ [8ms] orbi_filter_files() filtered the dataset by filenames
(nitrate_test_10scans) and removed a total of 30/337 peaks (8.9%)
```

![Isotopocule coverage of the 10 scan test
file.](functionality_guide_files/figure-html/fig-coverage-1.png)

Isotopocule coverage of the 10 scan test file.

## Data blocks

### `orbi_define_blocks()`

Data blocks mark which scans belong together (e.g. one sample in a flow
injection) and which scans are not used for the ratio calculations. Each
block is defined either by time (`start_time.min` and `end_time.min`) or
by scan number (`start_scan.no` and `end_scan.no`). By default, a block
is added to every file.

``` r

# one block for all files (by time)
data |>
  orbi_define_blocks(start_time.min = 0, end_time.min = 0.015) |>
  orbi_get_blocks_info()
```

``` fansi
✔ [38ms] orbi_define_blocks() added 1 block to 2 files
```

     → block: covers scans 1 to 5 (0.00399154 to 0.0133997 min) in 2 files

``` fansi
# A tibble: 3 × 11
   uidx filename     data_group block block_name data_type segment start_scan.no
  <int> <chr>             <int> <int> <chr>      <chr>       <int>         <int>
1     1 nitrate_tes…          1     1 NA         data           NA             1
2     1 nitrate_tes…          2     0 NA         unused         NA             6
3     2 nitrate_tes…          1     1 NA         data           NA             1
# ℹ 3 more variables: end_scan.no <int>, start_time.min <dbl>,
#   end_time.min <dbl>
```

To add a block only to specific file(s), use `in_filename`. Like all the
other parameters, it can be a single value or a vector with one value
per block.

``` r

# two blocks (by scan number) only in the 10 scan file
blocks_data <- data |>
  orbi_define_blocks(
    start_scan.no = c(2, 6),
    end_scan.no = c(4, 9),
    block_name = c("first", "second"),
    in_filename = "nitrate_test_10scans"
  )
```

``` fansi
✔ [58ms] orbi_define_blocks() added 2 blocks to 1 file
```

``` fansi
 → block first in nitrate_test_10scans: covers scans 2 to 4 (0.00675358 to
0.0111844 min)
 → block second in nitrate_test_10scans: covers scans 6 to 9 (0.0156155 to
0.0224996 min)
```

``` r

# overview of the blocks in each file
blocks_data |> orbi_get_blocks_info()
```

``` fansi
# A tibble: 6 × 11
   uidx filename     data_group block block_name data_type segment start_scan.no
  <int> <chr>             <int> <int> <chr>      <chr>       <int>         <int>
1     1 nitrate_tes…          1     0 NA         unused         NA             1
2     1 nitrate_tes…          2     1 first      data           NA             2
3     1 nitrate_tes…          3     0 NA         unused         NA             5
4     1 nitrate_tes…          4     2 second     data           NA             6
5     1 nitrate_tes…          5     0 NA         unused         NA            10
6     2 nitrate_tes…          1     0 NA         unused         NA             1
# ℹ 3 more variables: end_scan.no <int>, start_time.min <dbl>,
#   end_time.min <dbl>
```

``` r

blocks_data |>
  orbi_filter_files("nitrate_test_10scans") |>
  orbi_plot_raw_data(y = tic * it.ms, x = "scan.no") +
  geom_point(size = 4) # add the data points for clarity
```

``` fansi
✔ [8ms] orbi_filter_files() filtered the dataset by filenames
(nitrate_test_10scans) and removed a total of 30/337 peaks (8.9%)
```

![The two blocks defined in the 10 scan test file (scans outside the
blocks are
unused).](functionality_guide_files/figure-html/fig-blocks-1.png)

The two blocks defined in the 10 scan test file (scans outside the
blocks are unused).

For many blocks, it is often easier to provide them as a `blocks_table`
(which could also come from a spreadsheet) with one row per block. An
`in_filename` of `NA` adds that block to all files.

``` r

blocks_table <- tibble(
  start_scan.no = c(1, 2, 6),
  end_scan.no = c(1, 4, 9),
  block_name = c("all", "first", "second"),
  in_filename = c(NA, "nitrate_test_10scans", "nitrate_test_10scans")
)
blocks_table
```

``` fansi
# A tibble: 3 × 4
  start_scan.no end_scan.no block_name in_filename         
          <dbl>       <dbl> <chr>      <chr>               
1             1           1 all        NA                  
2             2           4 first      nitrate_test_10scans
3             6           9 second     nitrate_test_10scans
```

``` r

data |>
  orbi_define_blocks(blocks_table = blocks_table) |>
  orbi_get_blocks_info()
```

``` fansi
✔ [90ms] orbi_define_blocks() added 3 blocks to 2 files
```

``` fansi
 → block all: covers scans 1 to 1 (0.00399154 to 0.00453811 min) in 2 files
 → block first in nitrate_test_10scans: covers scans 2 to 4 (0.00675358 to
0.0111844 min)
 → block second in nitrate_test_10scans: covers scans 6 to 9 (0.0156155 to
0.0224996 min)
```

``` fansi
# A tibble: 6 × 11
   uidx filename     data_group block block_name data_type segment start_scan.no
  <int> <chr>             <int> <int> <chr>      <chr>       <int>         <int>
1     1 nitrate_tes…          1     1 all        data           NA             1
2     1 nitrate_tes…          2     2 first      data           NA             2
3     1 nitrate_tes…          3     0 NA         unused         NA             5
4     1 nitrate_tes…          4     3 second     data           NA             6
5     1 nitrate_tes…          5     0 NA         unused         NA            10
6     2 nitrate_tes…          1     1 all        data           NA             1
# ℹ 3 more variables: end_scan.no <int>, start_time.min <dbl>,
#   end_time.min <dbl>
```

## Ratio calculations

### `orbi_define_basepeak()` \*

### `orbi_summarize_results()` \*
