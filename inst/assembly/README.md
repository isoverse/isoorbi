# isoraw executable commandline options

See [NEWS.md](NEWS.md) for the changes in each version of isoraw.

## Usage

Directly from command line (assuming the executables are in the current working directory):

```
# on macOS
isoraw-osx-x64     [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks,statusLog>] [--skipProblematicPeaks] [--spectra <all|1,4,6>]

# on Linux
isoraw-linux-x64   [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks,statusLog>] [--skipProblematicPeaks] [--spectra <all|1,4,6>]

# on Windows
isoraw-win-x64.exe [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks,statusLog>] [--skipProblematicPeaks] [--spectra <all|1,4,6>]
```

### Options

 - `--skip <fileInfo,scans,peaks,statusLog>`: skip reading any combination of these datasets entirely, see [Output](#output) for what each of them holds. All four are read by default.
 - `--skipProblematicPeaks`: only process peaks that are **unproblematic**, see below. By default **all** peaks are processed.
 - `--spectra <all|1,4,6>`: which scan spectra to include (see note about file size below). Unlike the other datasets, spectra are only read when this option asks for them.

#### Problematic peaks

Every peak carries a `PeakOptions` bitmask that the centroider assigned to it, reported as-is in the `flags` column of `peaks.parquet`:

| bit | flag | meaning |
|---:|---|---|
| 0 | - | no flags, an ordinary peak |
| 1 | `Saturated` | signal over the ADC limit |
| 2 | `Fragmented` | peak split by the centroider |
| 4 | `Merged` | peaks combined by the centroider |
| 8 | `Exception` | part of the reference, but not used by calibration |
| 16 | `Reference` | hi-res internal reference compound |
| 32 | `Modified` | mathematically modified packet |
| 64 | `LockPeak` | high resolution SIM lock mass |

Only `Reference` and `LockPeak` describe *what a peak is*. Every other flag says something went wrong with the centroiding, which makes the peak unreliable for quantification. Despite the name of the `Exception` flag, it is not special in this regard - a `Saturated`, `Fragmented`, `Merged` or `Modified` peak is just as problematic.

`--skipProblematicPeaks` therefore keeps a peak only if its flags are **purely** none, reference and/or lock mass:

| flags | kept? |
|---|---|
| `0` (no flags) | yes |
| `Reference`, `LockPeak`, `Reference\|LockPeak` | yes |
| `Exception`, `Saturated`, `Fragmented`, `Merged`, `Modified`, ... | no |
| `Reference\|Fragmented`, `LockPeak\|Exception`, ... | no - a reference or lock mass peak that carries *any* other flag is problematic too |

#### Size impact of the problematic peaks

Problematic peaks make up a substantial fraction of a typical file, so keeping them (the default) makes `peaks.parquet` noticeably bigger. For a representative 27 MB raw file with 290k peaks:

| | peaks | `peaks.parquet` |
|---|---:|---:|
| default (all peaks) | 290,110 | 6.7 MB |
| `--skipProblematicPeaks` | 219,864 | 5.0 MB |

i.e. roughly **1.3x larger** on disk and 24% more rows. The 70,246 dropped peaks in that file are 58,265 `Exception`, 11,147 `Fragmented|Exception`, 773 `Fragmented`, 38 `Merged` and 23 `Fragmented|Merged`. The default is still to keep everything, since the `flags` column allows exactly the same filtering downstream - use `--skipProblematicPeaks` when disk space or read speed matter more than being able to revisit those peaks later.

Or via the rakefile (install with `bundle install`) assuming the executables are in the `dist` directory of the current working directory (e.g. if they've been built via `rake buildAll`, see below):

```
# show the version
rake version

# read test.raw including scans 1 and 5
rake run file=test.raw scans=1,5

# read only the fileInfo
rake fileInfo file=test.raw

# read only the scans
rake scans file=test.raw

# read only the peaks
rake peaks file=test.raw

# read only the status log
rake statusLog file=test.raw

# read only the spectra (all of them)
rake spectra file=test.raw spectra=all
```

## Output

Generates a folder with the same name as the raw file + `.cache` (for example, if `test.raw` is the test file, generates a folder called `test.raw.cache` in the same directory as the raw file) and stores all read data as `.parquet` files. These parquet files can be read at very high efficiency directly into R and python.

Example:

```
rake run file=test.raw scans=all
```

This creates the folder `test.raw.cache` with the following files:

| file | one row per | columns | contents |
|---|---|---|---|
| [`file_info.parquet`](#file_infoparquet) | file | always the same | file headers, instrument and sample information |
| [`scans.parquet`](#scansparquet) | scan | fixed + instrument-dependent | scan statistics and the complete scan trailer |
| [`peaks.parquet`](#peaksparquet) | centroided peak | always the same | m/z, intensity, resolution, noise, baseline and centroider flags |
| [`status_log.parquet`](#status_logparquet) | status log entry | instrument-dependent | instrument readbacks recorded independently of the scans |
| [`spectra.parquet`](#spectraparquet) | profile data point | always the same | the profile spectra of the selected scans |

`scans.parquet`, `peaks.parquet` and `spectra.parquet` are linked by the `scan.no` column. The status log runs on its own schedule and is linked to the scans by retention time (`StartTime` in both tables).

Columns that are *always the same* are typed (integer, double, boolean, timestamp), columns that depend on the instrument are always text and are meant to be cast downstream, since neither their names nor their types can be relied upon across instruments and methods.

### `file_info.parquet`

A single row describing the file as a whole:

 - **file**: `FileName`, `CreationDate` (timestamp), `InAquisition` (whether the file was still being acquired)
 - **run headers**: `Operator`, `FileDescription`, `MassResolution`, `SpectraCount`, `FirstSpectrum`, `LastSpectrum`, `StartTime` and `EndTime` (retention time range of the run in minutes), `LowMass` and `HighMass` (m/z range of the run)
 - **instrument**: `InstrumentCount`, `InstrumentModel`, `InstrumentName`, `SerialNumber`, `SoftwareVersion`, `HardwareVersion`, `RawFileVersion`, `InstrumentUnits`
 - **sample**: `Comment`, `SampleId`, `SampleName`, `SampleType`, `SampleWeight`, `SampleVolume`, `Barcode`, `RowNumber`, `Vial`, `InjectionVolume`, `DilutionFactor`, `IstdAmount`, `CalibrationLevel`, `InstrumentMethodFile`, `CalibrationFile`, `ProcessingMethodFile` and `UserText0` to `UserText4`

The sample fields are whatever was entered in the acquisition software (sample list / sequence) and are frequently empty.

### `scans.parquet`

One row per scan. The scan statistics come first and are always present:

| column | type | contents |
|---|---|---|
| `scan.no` | integer | the scan number, links to `peaks.parquet` and `spectra.parquet` |
| `StartTime` | double | retention time of the scan in minutes |
| `TIC` | double | total ion current of the scan |
| `BasePeakMass`, `BasePeakIntensity` | double | m/z and intensity of the most intense peak in the scan |
| `LowMass`, `HighMass` | double | m/z range of the scan |
| `IsCentroidScan` | boolean | whether the scan itself was recorded centroided |
| `ScanType` | text | the Thermo filter string, e.g. `FTMS + p ESI SIM ms [143.0000-171.0000]` |

Everything after that is the **scan trailer** (the "trailer extra" information), i.e. the settings and readbacks the instrument records for each individual scan. Which fields exist depends entirely on the instrument and the method, so they are read dynamically and kept as text with their trailing colon removed. Common entries are `Ion Injection Time (ms)`, `AGC Target`, `FT Resolution`, `Micro Scan Count`, `Analyzer Temperature`, `Number of LM Found`, `LM m/z-Correction (ppm)`, `S-Lens RF Level` or `FAIMS CV`, and just like in the status log the section headings (e.g. `=== Mass Calibration: ===`) are kept as empty columns so the grouping stays visible. For comparison: an Orbitrap Exploris 240 file has 72 trailer fields (81 columns in total), an Orbitrap Eclipse file 38 (47 columns in total).

### `peaks.parquet`

One row per centroided peak in every scan, i.e. the data that peak identification and quantification are based on:

| column | type | contents |
|---|---|---|
| `scan.no` | integer | the scan the peak belongs to, links to `scans.parquet` |
| `mass` | double | measured m/z of the peak |
| `intensity` | double | measured intensity of the peak |
| `resolution` | double | resolution of the peak |
| `baseline` | double | baseline at the peak |
| `noise` | double | noise at the peak (the basis for signal-to-noise) |
| `flags` | integer | the `PeakOptions` bitmask the centroider assigned to the peak, see [problematic peaks](#problematic-peaks) for what the bits mean |

Reference and lock mass peaks are included - they are identified by their `flags` rather than dropped - and so are the problematic peaks unless `--skipProblematicPeaks` is used.

### `status_log.parquet`

One row per status log entry. The status log holds the instrument readbacks that are recorded independently of the scans (typically every couple of seconds), i.e. the traces that Thermo's Qual Browser offers as *trace types*:

| column | type | contents |
|---|---|---|
| `log.no` | integer | the number of the log entry |
| `StartTime` | double | retention time of the log entry in minutes, on the same time base as `StartTime` in `scans.parquet` |
| one per channel | text | the recorded value of that channel |

Which channels exist depends entirely on the instrument, so they are read dynamically the same way the scan trailer is. An Orbitrap Exploris 240 records 196 channels organized in the sections `Overall Status`, `Ion Source`, `Ion Optics`, `Temperatures`, `Diagnostic Data`, `FAIMS Device` and `Collaborator Interface`, e.g.:

| channel | value |
|---|---|
| `Ambient temp. (°C)` | 30.53940642404822 |
| `Orbitrap block temp. (°C)` | 33.677056130200654 |
| `Detector temp. (°C)` | 37.870780724509416 |
| `ICB: UHV pres. (mbar)` | 5.4081987748194324E-11 |
| `ICB: Power-on time (h)` | 3598.25 |

A few details worth knowing:

 - the values are read **unformatted**, i.e. with the full precision of the recorded number instead of the rounded value the instrument displays (`33.677056130200654` rather than `33.68`)
 - the section headings are kept as (empty) columns so the grouping of the channels remains visible in the column order
 - a channel name that occurs in more than one section is prefixed with its section to keep the columns unambiguous (e.g. `TURBO PUMP 1: Temperature (°C)`, `TURBO PUMP 2: Temperature (°C)`, ...), anything still ambiguous afterwards gets a number appended (e.g. `Sweep Gas (2)`)
 - how often entries are recorded is up to the instrument and entirely independent of the scan rate (every 2 seconds on the Orbitrap Eclipse and every 10 seconds on the Orbitrap Exploris 240 files tested here), so the log can hold far fewer or far more entries than there are scans - a short acquisition may contain a single entry only

### `spectra.parquet`

One row per profile data point (`scan.no`, `mass`, `intensity`) of the scans selected with `--spectra`. This is the full profile spectrum, i.e. every position the analyzer recorded rather than just the centroided peaks, which makes it by far the largest of the datasets - reading all spectra typically produces a file larger than the original raw file. Nothing is written when no spectra are requested (the default), so select the scans of interest instead of reading all spectra whenever possible.

### Size and speed of the datasets

Example: for a 77 MB, 98 minute Orbitrap Eclipse file with 10,136 scans, read in one go with `--spectra all`:

| file | rows | columns | size | read + write |
|---|---:|---:|---:|---:|
| `file_info.parquet` | 1 | 42 | 6.5 kB | 0.2 s |
| `scans.parquet` | 10,136 | 47 | 310 kB | 1.9 s |
| `peaks.parquet` | 1,089,259 | 7 | 25.1 MB | 1.2 s |
| `status_log.parquet` | 2,926 | 207 | 674 kB | 0.6 s |
| `spectra.parquet` | 13,245,482 | 3 | 142 MB | 3.0 s |

Everything except the spectra is relatively cheap in both time and disk space.

## Build

To build the executables from scratch, it is highly recommended to use the rakefile (install dependencies with `bundle install`) which provides compilation via docker. 

```
# build the executable for local OS
rake build 

# build the executables for mac os, windows, and linux
rake buildAll
```

The resulting executables are stored in the `dist` folder and can be used from there, copied elsewhere (e.g. to your local isoorbi package via `isoorbi::orbi_check_isoraw(source = "PATH/TO/dist", reinstall_always = TRUE)` - then check with `isoorbi:::get_isoraw_version()`), or uploaded for distribution in a release. Note that local build is also possible (`rake buildLocal`) if dotnet is installed but not recommended. On MacOS it leads to non-functional binaries for osx (see [this issue](https://github.com/thermofisherlsms/RawFileReader/issues/3)).

## Tests

`test.sh` runs a set of smoke tests against a built executable using the raw files in `tests/`. These are also run automatically on Linux, Windows and macOS by the `isoraw test` GitHub action whenever the assembly sources or tests change. It checks `--version`/`--help`, a full read of every test file (all five parquet outputs), the `--skip` options (including that the status log is read by default and omitted with `--skip statusLog`), and that `--skipProblematicPeaks` actually drops peaks. It exits non-zero if any test fails.

```
# build first, then test the executable for the local OS
rake build
rake test

# or do both in one go
rake buildAndTest

# directly, optionally against a specific executable / test folder
bash test.sh
bash test.sh exe=dist/isoraw-linux-x64
bash test.sh exe=dist/isoraw-linux-x64 tests=/path/to/other/raw/files
```

The test files in `tests/` are deliberately small and cover different peak flag combinations:

 - `exceptions.raw`: 10 scans, 307 peaks that are unflagged (126), exception (161) and fragmented+exception (20) - drops to 126 peaks with `--skipProblematicPeaks`
 - `reference.raw`: 1 scan, 16 peaks that are unflagged (4), exception (7), fragmented+exception (4) and reference (1) - drops to 5 peaks with `--skipProblematicPeaks`, i.e. the reference peak is retained

Note that neither file currently contains a peak that carries the reference or lock mass flag *together with* another flag, so that part of the `--skipProblematicPeaks` condition is not covered by these test files.
