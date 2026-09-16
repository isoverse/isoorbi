# isoraw executable commandline options

## Usage

Directly from command line (assuming the executables are in the current working directory):

```
# on macOS
isoraw-osx-x64     [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks>] [--skipProblematicPeaks] [--spectra <all|1,4,6>]

# on Linux
isoraw-linux-x64   [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks>] [--skipProblematicPeaks] [--spectra <all|1,4,6>]

# on Windows
isoraw-win-x64.exe [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks>] [--skipProblematicPeaks] [--spectra <all|1,4,6>]
```

### Options

 - `--skip <fileInfo,scans,peaks>`: skip reading any combination of these datasets entirely
 - `--skipProblematicPeaks`: only process peaks that are **unproblematic**, see below. By default **all** peaks are processed.
 - `--spectra <all|1,4,6>`: which scan spectra to include (see note about file size below)

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

Or via the rakefile (install with `bundle install`) assuming the executables are in the `out` directory of the current working directory (e.g. if they've been built via `rake buildAll`, see below):

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

 - `file_info.parquet`: exported information from the file headers and instrument
 - `scans.parquet`: all scans in the raw file with details information available on the scans
 - `peaks.parquet`: all peaks from the raw file including their masses, intensities, noise, baselines, etc. as well as the raw `flags` column (the Thermo `PeakOptions` bitmask: 1 = saturated, 2 = fragmented, 4 = merged, 8 = exception, 16 = reference, 32 = modified, 64 = lock peak) which can be decoded downstream
 - `spectra.parquet`: all spectra from the raw file (becuase of `scans=all`, otherwise just the scans selected) - this file gets very big if all spectra are exported (typically larger than the original raw file) which is why it is recommended to focus just on the scans of interest

## Build

To build the executables from scratch, it is highly recommended to use the rakefile (install dependencies with `bundle install`) which provides compilation via docker. 

```
# build the executable for local OS
rake build 

# build the executables for mac os, windows, and linux
rake buildAll
```

The resulting executables are stored in the `out` folder and can be used from there, copied elsewhere (e.g. to your local isoorbi package via `isoorbi::orbi_check_isoraw(source = "PATH/TO/out", reinstall_always = TRUE)` - then check with `isoorbi:::get_isoraw_version()`), or uploaded for distribution in a release. Note that local build is also possible (`rake buildLocal`) if dotnet is installed but not recommended. On MacOS it leads to non-functional binaries for osx (see [this issue](https://github.com/thermofisherlsms/RawFileReader/issues/3)).

## Tests

`test.sh` runs a set of smoke tests against a built executable using the raw files in `tests/`. These are also run automatically on Linux, Windows and macOS by the `isoraw test` GitHub action whenever the assembly sources or tests change. It checks `--version`/`--help`, a full read of every test file (all four parquet outputs), the `--skip` options, and that `--skipProblematicPeaks` actually drops peaks. It exits non-zero if any test fails.

```
# build first, then test the executable for the local OS
rake build
rake test

# or do both in one go
rake buildAndTest

# directly, optionally against a specific executable / test folder
bash test.sh
bash test.sh exe=out/isoraw-linux-x64
bash test.sh exe=out/isoraw-linux-x64 tests=/path/to/other/raw/files
```

The test files in `tests/` are deliberately small and cover different peak flag combinations:

 - `exceptions.raw`: 10 scans, 307 peaks that are unflagged (126), exception (161) and fragmented+exception (20) - drops to 126 peaks with `--skipProblematicPeaks`
 - `reference.raw`: 1 scan, 16 peaks that are unflagged (4), exception (7), fragmented+exception (4) and reference (1) - drops to 5 peaks with `--skipProblematicPeaks`, i.e. the reference peak is retained

Note that neither file currently contains a peak that carries the reference or lock mass flag *together with* another flag, so that part of the `--skipProblematicPeaks` condition is not covered by these test files.
