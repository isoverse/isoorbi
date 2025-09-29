# isoraw executable commandline options

## Usage

Directly from command line (assuming the executables are in the current working directory):

```
# on macOS
isoraw-osx-x64     [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks>] [--spectra <all|1,4,6>]

# on Linux
isoraw-linux-x64   [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks>] [--spectra <all|1,4,6>]

# on Windows
isoraw-win-x64.exe [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks>] [--spectra <all|1,4,6>]
```

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
 - `peaks.parquet`: all peaks from the raw file including their masses, intensities, noise, baselines, etc.
 - `spectra.parquet`: all spectra from the raw file (becuase of `scans=all`, otherwise just the scans selected) - this file gets very big if all spectra are exported (typically larger than the original raw file) which is why it is recommended to focus just on the scans of interest

## Build

To build the executables from scratch, it is highly recommended to use the rakefile (install dependencies with `bundle install`) which provides compilation via docker. 

```
# build the executable for local OS
rake build 

# build the executables for mac os, windows, and linux
rake buildAll
```

The resulting executables are stored in the `out` folder and can be used from there, copied elsewhere, or uploaded for distribution in a release. Note that local build is also possible (`rake buildLocal`) if dotnet is installed but not recommended. On MacOS it leads to non-functional binaries for osx (see [this issue](https://github.com/thermofisherlsms/RawFileReader/issues/3)).
