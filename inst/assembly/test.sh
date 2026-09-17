#!/bin/bash
# Smoke tests for the isoraw executable.
#
# Designed to work both as a local call:
#   bash test.sh
#   bash test.sh exe=dist/isoraw-linux-x64
# AND via the dotnet docker image (see the `test` task in the Rakefile):
#   docker run --rm -v $PWD:/app -w /app mcr.microsoft.com/dotnet/sdk:8.0 /app/test.sh
#
# Exits 0 if all tests pass, 1 otherwise.

# note: deliberately no `set -e` - we want to run every test and report at the end
set -uo pipefail

echo ""
echo "--- STARTING TEST SCRIPT ---"

# Parameters
exe=""
test_dir="$(cd "$(dirname "$0")" && pwd)/tests"

for arg in "$@"; do
  case $arg in
    exe=*)
      exe="${arg#*=}"
      shift
      ;;
    tests=*)
      test_dir="${arg#*=}"
      shift
      ;;
    *)
      echo "Unknown option: $arg"
      exit 1
      ;;
  esac
done

# Find the executable for this platform if it was not provided
if [[ -z "$exe" ]]; then
  case "$(uname -s)" in
    Darwin*)            exe="dist/isoraw-osx-x64" ;;
    Linux*)             exe="dist/isoraw-linux-x64" ;;
    MINGW*|MSYS*|CYGWIN*) exe="dist/isoraw-win-x64.exe" ;;
    *)                  echo "Unsupported OS: $(uname -s)"; exit 1 ;;
  esac
fi

if [[ ! -f "$exe" ]]; then
  echo "Error: executable not found at '$exe'"
  echo "Build it first (e.g. 'rake build') or pass one with 'exe=<path>'"
  exit 1
fi
if [[ ! -d "$test_dir" ]]; then
  echo "Error: test file directory not found at '$test_dir'"
  exit 1
fi
shopt -s nullglob
test_files=("$test_dir"/*.raw)
shopt -u nullglob
if [[ ${#test_files[@]} -eq 0 ]]; then
  echo "Error: no .raw test files found in '$test_dir'"
  exit 1
fi

echo "Executable: $exe"
echo "Test files: $test_dir"

# Test helpers
passed=0
failed=0

ok() {
  echo "  PASS: $1"
  passed=$((passed + 1))
}

ko() {
  echo "  FAIL: $1"
  failed=$((failed + 1))
}

# assert that $1 (haystack) contains $2 (needle), described by $3
assert_contains() {
  if [[ "$1" == *"$2"* ]]; then ok "$3"; else ko "$3 (expected to find '$2')"; fi
}

# assert that $1 (haystack) does NOT contain $2 (needle), described by $3
assert_not_contains() {
  if [[ "$1" != *"$2"* ]]; then ok "$3"; else ko "$3 (did not expect '$2')"; fi
}

assert_file() {
  if [[ -f "$1" ]]; then ok "$2"; else ko "$2 (missing $1)"; fi
}

assert_no_file() {
  if [[ ! -f "$1" ]]; then ok "$2"; else ko "$2 (unexpected $1)"; fi
}

# assert that file $1 is strictly smaller than file $2, described by $3
assert_smaller() {
  local a b
  if [[ ! -f "$1" || ! -f "$2" ]]; then
    ko "$3 (missing $([[ -f "$1" ]] || echo "$1") $([[ -f "$2" ]] || echo "$2"))"
    return
  fi
  a=$(wc -c <"$1" | tr -d ' ')
  b=$(wc -c <"$2" | tr -d ' ')
  if [[ "$a" -lt "$b" ]]; then ok "$3 ($a < $b bytes)"; else ko "$3 ($a not < $b bytes)"; fi
}

# remove the .cache folder belonging to a raw file
clean_cache() {
  rm -rf "$1.cache"
}

# --- version and help -------------------------------------------------------

echo ""
echo "TEST: --version and --help"
out=$("$exe" --version 2>&1)
assert_contains "$out" "isoraw version" "--version reports a version"
out=$("$exe" --help 2>&1)
assert_contains "$out" "Usage:" "--help reports usage"
assert_contains "$out" "--skipProblematicPeaks" "--help lists --skipProblematicPeaks"

# --- missing file -----------------------------------------------------------

echo ""
echo "TEST: missing input file"
out=$("$exe" --file "$test_dir/does_not_exist.raw" 2>&1)
assert_contains "$out" "Error:" "missing file reports an error"

# --- full read of every test file -------------------------------------------

for raw in "${test_files[@]}"; do
  name=$(basename "$raw")
  echo ""
  echo "TEST: full read of $name"
  clean_cache "$raw"
  out=$("$exe" --file "$raw" --spectra 1 2>&1)
  assert_not_contains "$out" "ERROR:" "$name reads without errors"
  assert_file "$raw.cache/file_info.parquet" "$name writes file_info.parquet"
  assert_file "$raw.cache/scans.parquet" "$name writes scans.parquet"
  assert_file "$raw.cache/peaks.parquet" "$name writes peaks.parquet"
  assert_file "$raw.cache/spectra.parquet" "$name writes spectra.parquet"
  clean_cache "$raw"
done

# --- --spectra all ----------------------------------------------------------

raw="$test_dir/exceptions.raw"
echo ""
echo "TEST: --spectra all"
clean_cache "$raw"
out=$("$exe" --file "$raw" --spectra all 2>&1)
assert_not_contains "$out" "ERROR:" "--spectra all reads without errors"
assert_file "$raw.cache/spectra.parquet" "--spectra all writes spectra.parquet"
clean_cache "$raw"

# --- --skip -----------------------------------------------------------------

echo ""
echo "TEST: --skip fileInfo,scans,peaks"
clean_cache "$raw"
out=$("$exe" --file "$raw" --skip fileInfo,scans,peaks 2>&1)
assert_not_contains "$out" "ERROR:" "skipping everything reads without errors"
assert_no_file "$raw.cache/file_info.parquet" "--skip fileInfo omits file_info.parquet"
assert_no_file "$raw.cache/scans.parquet" "--skip scans omits scans.parquet"
assert_no_file "$raw.cache/peaks.parquet" "--skip peaks omits peaks.parquet"
clean_cache "$raw"

# --- --skipProblematicPeaks ---------------------------------------------------
# both test files carry problematic peaks, so filtering them out has to produce a
# smaller peaks.parquet in each case while still writing the remaining peaks

for raw in "${test_files[@]}"; do
  name=$(basename "$raw")
  echo ""
  echo "TEST: --skipProblematicPeaks on $name"
  # stash the unfiltered peaks outside the cache folder so the second run cannot touch it
  all_peaks=$(mktemp)
  clean_cache "$raw"
  "$exe" --file "$raw" --skip fileInfo,scans >/dev/null 2>&1
  cp "$raw.cache/peaks.parquet" "$all_peaks"
  clean_cache "$raw"
  out=$("$exe" --file "$raw" --skip fileInfo,scans --skipProblematicPeaks 2>&1)
  assert_contains "$out" "problematic peaks" "$name reports --skipProblematicPeaks in the banner"
  assert_not_contains "$out" "ERROR:" "$name reads without errors"
  assert_file "$raw.cache/peaks.parquet" "$name still writes peaks.parquet"
  assert_smaller "$raw.cache/peaks.parquet" "$all_peaks" "$name drops peaks"
  rm -f "$all_peaks"
  clean_cache "$raw"
done

# --- summary ----------------------------------------------------------------

echo ""
if [[ "$failed" -eq 0 ]]; then
  echo "--- ALL $passed TESTS PASSED ---"
  exit 0
else
  echo "--- $failed of $((passed + failed)) TESTS FAILED ---"
  exit 1
fi
