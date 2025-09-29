#!/bin/bash
# This file was adapted from the build.sh in https://github.com/fgcz/rawrr 
# Designed to work both as local call:
# bash build.sh
# bash build.sh runtime=osx-x64
# AND via the dotnet docker image:
# docker pull mcr.microsoft.com/dotnet/sdk:8.0
# docker run --rm -v $PWD:/app -w /app mcr.microsoft.com/dotnet/sdk:8.0 /app/build.sh project=/app output=/app/out runtime=osx-x64

# Setup
set -euo pipefail
# set -euxo pipefail 
SECONDS=0  # built-in Bash timer
echo ""
echo "--- STARTING BUILD SCRIPT ---"

# Parameters
project_folder="$PWD"
output_folder="$PWD/out"
runtimes="linux-x64 osx-x64 win-x64"

for arg in "$@"; do
  case $arg in
    project=*)
      project_folder="${arg#*=}"
      shift
      ;;
    output=*)
      output_folder="${arg#*=}"
      shift
      ;;
    runtime=*)
      runtimes="${arg#*=}"
      shift
      ;;
    *)
      echo "Unknown option: $arg"
      exit 1
      ;;
  esac
done

if [[ -z "$project_folder" || -z "$output_folder" ]]; then
  echo "Error: One or more required arguments are empty."
  echo "Usage: $0 project=<project_folder> output=<output_folder> [runtime=<runtime>]"
  exit 1
fi

# Dependencies
bash ./check.sh project="$project_folder"

# Build
cd "$project_folder"/src
for runtime in $runtimes; do
    echo "Compiling $runtime..."
    dotnet publish isoraw.csproj --runtime "$runtime" -c Release
    echo "Finished compiling $runtime"
done

# Export
mkdir -p "$output_folder"
for runtime in $runtimes; do
    echo "Moving exectuable for $runtime to output folder"
    source_path="bin/Release/net8.0/$runtime/publish"
    suffix=""
    # .exe suffix for windows
    if [[ "$runtime" == win-* ]]; then
        suffix=".exe"
    fi
    cp "$source_path/isoraw$suffix" "$output_folder/isoraw-$runtime$suffix"
    # make executable for linux/mac os
    if [[ "$runtime" == osx-* || "$runtime" == linux-* ]]; then
      chmod +x "$output_folder/isoraw-$runtime$suffix"
    fi
done

# Finish
echo "--- COMPLETED IN ${SECONDS} SECONDS ---"