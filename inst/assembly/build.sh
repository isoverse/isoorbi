#!/bin/bash
# This file was adapted from the build.sh in https://github.com/fgcz/rawrr 
# Designed to work both as local call:
# bash build.sh
# bash build.sh version=0.1 runtime=osx-x64
# AND via the dotnet docker image:
# docker pull mcr.microsoft.com/dotnet/sdk:8.0
# docker run --rm -v $PWD:/app -w /app mcr.microsoft.com/dotnet/sdk:8.0 /app/build.sh project=/app output=/app/out version=0.1 runtime=osx-x64

# Setup
set -euo pipefail
# set -euxo pipefail 
SECONDS=0  # built-in Bash timer
echo ""
echo "--- STARTING BUILD SCRIPT ---"

# Parameters
project_folder="$PWD"
output_folder="$PWD/out"
version="DEV"
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
    version=*)
      version="${arg#*=}"
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

if [[ -z "$project_folder" || -z "$output_folder" || -z "$version" ]]; then
  echo "Error: One or more required arguments are empty."
  echo "Usage: $0 project=<project_folder> output=<output_folder> version=<version> [runtime=<runtime>]"
  exit 1
fi

# Dependencies
source_name="RawFileReader"
if [ ! -d "$project_folder/RawFileReader" ]; then
    git clone --depth=1 https://github.com/thermofisherlsms/RawFileReader.git "$project_folder/RawFileReader"
fi
if dotnet nuget list source | grep -Fq "$source_name"; then
  echo "NuGet source for $source_name already exists, skipping add."
else
  dotnet nuget add source "$project_folder/RawFileReader/Libs/NetCore/Net8/" --name "$source_name"
fi

# Build
work_folder="/tmp/build"
rm -rf "$work_folder"
mkdir -p "$work_folder"
cp "$project_folder"/*.cs "$work_folder/"
cp "$project_folder"/isoraw.csproj "$work_folder/"
cd "$work_folder"
for runtime in $runtimes; do
    echo "Compiling $runtime..."
    dotnet publish isoraw.csproj --runtime "$runtime" -c Release
    echo "Finished compiling $runtime"
done

# Export
mkdir -p "$output_folder"
for runtime in $runtimes; do
    echo "Moving exectuable for $runtime version $version to output folder"
    source_path="bin/Release/net8.0/$runtime/publish"
    suffix=""
    if [[ "$runtime" == win-* ]]; then
        suffix=".exe"
    fi
    cp "$source_path/isoraw$suffix" "$output_folder/isoraw-$runtime$suffix-v$version"
done

# Finish
echo "--- COMPLETED IN ${SECONDS} SECONDS ---"