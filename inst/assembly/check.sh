#!/bin/bash
#check for the RawFileReader depencency

# Setup
set -euo pipefail

# Parameters
project_folder="$PWD"

for arg in "$@"; do
  case $arg in
    project=*)
      project_folder="${arg#*=}"
      shift
      ;;
    *)
      echo "Unknown option: $arg"
      exit 1
      ;;
  esac
done

if [[ -z "$project_folder" ]]; then
  echo "Error: One or more required arguments are empty."
  echo "Usage: $0 project=<project_folder>"
  exit 1
fi

# Check dependency
source_name="RawFileReader"
if [ ! -d "$project_folder/RawFileReader" ]; then
    git clone --depth=1 https://github.com/thermofisherlsms/RawFileReader.git "$project_folder/RawFileReader"
fi
if dotnet nuget list source | grep -Fq "$source_name"; then
  echo "NuGet source for $source_name already exists, skipping add."
else
  dotnet nuget add source "$project_folder/RawFileReader/Libs/NetCore/Net8/" --name "$source_name"
fi
