/*
  This implementation by Sebastian Kopf <sebastian.kopf@colorado.edu> is optimized
  for reading data relevant to Orbitrap IRMS instrumentation and storing the resulting
  data in parquet files for easy + type-safe + fast access from R and Python. 
  
  It would not have been possible without the example provided by Jim Shofstahl at
  https://github.com/thermofisherlsms/RawFileReader and the raw file reader developed
  by Witold Wolski, Christian Panse, Christian Trachsel, and Tobias Kockmann as part
  of the rawrr R package at https://github.com/fgcz/rawrr
    
  Using this code is subject to agreeing to the Thermo license agreement for the Thermo RawFileReader 
  available at https://github.com/thermofisherlsms/RawFileReader and included with this repository
  (inst/licenses/RawFileReaderLicense.txt)
*/

// System libraries
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;

// Parquet libraries
using Parquet;
using Parquet.Schema;
using Parquet.Data;
using Parquet.Serialization;

// Thermo libraries
using ThermoFisher.CommonCore.Data;
using ThermoFisher.CommonCore.Data.Business;
using ThermoFisher.CommonCore.Data.FilterEnums;
using ThermoFisher.CommonCore.Data.Interfaces;
using ThermoFisher.CommonCore.MassPrecisionEstimator;
using ThermoFisher.CommonCore.RawFileReader;

namespace Isoorbi
{

    /// <summary>
    /// Main class implementing methods for reading raw files for isoorbi.
    /// </summary>
    public static class RawReader
    {

        /// <summary>
        /// Read file.
        /// </summary>
        /// <param name="path">
        /// Path to the raw file.
        /// </param>
        /// <param name="output">
        /// Where to save the resulting parquet files to.
        /// </param>
        /// <param name="skipFileInfo">
        /// Whether to skip reading the file info from the headers and instrument details.
        /// </param>
        /// <param name="skipScans">
        /// Whether to skip reading the scans info.
        /// </param>
        /// <param name="skipPeaks">
        /// Whether to skip reading the peaks.
        /// </param>
        /// <param name="allSpectra">
        /// [Optional] If set to true, reads all available scan spectra and ignores the 'spectra' parameter.
        /// </param>
        /// <param name="spectra">
        /// [Optional] Array of scans from which to read the spectra.
        /// </param>
        public static async Task ReadFile(string path, string output, bool skipFileInfo = false, bool skipScans = false, bool skipPeaks = false, bool allSpectra = false, int[]? spectra = null)
        {

            // default for spectra == null
            spectra ??= new int[0];

            // safety checks
            if (string.IsNullOrEmpty(path))
            {
                throw new FileNotFoundException("No RAW file specified");
            }
            if (!File.Exists(path))
            {
                throw new FileNotFoundException($"RAW file not found: {path}");
            }

            // open raw file with thread manager
            // note: it is basically no overhead to create a thread accessor (i.e. rawFile) at any time from this
            Console.WriteLine("INFO: opening " + path);

            // raw file (sadly the threaded access doesn't seem to work for all platforms)
            // var threadManager = RawFileReaderFactory.CreateThreadManager(path);
            var rawFile = RawFileReaderAdapter.FileFactory(path);

            // post-load safety checks
            if (rawFile.IsError)
            {
                throw new Exception($"Encountered error ({rawFile.FileError}) when opening {path}");
            }
            if (rawFile.InAcquisition)
            {
                throw new InvalidOperationException($"RAW file is still being aquired, reading this is not yet supported: {path}");
            }

            // get the first and last scan from the RAW file
            rawFile.SelectInstrument(Device.MS, 1);
            int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
            int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;
            int nScans = lastScanNumber - firstScanNumber + 1;

            // make sure output folder exists
            Directory.CreateDirectory(output);

            // FILEINFO ========
            if (!skipFileInfo)
            {
                // read file info (without threading the async part is a bit pointless)
                try
                {
                    var stopwatch = Stopwatch.StartNew();
                    Console.WriteLine($"\nINFO: reading file info ...");
                    var fileInfoTable = ReadFileInfo(rawFile);
                    Console.WriteLine($"INFO: writing file info to file_info.parquet ...");
                    await ParquetRawWriter.WriteFileInfo(fileInfoTable, Path.Combine(output, "file_info.parquet"));
                    stopwatch.Stop();
                    Console.WriteLine($"INFO: reading+writing file info complete in {stopwatch.ElapsedMilliseconds} ms");
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"ERROR: reading/writing file info failed: {ex.Message}");
                }
            }
            else
            {
                Console.WriteLine($"\nINFO: skipping file info.");
            }

            // SCANS ========
            if (!skipScans)
            {
                // read scans (without threading the async part is a bit pointless)
                try
                {
                    var stopwatch = Stopwatch.StartNew();
                    Console.WriteLine($"\nINFO: reading {nScans} scans ...");
                    var scansTable = ReadScans(rawFile, firstScanNumber, lastScanNumber);
                    Console.WriteLine($"INFO: writing peaks to peaks.parquet ...");
                    await ParquetRawWriter.WriteScans(scansTable, Path.Combine(output, "scans.parquet"));
                    stopwatch.Stop();
                    Console.WriteLine($"INFO: reading+writing scans complete in {stopwatch.ElapsedMilliseconds} ms");
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"ERROR: reading/writing scans failed: {ex.Message}");
                }
            }
            else
            {
                Console.WriteLine($"\nINFO: skipping scans.");
            }

            // PEAKS ========
            if (!skipPeaks)
            {
                // read peaks (without threading the async part is a bit pointless)
                try
                {
                    var stopwatch = Stopwatch.StartNew();
                    Console.WriteLine($"\nINFO: reading peaks from {nScans} scans ...");
                    var peaksTable = ReadPeaks(rawFile, firstScanNumber, lastScanNumber);
                    Console.WriteLine($"INFO: writing peaks to peaks.parquet ...");
                    await ParquetRawWriter.WritePeaks(peaksTable, Path.Combine(output, "peaks.parquet"));
                    stopwatch.Stop();
                    Console.WriteLine($"INFO: reading+writing peaks complete in {stopwatch.ElapsedMilliseconds} ms");
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"ERROR: reading/writing peaks failed: {ex.Message}");
                }
            }
            else
            {
                Console.WriteLine($"\nINFO: skipping peaks.");
            }

            // SPECTRA ========
            if (allSpectra)
            {
                // read all spectra
                spectra = Enumerable.Range(firstScanNumber, nScans).ToArray();
            }
            else
            {
                // read the provided spectra as long as they are in range
                spectra = spectra.Where(n => n >= firstScanNumber && n <= lastScanNumber).ToArray();
            }
            if (spectra.Length > 0)
            {
                try
                {
                    // read spectra (without threading the async part is a bit pointless)
                    var stopwatch = Stopwatch.StartNew();
                    Console.WriteLine($"\nINFO: reading spectra from {spectra.Length} scans ...");
                    var spectraTable = ReadSpectra(rawFile, spectra);
                    Console.WriteLine($"INFO: writing spectra to spectra.parquet ...");
                    await ParquetRawWriter.WriteSpectra(spectraTable, Path.Combine(output, "spectra.parquet"));
                    stopwatch.Stop();
                    Console.WriteLine($"INFO: reading+writing spectra complete in {stopwatch.ElapsedMilliseconds} ms");

                }
                catch (Exception ex)
                {
                    Console.WriteLine($"ERROR: reading/writing spectra failed: {ex.Message}");
                }
            }
            else
            {
                Console.WriteLine($"\nINFO: not reading any spectra.");
            }

            // close the RAW file
            Console.WriteLine("\nINFO: closing raw file");
            rawFile.Dispose();
        }

        /// <summary>
        /// Read the scans in the RAW file.
        /// </summary>
        /// <param name="rawFile">
        /// The raw file.
        /// </param>
        /// <param name="firstScanNumber">
        /// The first scan number.
        /// </param>
        /// <param name="lastScanNumber">
        /// The last scan number.
        /// </param>
        private static ParquetRawWriter.FileInfoTable ReadFileInfo(IRawDataPlus rawFile)
        {
            // initialize the fields and data
            var fields = new List<DataField>();
            var data = new Dictionary<string, object?>();

            // add info
            void addInfo(DataField field, object? value)
            {
                fields.Add(field);
                data.Add(field.Name, value);
            }

            // basice info
            addInfo(new DataField<string>("FileName"), Path.GetFileName(rawFile.FileName));
            addInfo(new DataField<DateTime>("CreationDate"), rawFile.CreationDate);

            // headers / extended headers
            addInfo(new DataField<string>("Operator"), rawFile.FileHeader.WhoCreatedId);
            addInfo(new DataField<string>("FileDescription"), rawFile.FileHeader.FileDescription);
            addInfo(new DataField<double>("MassResolution"), rawFile.RunHeaderEx.MassResolution);
            addInfo(new DataField<int>("SpectraCount"), rawFile.RunHeaderEx.SpectraCount);
            addInfo(new DataField<int>("FirstSpectrum"), rawFile.RunHeaderEx.FirstSpectrum);
            addInfo(new DataField<int>("LastSpectrum"), rawFile.RunHeaderEx.LastSpectrum);
            addInfo(new DataField<double>("StartTime"), rawFile.RunHeaderEx.StartTime);
            addInfo(new DataField<double>("EndTime"), rawFile.RunHeaderEx.EndTime);
            addInfo(new DataField<double>("LowMass"), rawFile.RunHeaderEx.LowMass);
            addInfo(new DataField<double>("HighMass"), rawFile.RunHeaderEx.HighMass);

            // instrument data
            addInfo(new DataField<int>("InstrumentCount"), rawFile.InstrumentCount);
            addInfo(new DataField<string>("InstrumentModel"), rawFile.GetInstrumentData().Model);
            addInfo(new DataField<string>("InstrumentName"), rawFile.GetInstrumentData().Name);
            addInfo(new DataField<string>("SerialNumber"), rawFile.GetInstrumentData().SerialNumber);
            addInfo(new DataField<string>("SoftwareVersion"), rawFile.GetInstrumentData().SoftwareVersion);
            addInfo(new DataField<string>("HardwareVersion"), rawFile.GetInstrumentData().HardwareVersion);
            addInfo(new DataField<int>("RawFileVersion"), rawFile.FileHeader.Revision);
            addInfo(new DataField<string>("InstrumentUnits"), rawFile.GetInstrumentData().Units.ToString());

            // do we need/want filter information?

            // sample information
            addInfo(new DataField<string>("Comment"), rawFile.SampleInformation.Comment);
            addInfo(new DataField<string>("SampleId"), rawFile.SampleInformation.SampleId);
            addInfo(new DataField<string>("SampleName"), rawFile.SampleInformation.SampleName);
            addInfo(new DataField<string>("SampleType"), rawFile.SampleInformation.SampleType.ToString());
            addInfo(new DataField<double>("SampleWeight"), rawFile.SampleInformation.SampleWeight);
            addInfo(new DataField<double>("SampleVolume"), rawFile.SampleInformation.SampleVolume);
            addInfo(new DataField<string>("Barcode"), rawFile.SampleInformation.Barcode);
            addInfo(new DataField<int>("RowNumber"), rawFile.SampleInformation.RowNumber);
            addInfo(new DataField<string>("Vial"), rawFile.SampleInformation.Vial);
            addInfo(new DataField<double>("InjectionVolume"), rawFile.SampleInformation.InjectionVolume);
            addInfo(new DataField<double>("DilutionFactor"), rawFile.SampleInformation.DilutionFactor);
            addInfo(new DataField<double>("IstdAmount"), rawFile.SampleInformation.IstdAmount);
            addInfo(new DataField<string>("CalibrationLevel"), rawFile.SampleInformation.CalibrationLevel);
            addInfo(new DataField<string>("InstrumentMethodFile"), rawFile.SampleInformation.InstrumentMethodFile);
            addInfo(new DataField<string>("CalibrationFile"), rawFile.SampleInformation.CalibrationFile);
            addInfo(new DataField<string>("ProcessingMethodFile"), rawFile.SampleInformation.ProcessingMethodFile);
            addInfo(new DataField<string>("UserText0"), rawFile.SampleInformation.UserText[0]);
            addInfo(new DataField<string>("UserText1"), rawFile.SampleInformation.UserText[1]);
            addInfo(new DataField<string>("UserText2"), rawFile.SampleInformation.UserText[2]);
            addInfo(new DataField<string>("UserText3"), rawFile.SampleInformation.UserText[3]);
            addInfo(new DataField<string>("UserText4"), rawFile.SampleInformation.UserText[4]);

            // file structure
            var dataList = new List<Dictionary<string, object?>>();
            dataList.Add(data);
            return new ParquetRawWriter.FileInfoTable(fields.ToList(), dataList);
        }

        /// <summary>
        /// Read the scans in the RAW file.
        /// </summary>
        /// <param name="rawFile">
        /// The raw file.
        /// </param>
        /// <param name="firstScanNumber">
        /// The first scan number.
        /// </param>
        /// <param name="lastScanNumber">
        /// The last scan number.
        /// </param>
        private static ParquetRawWriter.ScansTable ReadScans(IRawDataPlus rawFile, int firstScanNumber, int lastScanNumber)
        {

            // initialize the data fields dictionary
            var fields = new Dictionary<string, DataField>();
            fields.Add("scan.no", new DataField<int>("scan.no"));
            fields.Add("BasePeakIntensity", new DataField<double>("BasePeakIntensity"));
            fields.Add("BasePeakMass", new DataField<double>("BasePeakMass"));
            fields.Add("HighMass", new DataField<double>("HighMass"));
            fields.Add("IsCentroidScan", new DataField<bool>("IsCentroidScan"));
            fields.Add("LowMass", new DataField<double>("LowMass"));
            fields.Add("TIC", new DataField<double>("TIC"));
            fields.Add("StartTime", new DataField<double>("StartTime"));
            fields.Add("ScanType", new DataField<string>("ScanType"));

            // initialize the data dictonary
            var data = new List<Dictionary<string, object?>>();

            // loop trough the scans
            foreach (int scanNumber in Enumerable.Range(firstScanNumber, lastScanNumber))
            {
                try
                {
                    // load the scan data from the RAW file
                    var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                    var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
                    var scanData = new Dictionary<string, object?>();

                    // basic can information ======
                    scanData.Add("scan.no", scanNumber);

                    // scan statistics =======
                    scanData.Add("BasePeakIntensity", scanStatistics.BasePeakIntensity);
                    scanData.Add("BasePeakMass", scanStatistics.BasePeakMass);
                    scanData.Add("HighMass", scanStatistics.HighMass);
                    scanData.Add("IsCentroidScan", scanStatistics.IsCentroidScan);
                    scanData.Add("LowMass", scanStatistics.LowMass);
                    scanData.Add("TIC", scanStatistics.TIC);
                    scanData.Add("StartTime", scanStatistics.StartTime);
                    scanData.Add("ScanType", scanStatistics.ScanType);

                    // scan trailer information ======
                    foreach (var (key, value) in Enumerable.Range(0, scanTrailer.Length).Select(i => (scanTrailer.Labels[i].TrimEnd(':'), scanTrailer.Values[i].Trim())))
                    {
                        // debug Console.WriteLine($"{key} = '{value.Trim()}'");

                        if (!fields.ContainsKey(key))
                        {
                            // add the new field: these are always string
                            fields.Add(key, new DataField<string>(key));
                        }
                        // add new value
                        scanData.Add(key, value);
                    }
                    // add to data
                    data.Add(scanData);
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"ERROR: could not read spectrum for {scanNumber}: {ex.Message}");
                }
            }

            // scans structure
            return new ParquetRawWriter.ScansTable(fields.Values.ToList(), data);
        }

        /// <summary>
        /// Read centroid peaks in the raw file.
        /// </summary>
        private static ParquetRawWriter.PeaksTable ReadPeaks(IRawDataPlus rawFile, int firstScanNumber, int lastScanNumber)
        {
            // data (more structured than file info + scans, can be written more efficiently to parquet)
            var scans = new List<int>();
            var masses = new List<double>();
            var intensities = new List<double>();
            var resolutions = new List<double>();
            var baselines = new List<double>();
            var noises = new List<double>();
            var isRefs = new List<bool>();
            var isLockPeaks = new List<bool>();

            // loop through scans
            foreach (int scanNumber in Enumerable.Range(firstScanNumber, lastScanNumber))
            {
                try
                {
                    // get the centroids (peaks) from the RAW file for this scan
                    // to get ref/lock peaks, have to specify includeReferenceAndExceptionPeaks = true
                    var centroidStream = rawFile.GetCentroidStream(scanNumber, includeReferenceAndExceptionPeaks: true);
                    for (int i = 0; i < centroidStream.Length; i++)
                    {
                        // pull out all peaks that have either no flags or the reference flag or the lock peak flag
                        if (centroidStream.Flags[i] == PeakOptions.None || (centroidStream.Flags[i] & PeakOptions.Reference) != 0 || (centroidStream.Flags[i] & PeakOptions.LockPeak) != 0)
                        {
                            // note: could also include charges[i] but not clear this provides useful additional information
                            scans.Add(scanNumber);
                            masses.Add(centroidStream.Masses[i]);
                            intensities.Add(centroidStream.Intensities[i]);
                            resolutions.Add(centroidStream.Resolutions[i]);
                            baselines.Add(centroidStream.Baselines[i]);
                            noises.Add(centroidStream.Noises[i]);
                            isRefs.Add((centroidStream.Flags[i] & PeakOptions.Reference) != 0);
                            isLockPeaks.Add((centroidStream.Flags[i] & PeakOptions.LockPeak) != 0);
                        }
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"ERROR: could not read peaks for scan #{scanNumber}: {ex.Message}");
                }
            }
            return new ParquetRawWriter.PeaksTable(scans.ToArray(), masses.ToArray(), intensities.ToArray(), resolutions.ToArray(), baselines.ToArray(), noises.ToArray(), isRefs.ToArray(), isLockPeaks.ToArray());
        }

        /// <summary>
        /// Read spectrum from the raw file.
        /// </summary>
        private static ParquetRawWriter.SpectraTable ReadSpectra(IRawDataPlus rawFile, int[] scanNumbers)
        {

            // data
            var scans = new List<int>();
            var masses = new List<double>();
            var intensities = new List<double>();

            // loop through scans
            var segmentedScan = new SegmentedScan();
            foreach (int scanNumber in scanNumbers)
            {
                try
                {
                    segmentedScan = rawFile.GetSegmentedScanFromScanNumber(scanNumber, null) ?? new SegmentedScan();
                    if (segmentedScan.PositionCount > 0)
                    {
                        scans.AddRange(Enumerable.Repeat(scanNumber, segmentedScan.PositionCount).ToArray());
                        masses.AddRange(segmentedScan.Positions);
                        intensities.AddRange(segmentedScan.Intensities);
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"ERROR: could not read spectra for scan #{scanNumber}: {ex.Message}");
                }
            }


            // spectra structure
            return new ParquetRawWriter.SpectraTable(scans.ToArray(), masses.ToArray(), intensities.ToArray());
        }

    }

    /// <summary>
    /// Parquet writer for interfacing with R/Python.
    /// </summary>
    public static class ParquetRawWriter
    {

        /// <summary>
        /// Structure of the file info table (dyamic)
        /// </summary>
        public record FileInfoTable
        (
            List<DataField> fields,
            List<Dictionary<string, object?>> data
        );

        public static async Task WriteFileInfo(FileInfoTable fileInfo, string path)
        {
            var schema = new ParquetSchema(fileInfo.fields.ToArray());
            using (Stream fs = System.IO.File.OpenWrite(path))
            {
                await ParquetSerializer.SerializeAsync(schema, fileInfo.data, fs);
            }
        }

        /// <summary>
        /// Structure of the scans table (dyamic)
        /// </summary>
        public record ScansTable
        (
            List<DataField> fields,
            List<Dictionary<string, object?>> data
        );

        public static async Task WriteScans(ScansTable scans, string path)
        {
            var schema = new ParquetSchema(scans.fields.ToArray());
            using (Stream fs = System.IO.File.OpenWrite(path))
            {
                await ParquetSerializer.SerializeAsync(schema, scans.data, fs);
            }
        }

        /// <summary>
        /// Structure of the peaks table.
        /// </summary>
        public record PeaksTable
        (
            int[] scans,
            double[] masses,
            double[] intensities,
            double[] resolutions,
            double[] baselines,
            double[] noises,
            bool[] isRefs,
            bool[] isLockPeaks
        );

        public static async Task WritePeaks(PeaksTable peaks, string path)
        {
            // define parquet schema
            var schema = new ParquetSchema(
                new DataField<int>("scan.no"),
                new DataField<double>("mass"),
                new DataField<double>("intensity"),
                new DataField<double>("resolution"),
                new DataField<double>("baseline"),
                new DataField<double>("noise"),
                new DataField<bool>("is_ref"),
                new DataField<bool>("is_lock_peak")
            );

            // write file
            using (Stream fs = System.IO.File.OpenWrite(path))
            {
                using (ParquetWriter writer = await ParquetWriter.CreateAsync(schema, fs))
                {
                    using (ParquetRowGroupWriter groupWriter = writer.CreateRowGroup())
                    {

                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[0], peaks.scans)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[1], peaks.masses)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[2], peaks.intensities)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[3], peaks.resolutions)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[4], peaks.baselines)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[5], peaks.noises)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[6], peaks.isRefs)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[7], peaks.isLockPeaks)
                        );
                    }
                }
            }
        }

        /// <summary>
        /// Structure of spectra table.
        /// </summary>
        public record SpectraTable
        (
            int[] scans,
            double[] masses,
            double[] intensities
        );

        public static async Task WriteSpectra(SpectraTable spectra, string path)
        {
            // define parquet schema
            var schema = new ParquetSchema(
                new DataField<int>("scan.no"),
                new DataField<double>("mass"),
                new DataField<double>("intensity")
            );

            // write file
            using (Stream fs = System.IO.File.OpenWrite(path))
            {
                using (ParquetWriter writer = await ParquetWriter.CreateAsync(schema, fs))
                {
                    using (ParquetRowGroupWriter groupWriter = writer.CreateRowGroup())
                    {

                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[0], spectra.scans)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[1], spectra.masses)
                        );
                        await groupWriter.WriteColumnAsync(
                            new DataColumn(schema.DataFields[2], spectra.intensities)
                        );
                    }
                }
            }
        }
    }

    /// <summary>
    /// Main C# program.
    /// </summary>
    internal static class Program
    {

        /// <summary>
        /// The main routine for this program. This implementation is based on a single-threaded application because the multi-threading does not seem to work on all platforms.
        /// </summary>
        /// <param name="args">The command line arguments for this program. Run the program without arguments or with --help to get the usage help.
        public static async Task Main(string[] args)
        {

            // usage string:
            string? exeName = Path.GetFileName(Process.GetCurrentProcess().MainModule?.FileName);
            string help = $"Usage: {exeName} [--version] [--help] [--file <path>] [--skip <fileInfo,scans,peaks>] [--spectra <all|1,4,6>]";

            // print help
            if (args.Contains("--help"))
            {
                Console.WriteLine(help);
            }

            // print version
            if (args.Contains("--version"))
            {
                var version = Assembly.GetExecutingAssembly().GetName().Version;
                Console.WriteLine($"isoraw version: {version}");
            }

            // do we have a file to process/
            string? path = GetArgumentValue(args, "--file");
            if (path != null)
            {
                // spectra
                string? spectra = GetArgumentValue(args, "--spectra");
                bool allSpectra = spectra != null && spectra == "all";
                int[] scanSpectra = (spectra == null || allSpectra) ? new int[0] :
                    spectra.Split(',', StringSplitOptions.RemoveEmptyEntries).Select(s => int.Parse(s.Trim())).ToArray();
                string spectraInfo = (allSpectra) ? "all" : ((scanSpectra.Length > 0) ? $"{scanSpectra.Length}" : "no");

                // skips
                string? skip = GetArgumentValue(args, "--skip");
                string[] skips = (skip == null) ? new string[0] :
                    skip.Split(',', StringSplitOptions.RemoveEmptyEntries).Select(s => s.Trim()).ToArray();
                bool skipFileInfo = false, skipScans = false, skipPeaks = false;
                string skipInfo = "";
                foreach (string s in skips)
                {
                    switch (s)
                    {
                        case "fileInfo":
                        case "fileinfo":
                            skipFileInfo = true;
                            skipInfo += ", file info";
                            break;
                        case "scans":
                            skipScans = true;
                            skipInfo += ", scans";
                            break;
                        case "peaks":
                            skipPeaks = true;
                            skipInfo += ", peaks";
                            break;
                        default:
                            Console.WriteLine($"Warning: unexpected skip value '{s}'");
                            break;
                    }
                }
                if (!string.IsNullOrEmpty(skipInfo) && skipInfo.Length > 0)
                {
                    skipInfo = " and skipping " + skipInfo.Substring(1);
                }

                // read file
                Console.WriteLine($"\n---- READING {path} (+{spectraInfo} spectra){skipInfo} ----");
                try
                {
                    string output = path + ".cache";
                    await RawReader.ReadFile(path, output, skipFileInfo: skipFileInfo, skipScans: skipScans, skipPeaks: skipPeaks, allSpectra: allSpectra, spectra: scanSpectra);
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error: " + ex.Message);
                }
                Console.WriteLine($"---- FINISHED ----");
            }

            // nothing supplied
            if (!args.Contains("--help") && !args.Contains("--version") && path == null)
            {
                // print the help
                Console.WriteLine(help);
            }
        }

        /// <summary>
        /// Get the value of an argument (or default if it's not provided)
        /// </summary>
        static string? GetArgumentValue(string[] args, string arg, string? defaultValue = null)
        {
            for (int i = 0; i < args.Length; i++)
            {
                if (args[i] == arg && i + 1 < args.Length)
                {
                    return args[i + 1];
                }
            }
            return defaultValue;
        }

    }
}