# GRIB1-to-GRIB2 Distributed Conversion Output

This README explains how to interpret the files produced by a `dist-grib1-to-grib2` run.

It does **not** describe how to generate input lists, configure the tool, or submit the job.

---

## Run directory layout

A completed run typically contains:

```text
<RUNDIR>/
├── input/
│   ├── <list-file-basename>
│   └── optionfile.yaml
├── mtg2-convert-<timestamp>-<pid>.sbatch
├── Summary.log
├── Summary.json
├── output/
└── logging/
```

## `input/`

The `input/` directory contains the staged inputs used by the submitted job:

```text
<RUNDIR>/input/<list-file-basename>
<RUNDIR>/input/optionfile.yaml
```

These are copies of the original list file and option file. They are the files actually passed to the batch job.

This is useful for reproducibility: after the run, this directory shows which input list and which configuration were used.

---

## Generated batch script

The wrapper stores the generated SLURM batch script in the run directory:

```text
<RUNDIR>/mtg2-convert-<timestamp>-<pid>.sbatch
```

This is the exact script submitted with `sbatch`.

It can be inspected to check:

* the SLURM options used;
* the executable path;
* the staged list file passed to the executable;
* the staged option file passed to the executable;
* the run directory passed to the executable.

---

## `output/`

The `output/` directory contains GRIB files **only if the tool is configured to write to files**.

In file-output mode, the distributed workflow typically writes one GRIB output file per MPI rank.

If the tool is configured to archive directly to FDB, the `output/` directory may be empty. In that case, this is not necessarily an error: the produced data is written to FDB instead of local GRIB files.

---

## Top-level summary files

The distributed tool writes two main summary files in the run directory:

```text
<RUNDIR>/Summary.log
<RUNDIR>/Summary.json
```

| File           | Purpose                                           |
| -------------- | ------------------------------------------------- |
| `Summary.log`  | Human-readable per-file summary                   |
| `Summary.json` | Machine-readable JSON version of the same summary |

For a quick manual inspection, start with:

```text
Summary.log
```

For scripting or post-processing, use:

```text
Summary.json
```

---

## `Summary.log` format

Each line in `Summary.log` corresponds to one processed input file.

The format is:

```text
[STATUS], "filename", nMessages=N, nWorkUnits=N, nSuccess=N, nRejected=[N,N], nFail=[N,N,N,N,N,N,N,N,N,N,N]
```

Example:

```text
[SUCCESS], "/path/to/input/file.grib", nMessages=42, nWorkUnits=1, nSuccess=42, nRejected=[0,0], nFail=[0,0,0,0,0,0,0,0,0,0,0]
```

The fields mean:

| Field         | Meaning                                           |
| ------------- | ------------------------------------------------- |
| `STATUS`      | Final status for that input file                  |
| `filename`    | Input file path                                   |
| `nMessages`   | Number of GRIB messages read from that input file |
| `nWorkUnits`  | Number of merged work-unit attempts for that file |
| `nSuccess`    | Number of messages successfully archived          |
| `nRejected`   | Rejection totals per rejection stage              |
| `nFail`       | Failure totals per processing stage               |

The array order is fixed:

* `nRejected=[GribBasedFilter, MarsBasedFilter]`
* `nFail=[OpenFile, ReadMessage, GribBasedFilter, GribToMars, MarsOverrides, MarsToMars, MarsBasedFilter, MarsToGrib, PostEncodeValidation, Grib2Fdb5, FileFlush]`

The `GribBasedFilter` fail slot counts only technical filter failures. Policy-driven filter rejections are reported in `nRejected`.

For files processed across multiple work units, the final status is derived from the merged per-file totals. A file is not considered failed merely because `nWorkUnits` is greater than `1`.

---

## Status meanings

Each input file receives exactly one final status.

| Status        | Meaning                                                                                                                                                                              |
| ------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `SUCCESS`     | All messages in the file were extracted, encoded, and archived successfully                                                                                                          |
| `PARTIAL`     | There were no genuine failures, but at least one message was skipped by policy or classified as copy-required; all other messages were extracted, encoded, and archived successfully |
| `EXTRACTFAIL` | The file contains genuine failures, and all of them are extraction failures                                                                                                          |
| `ENCODEFAIL`  | The file contains genuine failures, and all of them are encoding failures                                                                                                            |
| `ARCHIVEFAIL` | The file contains genuine failures, and all of them are archiving failures                                                                                                           |
| `FAIL`        | The file contains a mix of different genuine failure families                                                                                                                        |

---

## Practical interpretation

### `SUCCESS`

The file was fully processed.

All messages were extracted, encoded, and archived successfully.

No further action is normally required for this file.

---

### `PARTIAL`

The file did not have genuine processing failures, but not every message went through the full extract → encode → archive path.

Typical reasons are:

* a message was skipped by policy;
* a message was classified as copy-required;
* a `CopyRequired*` outcome was produced.

In distributed summaries, `CopyRequired*` outcomes are treated as skip-like outcomes.

A `PARTIAL` file is therefore not necessarily a failure, but it should be checked if full conversion of every message was expected.

---

### `EXTRACTFAIL`

At least one message failed during extraction, and all genuine failures in this file are extraction failures.

This usually means the tool failed while reading or interpreting information from the input GRIB file.

Start by checking the corresponding logs and error views for extraction-related errors.

---

### `ENCODEFAIL`

At least one message failed during encoding, and all genuine failures in this file are encoding failures.

This usually means the tool extracted the message but failed while producing the converted GRIB representation.

Start by checking the encoding error reports and the metadata of the failing messages.

---

### `ARCHIVEFAIL`

At least one message failed during archive/write, and all genuine failures in this file are archiving failures.

This usually means the message was extracted and encoded, but the final write/archive step failed.

Depending on the configuration, this may refer to:

* writing output GRIB files;
* archiving to FDB;
* filesystem or archive availability problems;
* metadata/key issues at archive time.

---

### `FAIL`

The file contains a mix of different genuine failure families.

For example, the same input file may contain both encoding failures and archiving failures.

This status usually requires a more detailed inspection of the counters, logs, and report views.

---

## `Summary.json`

`Summary.json` contains the same final status information as `Summary.log`, but in machine-readable JSON form.

It also includes all available counters for each processed input file.

When possible, it also extracts metadata from the input path, such as:

```text
class
expver
stream
type
levtype
```

Use `Summary.json` when the output needs to be processed automatically, filtered, or aggregated.

---

## `logging/`

The `logging/` directory contains logs, report views, and status-partitioned file lists.

The most useful files for a first inspection are the status lists:

```text
<RUNDIR>/logging/success_list.txt
<RUNDIR>/logging/partial_list.txt
<RUNDIR>/logging/extractfail_list.txt
<RUNDIR>/logging/encodefail_list.txt
<RUNDIR>/logging/archivefail_list.txt
<RUNDIR>/logging/fail_list.txt
```

Each input file appears in exactly one of these six files.

These lists are useful for quickly selecting subsets of the original input files.

For example:

```bash
cat <RUNDIR>/logging/fail_list.txt
```

shows all files classified as `FAIL`.

```bash
cat <RUNDIR>/logging/encodefail_list.txt
```

shows all files whose genuine failures are encoding failures.

---

## Suggested inspection order

For a quick run check:

```bash
ls -lh <RUNDIR>
```

Then inspect the final status distribution:

```bash
wc -l <RUNDIR>/logging/*_list.txt
```

Then inspect failed or partial files:

```bash
cat <RUNDIR>/logging/fail_list.txt
cat <RUNDIR>/logging/extractfail_list.txt
cat <RUNDIR>/logging/encodefail_list.txt
cat <RUNDIR>/logging/archivefail_list.txt
cat <RUNDIR>/logging/partial_list.txt
```

Then inspect the human-readable summary:

```bash
less <RUNDIR>/Summary.log
```

For automated processing, use:

```bash
<RUNDIR>/Summary.json
```

---

## Important notes

An empty `output/` directory is not necessarily an error. If the tool is configured to archive to FDB, converted data is written to FDB rather than to local GRIB files.

`PARTIAL` does not necessarily mean the file failed. It means at least one message did not follow the full normal path, but there were no genuine failures.

`FAIL` means mixed genuine failure families. It is usually the status that requires the most detailed inspection.

Each input file appears exactly once in the status-partitioned lists under `logging/`.
