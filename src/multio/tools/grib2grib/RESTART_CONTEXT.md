# grib2grib Restart Context

This document is the current architecture and recovery reference for
`src/multio/tools/grib2grib/`.

It describes the code as it exists now. It does not describe historical target
states, intermediate migration steps, or deferred design ideas unless they are
explicitly called out as non-goals.

## Scope

- Work only under `src/multio/tools/grib2grib/`
- Standalone stage modules live under `src/multio/tools/grib2grib/stages/`
- The folder is intentionally self-contained and may depend only on:
  - files inside `src/multio/tools/grib2grib/`
  - external libraries: `eckit`, `metkit`, `eccodes`, `multio`
  - the shared `../MultioTool.cc` framework
- Do not reintroduce dependencies on sibling code under `src/multio/tools/`
- Scalar tools must remain MPI-free
- `grib2grib` owns its own `CodesHandle -> eckit::message::Message` bridge
- `grib2grib` does not modify the legacy `grib2MarsMisc` tooling

## Build Graph

`CMakeLists.txt` defines:

- `multio-grib2grib`
  - MPI-free shared library
  - owns the core pipeline, stage modules, sink utilities, unit reader,
    load balancer, summary utilities, and scalar helper code
- `multio-grib2grib-mpi`
  - thin MPI-dependent shared library
  - owns `OptionsUtilsMpi.cc`, `MpiUtils.cc`, and tool-level orchestration
- executables:
  - `grib2grib-options-parser`
  - `grib2grib-data-distribution-test`
  - `grib2grib-sink-test`
  - `distributed-grib-to-grib`

The two-library split is required because `OptionsUtilsMpi.cc` and `MpiUtils.cc`
would otherwise pull `eckit_mpi` into the scalar tools.

## Pipeline Order

The runtime stage order is fixed:

1. `OpenFile`
2. `ReadMessage`
3. `GribBasedFilter`
4. `GribToMars`
5. `MarsToMars`
6. `MarsOverrides`
7. `MarsBasedFilter`
8. `MarsToGrib`
9. `PostEncodeValidation`
10. `Grib2Fdb5`
11. `FileFlush`

`OpenFile`, `ReadMessage`, and `FileFlush` are handled outside
`ProcessOneMessage(...)`.

## Strict YAML Schema

The runtime configuration is strict. The parser requires top-level `reader` and
`stages` sections. Stage-local keys must live inside the corresponding
`stages.<name>` block.

### Required top-level structure

```yaml
reader:
  mode: eccodes-stream

stages:
  grib-based-filter: {}
  grib-to-mars: {}
  mars-to-mars: {}
  overrides: {}
  mars-based-filter: {}
  mars-to-grib: {}
  post-encode-validation: {}
  grib2fdb5: {}

sink:
  type: file
  path: /path/output.grib2
```

### Optional top-level sections

- `sink`
  - if missing, defaults to a rank-local file sink under
    `output/rank<rank>.grib2`
- `debug-sinks`
  - optional stage-specific best-effort side sinks
  - each entry uses the exact same schema as the top-level `sink`

### Reader section

```yaml
reader:
  mode: eccodes-stream
```

Supported values:

- `eccodes-stream`
- `candidate-boundary`

### Stage sections

#### `stages.grib-based-filter`

```yaml
stages:
  grib-based-filter:
    verbosity: 0
    grib1-messages-policy: try-to-handle
    grib2-messages-policy: try-to-handle
    invalid-messages-policy: ignore
    discipline192-messages-policy: ignore
```

#### `stages.grib-to-mars`

```yaml
stages:
  grib-to-mars:
    verbosity: 0
    api-options:
      saveErrorStack: false
      errorStackPath: "./"
      printErrorStackToStdErr: false
```

`api-options` is optional. If absent, `metkit::grib2mars::Grib2Mars` is
default-constructed. If present, the `LocalConfiguration` constructor is used.

#### `stages.mars-to-mars`

```yaml
stages:
  mars-to-mars:
    verbosity: 0
    api-options:
      saveErrorStack: false
      errorStackPath: "./"
      printErrorStackToStdErr: false
```

`api-options` is optional. If absent, `metkit::mars2mars::Mars2Mars` is
default-constructed. If present, the `LocalConfiguration` constructor is used.

#### `stages.overrides`

```yaml
stages:
  overrides:
    verbosity: 0
    packing: ccsds
    model: ifs
    ncycle: 162
    ensemble-size: 50
    analysis-window-length-in-hours: 6
    control: false
    expver: "2250"
```

All keys are optional.

#### `stages.mars-based-filter`

```yaml
stages:
  mars-based-filter:
    verbosity: 0
    selectors:
      any:
        - stream: ["oper"]
        - type: ["fc"]
```

`selectors` is optional.

If `selectors` is absent:

- every message is accepted by this stage

If `selectors` is present:

- the block is converted into `multio::message::match::MatchReduce`
- the post-override `mars` and `misc` dictionaries are converted into
  `multio::message::Metadata`
- `misc` values overwrite duplicate keys from `mars`
- if the selector matches, the stage returns `Rejected`
- if the selector does not match, the stage returns `Accepted`

This stage uses inverted selector semantics relative to the MultIO `select`
action:

- `select` match means keep
- `mars-based-filter` match means reject

#### `stages.mars-to-grib`

```yaml
stages:
  mars-to-grib:
    verbosity: 0
    generate-testcases: false
    testcases-dir: /path/to/output
    api-options:
      saveErrorStack: false
      errorStackPath: "./"
      printErrorStackToStdErr: false
```

Current stage-local keys:

- `verbosity`
- `generate-testcases`
- `testcases-dir`
- `api-options`

`api-options` is optional. If absent, both:

- `metkit::mars2grib::Mars2Grib`
- `metkit::mars2grib::Mars2GribTestCaseGenerator`

are default-constructed. If present, both use the `LocalConfiguration`
constructor.

`generate-testcases: true` requires `testcases-dir`.

#### `stages.post-encode-validation`

```yaml
stages:
  post-encode-validation:
    verbosity: 0
```

#### `stages.grib2fdb5`

```yaml
stages:
  grib2fdb5:
    verbosity: 0
```

## Reader Modes

`UnitOfWork` supports two reader implementations.

### `eccodes-stream`

- current historical behavior
- seeks to the coarse byte offset
- calls `codes_grib_handle_new_from_file(...)`
- asks ecCodes for the actual message start offset
- copies the decoded message bytes out of the ecCodes handle

### `candidate-boundary`

- explicit candidate-based scanning behavior
- searches for candidate `GRIB` starts only within the owned start range
- validates the full message against physical EOF
- accepts messages whose start is owned, even when the body crosses the unit end
- copies the validated bytes and constructs the handle from memory via
  `codesHandleFromMessageCopy(...)`

### CLI override

The following tools accept `--reader-mode`:

- `distributed-grib-to-grib`
- `grib2grib-sink-test`
- `grib2grib-data-distribution-test`

The CLI override wins over `reader.mode` from YAML.

Supported values are the same as in YAML:

- `eccodes-stream`
- `candidate-boundary`

## GlobalContext

`GlobalContext` is the immutable config-derived runtime bundle for the whole
pipeline.

It contains:

- `ReaderContext reader`
- `GribBasedFilterContext gribBasedFilter`
- `GribToMarsContext gribToMars`
- `MarsToMarsContext marsToMars`
- `OverridesContext overrides`
- `MarsBasedFilterContext marsBasedFilter`
- `MarsToGribContext marsToGrib`
- `PostEncodeValidationContext postEncodeValidation`
- `Grib2Fdb5Context grib2Fdb5`

It does not contain:

- sink runtime state
- open files
- MPI communicator state

`validateGlobalContext(...)` requires top-level `reader` and `stages`.
Missing stage sub-blocks are allowed and are parsed as empty
`eckit::LocalConfiguration{}` blocks.

## Stage Contracts

### `GribBasedFilter`

Input:

- `const metkit::codes::CodesHandle&`

Output:

- `GribBasedFilterCode`

Current outcomes:

- `Accepted`
- `RejectedDiscipline192`
- `RejectedGrib1ByEditionPolicy`
- `RejectedGrib2ByEditionPolicy`
- `RejectedInvalidInputMessage`
- `FailedGribBasedFilter`

### `GribToMars`

Input:

- `const metkit::codes::CodesHandle&`

Output:

- `GribToMarsResult`
  - `outcome`
  - `mars` as `eckit::LocalConfiguration`
  - `misc` as `eckit::LocalConfiguration`
  - `values` as `std::vector<double>`

The stage:

- runs `grib2mars.convert<eckit::LocalConfiguration>(...)`
- extracts data values from `inputHandle.getDoubleArray("values")`

### `MarsToMars`

Input:

- `mars`
- `misc`

Output:

- `MarsToMarsResult`
  - `outcome`
  - mapped `mars`
  - merged `misc`

The stage:

- runs `mars2mars.convert<eckit::LocalConfiguration>(mars)`
- merges returned `misc` with incoming `misc`
- keeps the merged `misc` as stage output

### `Overrides`

Input:

- `mars`
- `misc`

Output:

- `OverrideResult`
  - `outcome`
  - overridden `mars`
  - overridden `misc`

Implemented override families:

- packing policy
- model
- generating process identifier
- ensemble size
- analysis-window length in hours
- control-forecast override
- expver

### `MarsBasedFilter`

Input:

- post-override `mars`
- post-override `misc`

Output:

- `MarsBasedFilterCode`
  - `Accepted`
  - `Rejected`

Selector matching uses `mars + misc` converted to `multio::message::Metadata`.

### `MarsToGrib`

Input:

- `values`
- `mars`
- `misc`
- `MarsToGribContext`
- optional `TestCaseFileSink*`

Output:

- `MarsToGribResult`
  - `outcome`
  - encoded `std::unique_ptr<metkit::codes::CodesHandle>`
  - `testCaseGenerationFailed`
  - `testCaseWriteFailed`

Testcase generation is best-effort and non-fatal.

### `PostEncodeValidation`

Input:

- encoded `const metkit::codes::CodesHandle&`

Output:

- `PostEncodeValidationCode`
  - `Valid`
  - `InvalidEncodedMessage`

### `Grib2Fdb5`

Input:

- encoded `const metkit::codes::CodesHandle&`
- `multio::sink::DataSink&`

Output:

- `Grib2Fdb5Result`
  - `Valid`
  - `ArchiveFailed`
  - `UnknownFailure`

## Sink Model

### Main sink

Top-level `sink` config is the main accepted-output sink.

If `sink` is absent, a file sink is synthesized with rank-local default output
path:

- `output/rank<rank>.grib2`

### `Grib2GribSinks`

`Grib2GribSinks` is the rank-local runtime owner of:

- the main accepted-output sink
- optional per-stage debug sinks
- optional testcase sink

It is constructed once per rank and threaded through the processing chain.

### Testcase sink

When enabled by `stages.mars-to-grib.generate-testcases`,
`Grib2GribSinks` owns one append-only `TestCaseFileSink`.

One file is created per rank:

- `mars2grib-testcases.<MPI_RANK>.json`

### Debug sinks

Top-level `debug-sinks` is optional.

If it is missing:

- no debug sinks are instantiated
- the pipeline behaves exactly as before

If it exists:

- each configured stage entry must be a subconfiguration
- each stage entry uses the exact same schema as `sink`
- file sinks get default path:
  - `debug/<stage-key>/rank<rank>.grib`

Supported stage keys:

- `grib-based-filter`
- `grib-to-mars`
- `mars-to-mars`
- `overrides`
- `mars-based-filter`
- `mars-to-grib`
- `post-encode-validation`
- `grib2fdb5`

Debug sink payload policy:

- every configured debug sink receives the original input GRIB message
- this is true even for late-stage failures such as `grib2fdb5`

Debug sink write policy:

- best-effort only
- write failures are caught internally
- write failures do not change the main outcome classification

## ProcessOneMessage

`processOneMessage(...)` is intentionally flat and explicit.

Behavior:

1. bump message count
2. run `GribBasedFilter`
3. on reject/failure:
   - bump counters
   - best-effort debug-sink original input
   - return
4. run `GribToMars`
5. on failure:
   - bump counters
   - best-effort debug-sink original input
   - return
6. run `MarsToMars`
7. on failure:
   - bump counters
   - best-effort debug-sink original input
   - return
8. run `Overrides`
9. on failure:
   - bump counters
   - best-effort debug-sink original input
   - return
10. run `MarsBasedFilter`
11. on reject:
   - bump counters
   - best-effort debug-sink original input
   - return
12. run `MarsToGrib`
13. on failure:
   - bump counters
   - best-effort debug-sink original input
   - return
14. run `PostEncodeValidation`
15. on failure:
   - bump counters
   - best-effort debug-sink original input
   - return
16. run `Grib2Fdb5`
17. on failure:
   - bump counters
   - best-effort debug-sink original input

An outer safety net increments `nGenericProcessOneMessageFailures` on any
unexpected exception that escapes this logic.

## WorkUnit Ownership Rule

- a `WorkUnit` owns a message iff the message start offset lies in
  `[startOffset, endOffset)`
- ownership is determined by message start offset only
- a claimed message may extend beyond `endOffset`
- a message that starts before `startOffset` is not owned by the unit even if it
  overlaps the unit span

## UnitOfWork

`UnitOfWork` is the runtime iterator bound to one immutable `WorkUnit`.

It stores:

- immutable `WorkUnit`
- immutable `WorkUnitReaderMode`
- open file handle
- physical file end offset
- current cursor offset

API:

- `workUnit()`
- `theoreticalSize()`
- `open()`
- `newMessageAvailable()`
- `nextMessage()`
- `close()`

### `open()`

- opens the file
- computes physical file size
- aligns `currentOffset_` to the first owned GRIB message start according to the
  selected reader mode

### `nextMessage()`

- returns one copied `CodesHandle`
- returns `nullptr` when no further owned message exists

### `close()`

- best-effort resource cleanup
- returns `bool` so close failures can be counted separately

## Candidate boundary helper

`handleGribBoundaries.*` contains an unused/optional helper for explicit
boundary-driven scanning.

Current contract:

- search for message starts only in `[searchOffset, endOffset)`
- validate the full message against `fileEndOffset`
- accept a message that starts inside the owned range even if it ends beyond
  `endOffset`
- reject truncated or invalid candidates and keep scanning

Validation sequence:

1. scan for `GRIB`
2. parse GRIB1 or GRIB2 total length
3. check trailing `7777`
4. validate with ecCodes from memory via `codes_handle_new_from_message_copy(...)`

## Load Balancing

Load balancing is MPI-free and based only on raw work-unit spans.

Key types:

- `WorkUnit`
- `WorkBucket`
- `LoadBalancePlan`

Public API remains intentionally small:

- `createBuckets(...)`
- `serializeWorkBucket(...)`
- `deserializeWorkBucket(...)`

Split rule:

- split a file when its size is strictly larger than the reference work-unit
  size

## MPI Flow

MPI-specific code lives outside the scalar core.

Correct distributed flow:

1. rank 0 reads options file as raw string
2. raw string is broadcast to all ranks
3. every rank parses the same YAML into `rawOptions`
4. every rank builds its own `GlobalContext`
5. rank 0 computes balanced buckets and distributes rank-owned buckets
6. each rank builds its rank-local sinks
7. each rank processes only its local units
8. outcomes are gathered to rank 0
9. rank 0 writes summaries and prints aggregate summary

## Summary Model

### Per-work-unit outcomes

The processing chain returns one `FileStageOutcomes` per processed `UnitOfWork`.

### Per-file outcomes

`Summary.cc` groups work-unit outcomes by filename using `std::map`, so the
result is filename-sorted.

### File summary classification

Current `FileSummary` values:

- `SUCCESS`
- `PARTIAL`
- `FAIL`

High-level meaning:

- `SUCCESS`: all messages from the file converted and archived successfully
- `PARTIAL`: some messages intentionally rejected, but no technical failures
- `FAIL`: technical failures occurred in reading, mapping, encoding, archiving,
  flushing, or generic catch-all buckets

### Output files

Rank 0 writes:

- `Summary.log`
- `Summary.json`

### Aggregate terminal summary

Rank 0 also prints three aggregate lines before exit:

```text
SUCCESS,<numFiles>,<totalMessages>,<percentOfFiles>
PARTIAL,<numFiles>,<totalMessages>,<percentOfFiles>
FAIL,<numFiles>,<totalMessages>,<percentOfFiles>
```

The percentage is file-based, not message-based.

## CLI Tools

### `distributed-grib-to-grib`

Options:

- `--options-file`
- `--file-list`
- `--output-directory`
- `--average-work-units-per-rank`
- `--reader-mode`

### `grib2grib-options-parser`

Options:

- `--options-file`

Purpose:

- parse YAML
- validate `GlobalContext`
- parse `GlobalContext`
- free `GlobalContext`

### `grib2grib-sink-test`

Options:

- `--options-file`
- `--input-file`
- `--output-directory`
- `--rank`
- `--reader-mode`

Purpose:

- scalar harness for `Grib2GribSinks`
- iterates one file through `UnitOfWork`
- writes to main sink and optional testcase sink

### `grib2grib-data-distribution-test`

Options:

- `--file-list`
- `--output-directory`
- `--n-workers`
- `--average-work-units-per-rank`
- `--scan-work-unit-messages`
- `--reader-mode`

Purpose:

- compute synthetic work distribution without MPI
- optionally iterate all generated units through `UnitOfWork`
- dump distribution CSV files

## Exception and failure rules

- runtime stage entry points are `noexcept`
- stage-local failures are converted into explicit outcome codes
- `ProcessOneMessage(...)` and `ProcessOneUnitOfWork(...)` classify and continue
- sink debug writes are best-effort and non-fatal
- testcase generation and testcase write are best-effort diagnostics

## Non-goals

- no fallback support for the old flat YAML schema
- no reuse of the `select` action class itself; only matcher semantics are reused
- no sink-side `MultioConfiguration` integration beyond the currently isolated
  sink construction path
