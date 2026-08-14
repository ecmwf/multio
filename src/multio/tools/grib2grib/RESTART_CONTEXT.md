# grib2grib Restart Context

This file is written for agent restart and context recovery.

## Scope and isolation rules

- Work only under `src/multio/tools/grib2grib/`
- Standalone stage modules now live under `src/multio/tools/grib2grib/stages/`
- The folder is self-contained: it may depend only on
  - files inside `src/multio/tools/grib2grib/`
  - external libs: eckit, metkit, eccodes, `multio` (sink), `multio-mars2mars`
  - the `../MultioTool.cc` framework only
- Do not depend on any sibling file under `src/multio/tools/`
- Scalar (non-MPI) tools must never link or inherit MPI
- Do not modify legacy `grib2MarsMisc`
- The parent `src/multio/tools/CMakeLists.txt` is intentionally NOT modified;
  the folder will not build until `add_subdirectory(grib2grib)` is added there
  (deferred on purpose)
- Build the new pipeline stage by stage in isolation

## Build and CMake layout

Local `CMakeLists.txt` defines two libraries and four executables.

- `multio-grib2grib` (MPI-free core library)
  - sources: `../MultioTool.cc`, all stages, `GlobalContext`, `StageOutcomes`,
    `Utils`, `UnitOfWork`, `WorkUnitLoadBalancer`, `Sink`, `Summary`,
    `Process*`, `OptionsUtils.cc`, `CodesHandleToEckitMessage.cc`
  - `PUBLIC_LIBS`: `multio`, `multio-mars2mars`, `eckit`, `metkit`, `eccodes`
  - `CONDITION HAVE_GRIB1_TO_GRIB2`
- `multio-grib2grib-mpi` (MPI library)
  - sources: `MpiUtils.cc`, `OptionsUtilsMpi.cc`, `MultioToolUtils.cc`
  - `PUBLIC_LIBS`: `multio-grib2grib`, `eckit`, `eckit_mpi`
  - `CONDITION HAVE_GRIB1_TO_GRIB2 AND HAVE_MPI`
- Executables:
  - `grib2grib-options-parser` (scalar, `LIBS multio-grib2grib`)
  - `grib2grib-data-distribution-test` (scalar, `LIBS multio-grib2grib`)
  - `grib2grib-sink-test` (scalar, `LIBS multio-grib2grib`)
  - `distributed-grib-to-grib` (`LIBS multio-grib2grib-mpi`, needs `HAVE_MPI`)

The two-library split is required: the MPI-dependent symbol in
`OptionsUtilsMpi.cc` (`loadAndBroadcastOptionsAsConfiguration`) would otherwise
force `eckit_mpi` onto the scalar tools. Keeping it in a separate library lets
the scalar tools stay MPI-free.

## Current stage order

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

## Summary semantics

- `Success`: all messages converted and archived successfully
- `Partial`: some messages intentionally rejected, no technical failures
- `Fail`: any technical failure in any stage, including open and flush

## Implemented modules

### `StageOutcomes`

Files:

- `StageOutcomes.h`
- `StageOutcomes.cc`

Contains:

- stage enums
- counters per stage
- per-file and aggregated outcomes
- summary derivation
- JSON helpers
- serialization and deserialization helpers
- compact text formatting helpers

### `Utils`

Files:

- `Utils.h`
- `Utils.cc`

Contains:

- generic `OptionPolicy`
  - `TryToHandle`
  - `Ignore`
- `toString(OptionPolicy)`
- `parseOptionPolicy(...)`
- `getOptionPolicy(...)`

### `CodesHandleToEckitMessage`

Files:

- `CodesHandleToEckitMessage.h`
- `CodesHandleToEckitMessage.cc`

Contains:

- `to_eckit_message(const metkit::codes::CodesHandle&)`
- created for self-containment (replaces a dependency on a sibling `tools/` file)
- used by `stages/Grib2Fdb5.cc` and by `grib2grib-sink-test.cc`

### `GribBasedFilter`

Files:

- `stages/GribBasedFilter.h`
- `stages/GribBasedFilter.cc`

Current state:

- parsed option struct exists
- options validator exists
- options parser exists
- classifier entry point exists
- stage consumes `const metkit::codes::CodesHandle&` directly
- discipline-192 classifier is implemented
- edition-policy classifiers are implemented
- invalid-message classifier is implemented
- the public entry point calls the stage predicates explicitly in order
- whole stage is wrapped in a single `try/catch`
- catch returns `FailedGribBasedFilter`

### `Overrides`

Files:

- `Overrides.h`
- `Overrides.cc`

Current state:

- stage-local options struct exists
- stage-local result struct exists
- `PackingPolicy` exists with `Ccsds` and `Simple`
- options validator exists
- options parser exists
- stage entry point exists
- flat explicit override callbacks are in place
- implemented callbacks:
  - packing policy override
  - model override
  - generating process identifier override
  - control override
  - expver override
- stage operates only on post-`MarsToMars` `mars` and `misc` dictionaries
- stage does not access the input GRIB message
- `ccsds` is the default packing policy
- packing policy selects one of two explicit frozen maps:
  - `ccsds`: exact mapping from `grib2MarsMisc`
  - `simple`: exact mapping from `grib1-to-grib2`
- explicit field-exclusion rules are deferred until after Monday
- do not wire `MetadataMatcher` into `Overrides` for Monday delivery

## Context model

- stage `Options` have been renamed to stage `Context`
- runtime callback arguments should use `context`, not `options`
- runtime stage entry points should use uniform `run...Stage(...)` naming
- every stage should expose:
  - `validate<Stage>Context(...)`
  - `parse<Stage>Context(...)`
  - `free<Stage>Context(...)`
- `free<Stage>Context(...)` should be `noexcept`
- a dedicated `GlobalContext` should aggregate all stage contexts
- `GlobalContext` should expose:
  - `validateGlobalContext(...)`
  - `parseGlobalContext(...)`
  - `freeGlobalContext(...)`
- `GlobalContext` is config-derived only and must not contain rank-local sink state

### `GribToMars`

Current state target:

- fake stage-local context struct should exist with `verbosity` only
- fake context validator should exist
- fake context parser should exist
- fake context free callback should exist
- stage-local result struct should contain:
  - `mars` as `eckit::LocalConfiguration`
  - `misc` as `eckit::LocalConfiguration`
  - `values` as `std::vector<double>`
- stage consumes `const metkit::codes::CodesHandle&`
- stage calls metkit `grib2mars.convert<eckit::LocalConfiguration>(...)`
- stage extracts values with `inputHandle.getDoubleArray("values")`
- main callback should be `noexcept`
- main runtime callback should be `runGribToMarsStage(...)`
- stage result should contain `outcome`, `mars`, `misc`, and `values`
- source of truth for the metkit call sequence is:
  - `/ec/res4/hpcperm/mavm/ba/multio-bundle/source/metkit/src/tools/grib1-to-grib2.cc`
- stage outcomes should be:
  - `Valid`
  - `MapGribToMarsFailed`
  - `ValuesExtractionFailed`
  - `UnknownFailure`

### `MarsToMars`

Current state target:

- fake stage-local context struct should exist with `verbosity` only
- fake context validator should exist
- fake context parser should exist
- fake context free callback should exist
- stage consumes `mars` and `misc`
- stage calls metkit `mars2mars.convert<eckit::LocalConfiguration>(mars)`
- stage merges returned `misc` with incoming `misc`
- main callback should be `noexcept`
- main runtime callback should be `runMarsToMarsStage(...)`
- stage result should contain `outcome`, `mars`, and `misc`
- effective prototype is `[mars, misc] = mars2marsStage(mars, misc)`
- WMO-unit mapping is always active
- no `wmo-units` switch in the new pipeline
- values are not rescaled in `MarsToMars`
- scale factor and offset stay in `misc`
- stage outcomes should separate mapping failure and misc-merge failure

### `MarsBasedFilter`

Monday target:

- fake stage-local context struct should exist with `verbosity` only
- fake context validator should exist
- fake context parser should exist
- fake context free callback should exist
- stage exists
- main callback should be `noexcept`
- current behavior is accept-all
- real rejection rules will be added after first test feedback

### `MarsToGrib`

Monday target (updated to current state):

- context struct `MarsToGribContext` now contains only:
  - `verbosity`
  - `encoderConfig` as `eckit::LocalConfiguration`
- the encoder (`Mars2Grib`) and testcase generator
  (`Mars2GribTestCaseGenerator`) are no longer stored in the context
- they are built fresh per message inside `runMarsToGribStage(...)`
- `parseMarsToGribContext(...)` no longer opens any file
- `freeMarsToGribContext(...)` is now a no-op (kept for API symmetry)
- main callback should be `noexcept`
- main runtime callback should be `runMarsToGribStage(...)`
- `runMarsToGribStage(values, mars, misc, context, TestCaseFileSink* testCaseSink)`
- stage result should contain `outcome` and encoded `std::unique_ptr<metkit::codes::CodesHandle>`
- `MarsToGribResult` also carries `testCaseGenerationFailed` and `testCaseWriteFailed` flags
- testcase generation runs only when `testCaseSink != nullptr`
  (the old `mars2grib-generate-testcases` gate now lives in `Grib2GribSinks`)
- testcase lines are written via `testCaseSink->write(...)`
- testcase generation and testcase write failures are non-fatal diagnostics
- failed testcase generation and failed testcase write should increment dedicated counters
  (bumped in `ProcessOneMessage`)
- `mars2grib-options` are parsed from YAML into `encoderConfig` and passed to both:
  - `metkit::mars2grib::Mars2Grib`
  - `metkit::mars2grib::Mars2GribTestCaseGenerator`
- supported `mars2grib-options` currently match the existing `encode-mtg2` boolean option surface

### testcase output

- ownership: the testcase file sink is owned by `Grib2GribSinks`, not by the stage context
- enabling is decided in the `Grib2GribSinks` constructor from top-level options:
  - `mars2grib-generate-testcases`
  - `mars2grib-testcases-dir`
- filename helper `testCaseFilePath(...)` lives in an anonymous namespace in `Sink.cc`
- one output file per MPI rank
- filename pattern: `mars2grib-testcases.<MPI_RANK>.json`
- append one testcase JSON string per line

### `PostEncodeValidation`

Current state target:

- fake stage-local context struct should exist with `verbosity` only
- fake context validator should exist
- fake context parser should exist
- fake context free callback should exist
- main callback should be `noexcept`
- main runtime callback should be `runPostEncodeValidationStage(...)`
- stage consumes encoded `const metkit::codes::CodesHandle&`
- stage checks ecCodes `isMessageValid`
- current outcomes are:
  - `Valid`
  - `InvalidEncodedMessage`

### `Grib2Fdb5`

Current state target:

- context struct `Grib2Fdb5Context` contains only:
  - `verbosity`
- the sink writer is no longer part of the context
- fake context validator should exist
- fake context parser should exist
- fake context free callback should exist
- main callback should be `noexcept`
- main runtime callback should be `runGrib2Fdb5Stage(...)`
- `runGrib2Fdb5Stage(encodedHandle, context, multio::sink::DataSink& writer)`
- the writer is passed in as `Grib2GribSinks::mainDataSink()`
- stage consumes encoded `const metkit::codes::CodesHandle&`
- stage writes through `sink::DataSink`
- `Grib2Fdb5.cc` includes `multio/sink/DataSink.h` and uses `to_eckit_message(...)`
- current outcomes are:
  - `Valid`
  - `ArchiveFailed`
  - `UnknownFailure`

## Exception style

- use `eckit` exceptions, not `std` exceptions
- attach `Here()` to thrown `eckit` exceptions
- runtime stage entry points are classifier callbacks and must not throw

## ProcessOneMessage scope for Monday

- `ReadMessage` remains outside `ProcessOneMessage` for now
- `OpenFile` remains outside `ProcessOneMessage` for now
- `Flush` remains outside `ProcessOneMessage` for now
- `ProcessOneMessage` itself should stay flat and explicit
- update stage counters inline at each stage call site
- return immediately after every non-success or non-accepted stage outcome
- `processOneMessage(...)` should keep the rank-local writer as an explicit argument
- `processOneMessage(...)` should have an outer generic safety-net that increments `nGenericProcessOneMessageFailures`

## UnitOfWork

- `WorkUnit` should be the serializable orchestration payload
- `WorkUnit` contains:
  - `filename`
  - `startOffset`
  - `endOffset`
- `UnitOfWork` should store an immutable copy of `WorkUnit`
- `UnitOfWork` should expose:
  - `workUnit()`
  - `theoreticalSize()`
  - `open()`
  - `newMessageAvailable()`
  - `nextMessage()`
  - `close()`
- `open()` should seek to the start offset and advance to the next valid GRIB message
- `nextMessage()` should return `std::unique_ptr<metkit::codes::CodesHandle>` built via copy
- `nextMessage()` should return `nullptr` when no more messages are available inside the unit
- `close()` now returns `bool` so the caller can count close failures

## Unit processing

- `processOneUnitOfWork(...)` should:
  - `open()` the unit
  - loop on `newMessageAvailable()`
  - call `nextMessage()`
  - call `processOneMessage(...)` for each returned handle
  - `close()` the unit
- `processOneUnitOfWork(...)` should classify `open()`, `nextMessage()`, and `flush()` failures into the existing counters
- `processOneUnitOfWork(...)` should wrap `close()` as best-effort cleanup and count close failures through `nCloseFailures`
- diagnostic counters should exist for:
  - `nCloseFailures`
  - `nGenericProcessOneMessageFailures`
  - `nGenericProcessUnitOfWorkFailures`
- `processRankOwnedUnitsOfWork(...)` should process all `WorkUnit`s assigned to the current MPI rank
- `processRankOwnedUnitsOfWork(...)` should continue with the next unit when one unit fails
- `processOneUnitOfWork(...)` should return one `FileStageOutcomes` per processed `UnitOfWork`
- `processRankOwnedUnitsOfWork(...)` should return `std::vector<FileStageOutcomes>`
- `processOneUnitOfWork(...)` is now `noexcept`
- `processOneUnitOfWork(...)` should flush after every `UnitOfWork`

## Load balancing

- load balancing should remain MPI-free
- worker count should be an explicit function argument
- `FileWithSize` should contain:
  - `filename`
  - `totalSizeBytes`
- `WorkBucket` should contain:
  - `workUnits`
  - cached `totalWeightBytes`
- `LoadBalancePlan` should contain:
  - files
  - estimated work units
  - balanced buckets
  - total size
  - size per worker
  - reference work-unit size
  - worker count
  - average work-units per worker
- reference work-unit size should be computed from:
  - total size
  - number of workers
  - average work-units per worker
- Monday split rule:
  - split when file size is strictly larger than the reference work-unit size
  - no tolerance parameter yet
- balancing should use estimated `WorkUnit::theoreticalSize()` only
- bucket assignment should reuse the old greedy min-heap strategy
- public load-balancer API is intentionally small:
  - `createBuckets(...)`
  - `serializeWorkBucket(...)`
  - `deserializeWorkBucket(...)`

## Distribution

- MPI wrapper/helper code should live in `MpiUtils`
- `MpiUtils` public API should only expose:
  - `broadcastOptionsStringFromRoot(...)`
  - `distributeRankOwnedBucket(...)`
  - `gatherOutcomes(...)`
- use `eckit::mpi::Comm`, not raw `MPI_*`
- root should distribute balanced work buckets to ranks
- ranks should receive only their rank-owned bucket payload

## OptionsUtils

- options/context loading code is split into a scalar file and an MPI file:
  - `OptionsUtils.cc` (scalar, MPI-free) holds:
    - `readOptionsFileAsString(...)`
    - `parseOptionsYaml(...)` (plus internal `normalizeOptions(...)`)
  - `OptionsUtilsMpi.cc` (MPI library) holds:
    - `loadAndBroadcastOptionsAsConfiguration(...)`
- this split keeps the MPI symbol out of the scalar core library
- `parseGlobalContext(config)` and `parseMarsToGribContext(config)` no longer take an `mpiRank`
- root should load the options file as a raw string
- raw options string should be broadcast to every rank
- every rank should parse the same raw string into `eckit::LocalConfiguration`
- every rank should then build its own `GlobalContext` from the already parsed local `rawOptions`
- the correct flow is:
  - `rawOptions = loadAndBroadcastOptionsAsConfiguration(...)`
  - local sink built from `rawOptions`
  - `GlobalContext` built from `rawOptions`
- no rank should reopen the options file except rank 0

## Sink

- sink initialization logic should be copied as literally as possible from the old distributed tool
- sink writer is rank-local runtime state
- sink writer must not be part of `GlobalContext`
- sink files now exist:
  - `Sink.h`
  - `Sink.cc`
- free functions:
  - `rankOutputPath(...)`
  - `sinkConfigurationForRank(...)`
  - `buildSink(...)` returns `std::unique_ptr<multio::sink::DataSink>`
- sink initialization should preserve the old behavior bit by bit

### `TestCaseFileSink`

- append-only text file sink for mars2grib testcases
- RAII wrapper: `fopen("a")` / `fwrite` / `fflush` / `fclose`
- throws `eckit::CantOpenFile` on open failure, `eckit::WriteError` on short write
- non-copyable and non-movable
- exposes `write(const std::string&)` and `flush()`
- replaces the old manual `std::FILE*` lifecycle that lived in the `MarsToGrib` context

### `Grib2GribSinks`

- rank-local class owning all sinks for one rank
- members:
  - `std::vector<std::unique_ptr<multio::sink::DataSink>> sinks_` (currently one, built via `buildSink`)
  - `std::unique_ptr<TestCaseFileSink> testCaseSink_` (optional)
- constructor builds the data sink and, when enabled by options, the testcase sink
  - testcase enabling read from top-level `mars2grib-generate-testcases` / `mars2grib-testcases-dir`
- public API:
  - `mainDataSink()` returns `*sinks_[0]` (no magic index leaked to callers)
  - `testCaseSink()` returns `nullptr` when testcase generation is disabled
  - `flush()` flushes the data sink(s) and the testcase sink (separate flush)
- the vector-of-sinks is future preparation; only `sinks_[0]` is exposed today
- has an out-of-line destructor defined in `Sink.cc`, because `unique_ptr<DataSink>`
  sees `DataSink` as an incomplete type in `Sink.h` (only `Sink.cc` / `Grib2Fdb5.cc`
  include `multio/sink/DataSink.h`)
- created in `buildRankLocalWriter(...)`, where the multio sink is initialised, and
  passed down the whole processing chain exactly like the writer used to be

## Summary

- gather should collect `std::vector<FileStageOutcomes>` where each entry corresponds to one processed `UnitOfWork`
- `Summary` should expose `createPerFileOutcomes(...)`
- `createPerFileOutcomes(...)` should internally use a map keyed by filename
- `createPerFileOutcomes(...)` should return `std::vector<FileStageOutcomes>`
- `FileStageOutcomes` should expose `add(const FileStageOutcomes&)`
- `createSummary(...)` and `writeSummary(...)` are still placeholders in the tool-level utils

## Tool shell

- the new distributed tool shell exists in:
  - `distributed-grib-to-grib.cc`
- it derives from `multio::MultioTool`
- command line options are:
  - `--options-file`
  - `--file-list`
  - `--output-directory`
- tool-level orchestration helpers live in:
  - `MultioToolUtils.h`
  - `MultioToolUtils.cc`
- these helpers are in:
  - `namespace multio::grib2grib::utils`
- current tool-level helper set is:
  - `loadAndBroadcastOptionsAsConfiguration(...)`
  - `buildGlobalContext(rawOptions)` (no longer takes a comm)
  - `buildRankLocalWriter(...)` returns `std::unique_ptr<Grib2GribSinks>`
  - `distributeWork(...)`
  - `processWorkUnits(..., Grib2GribSinks& writer)`
  - `gatherWorkUnitOutcome(...)`
  - `summarizeWorkUnitOutcomePerFile(...)`
  - `createSummary(...)`
  - `writeSummary(...)`
- `distributed-grib-to-grib.cc` calls `buildGlobalContext(rawOptions)` and passes `*writer`
  (a `Grib2GribSinks&`) into the processing chain
- the `Process*` chain threads `Grib2GribSinks&` end to end:
  - `ProcessOneMessage` passes `writer.testCaseSink()` to `MarsToGrib` and
    `writer.mainDataSink()` to `Grib2Fdb5`
  - `ProcessOneUnitOfWork` calls `writer.flush()` after every unit

## Scalar distribution test tool

- a new non-MPI scalar tool exists:
  - `grib2grib-data-distribution-test.cc`
- purpose:
  - read file list
  - compute buckets
  - write `work-units.csv`
  - write `distribution-stats.csv`
- command line options are:
  - `--file-list`
  - `--output-directory`
  - `--n-workers`
  - `--average-work-units-per-rank`
- output CSV rows are:
  - `MPI_rank,filename,offsetStart,offsetEnd,size`
- here `MPI_rank` means the synthetic bucket index, not a real MPI rank

## Scalar options-parser tool

- a non-MPI scalar tool exists:
  - `grib2grib_options-parser.cc`
- command line options are:
  - `--options-file`
- flow:
  - `readOptionsFileAsString(...)`
  - `parseOptionsYaml(...)`
  - `validateGlobalContext(...)`
  - `parseGlobalContext(...)`
  - `freeGlobalContext(...)`
- purpose: exercise options loading and full context parsing in isolation

## Scalar sink-test tool

- a throwaway non-MPI scalar isolation harness for `Grib2GribSinks` exists:
  - `grib2grib-sink-test.cc`
- minimal by design; will be deleted later (no summary/log counters)
- command line options are:
  - `--options-file` (must be exactly the distributed tool's options file, loaded identically)
  - `--input-file`
  - `--output-directory`
  - `--rank` (optional, default 0)
- flow:
  - load options via `readOptionsFileAsString(...)` + `parseOptionsYaml(...)`
  - construct `Grib2GribSinks{rawOptions, outputDirectory, rank}`
  - read messages from the input file via `UnitOfWork`
  - write each message to `mainDataSink().write(to_eckit_message(*msg))`
  - write a synthetic line to `testCaseSink()` when it is non-null
  - call `flush()` at the end

## Logging

- `timestampString()` is now available in the new `Utils`
- a shared trapped-error disclaimer helper is also available in `Utils`
- classify-and-continue catches should print the disclaimer with timestamp
- fatal orchestration paths should still fail hard and do not need the disclaimer
- splitting helpers should exist for:
  - filename + number of chunks
  - filename + maximum size in bytes

## Current coarse-grain options

Example options section:

```yaml
coarse-grain-options:
  grib1-messages-policy: try-to-handle
  grib2-messages-policy: ignore
  invalid-messages-policy: try-to-handle
  discipline192-messages-policy: ignore
  verbosity: 0
```

Parsed structure:

- `discipline192Policy`
- `grib1Policy`
- `grib2Policy`
- `invalidMessagesPolicy`
- `verbosity`

Current dedicated option keys:

- `grib1-messages-policy`
- `grib2-messages-policy`
- `invalid-messages-policy`
- `discipline192-messages-policy`
- `verbosity`

## Important design decisions

- The coarse-grain classifier should consume `const metkit::codes::CodesHandle&` directly
- `ProcessOneMessage` should also consume `const metkit::codes::CodesHandle&`
- The message should be decoded into a `CodesHandle` once before stage processing begins
- `GribToMars` should return `mars`, `misc`, and `values`
- `Overrides` consumes only `mars` and `misc` as `eckit::LocalConfiguration`
- `Overrides` returns overridden `mars` and `misc`
- stream conversion is not part of `Overrides`; it belongs to `MarsToMars`

## Discipline-192 decision

The discipline-192 logic should stay readable and explicit.

Private helpers to use in `GribBasedFilter.cc`:

- `isDiscipline192Grib1(const metkit::codes::CodesHandle&)`
- `isDiscipline192Grib2(const metkit::codes::CodesHandle&)`
- `isDiscipline192Message(const metkit::codes::CodesHandle&)`

Current ordered coarse classifiers:

1. `rejectByDiscipline192(...)`
2. `rejectByEditionPolicyGrib1(...)`
3. `rejectByEditionPolicyGrib2(...)`
4. `rejectByInvalidInputMessage(...)`

Behaviour:

- GRIB1: use the lookup-table based `paramId` test
- GRIB2: read `discipline` from the header and compare with `192`

Classifier rule:

- if `discipline192Policy == OptionPolicy::Ignore`
- and the message is discipline 192
- return `RejectedDiscipline192`

Current coarse-grain outcomes:

- `Accepted`
- `RejectedDiscipline192`
- `RejectedGrib1ByEditionPolicy`
- `RejectedGrib2ByEditionPolicy`
- `RejectedInvalidInputMessage`
- `FailedGribBasedFilter`

## Next likely steps after Stage 1

1. implement standalone `GribToMars`
2. implement standalone `MarsToMars`
3. implement standalone `MarsBasedFilter` stub
4. implement standalone `MarsToGrib`
5. implement standalone `PostEncodeValidation`
6. implement standalone `Grib2Fdb5`
7. revisit stage names and options only after standalone stages exist
8. leave `ProcessOneMessage` for final assembly
