# statistics-mtg2 Context

This document records the current understanding of `statistics-mtg2`, the design decisions agreed during the timing-metadata work, and the constraints that apply to future changes.

## Working Constraints

- Work must remain in this directory and its subtree unless the user explicitly grants access elsewhere.
- Access to `../../datamod` was explicitly granted for metadata data-model work.
- Do not build or configure the project.
- Do not commit changes.
- Keep changes small, clean, and readable. This action is already complex, so avoid deep nesting and unnecessary abstractions.
- Restart I/O and synoptic functionality are currently low priority.

## Action Architecture

- `Statistics` is the `statistics-mtg2` chained action and main coordinator.
- Fields are grouped by parameter, level, level type, grid/truncation, precision, and source peer.
- Each field owns a `TemporalStatistics` instance containing:
  - an `OperationWindow`;
  - a calendar `PeriodUpdater`;
  - configured statistical operations;
  - retained input metadata.
- Supported operations are `instant`, `average`, `accumulate`, `difference`, `inverse-difference`, `minimum`, `maximum`, and `stddev`.
- Supported statistical output periods are daily and monthly. Hourly `stattype` is intentionally unsupported.
- Synoptic filtering exists as disabled/incomplete code and is not a priority.
- Restart state exists through `fstream` and optional `eckit-codec`, but restart is not currently used and is not a priority.

## Time Concepts

Three intervals have distinct meanings and must not be conflated:

- `integrationStepInSeconds`: solver integration timestep, typically 600 seconds.
- `outputStepInSeconds`: cadence at which the IO server emits fields, typically 3600 seconds.
- `timeIncrementInSeconds`: spacing of the samples used to compute a field that is already statistical.

The metadata keys include the `misc-` prefix:

- `misc-outputStepInSeconds`
- `misc-integrationStepInSeconds`
- `misc-timeIncrementInSeconds`

`step` is an absolute forecast duration. An integer MARS step is interpreted as hours; explicit strings may carry units such as seconds.

Current time must therefore be calculated as:

```text
currentTime = epoch + step.toSeconds()
```

`outputStepInSeconds` is solver/IO-server configuration describing the output cadence. It is preserved as input metadata and is not used to interpret `step`. `timeIncrementInSeconds` must never be used as the generic forecast-step duration.

## Input Classification And Validation

An input is statistical when either `timespan` or `stattype` is present. It is instantaneous when neither is present.

The agreed hard validation rules are:

- Every field requires positive `outputStepInSeconds` and `integrationStepInSeconds`.
- Statistical input requires `timeIncrementInSeconds`.
- Instantaneous input must not contain `timeIncrementInSeconds`.
- `timeIncrementInSeconds`, when present, must be positive.
- `stattype` requires `timespan`.
- Statistical `timespan` must be a finite duration.
- The represented extent of statistical input must be strictly smaller than the requested output window.
- Equal extents are a hard error. For example, a daily average cannot be averaged over the same daily window.
- Larger extents are a hard error. For example, a field with `timespan=744h` cannot feed a daily statistic.
- Squashing does not bypass the strict smaller-than rule.
- `instant` only accepts instantaneous fields. It acts as a time filter and must not be applied to statistical fields.

For nested statistics, the input extent is determined from:

- `timespan` for first-level statistical fields;
- the outer `stattype` duration for fields already carrying `stattype`.

Monthly statistical input cannot currently feed another supported statistics window because there is no larger supported period and a month must not be converted using a fixed number of seconds.

## Output `timeIncrementInSeconds`

The intended invariant is that `timeIncrementInSeconds` describes the spacing of samples consumed by the innermost represented statistical computation.

The output rules are:

- Statistic computed from instantaneous fields: set `timeIncrementInSeconds` to `outputStepInSeconds`.
- Compatible squashed statistic: preserve the input `timeIncrementInSeconds`.
- Non-squashed statistic over a first-level statistic: set it to the input `timespan`, converted to seconds.
- Non-squashed statistic over an input carrying `stattype`: set it to the time extent represented by the input outer `stattype` level.
- `instant` output does not create `timeIncrementInSeconds` because its input must be instantaneous.

The current statistical representation supports:

- no `timespan` and no `stattype`: instantaneous field;
- `timespan` only: first statistical level;
- `timespan` plus one-level `stattype`: second statistical level;
- `timespan` plus two-level `stattype`: third statistical level.

Adding another operation to an existing two-level `stattype` is unsupported.

## Window Semantics

Windows must always use nominal calendar boundaries. Artificial one-second boundary shifts are incorrect and misleading.

Boundary rules are:

- Forward offset: `(start, end]`, using `>` at the lower bound and `<=` at the upper bound.
- Backward offset: `[start, end)`, using `>=` at the lower bound and `<` at the upper bound.

`isWithin()` and `updateData()` must use the same rules.

For a newly created forward-offset window, a message exactly on the nominal lower boundary is skipped as a statistical sample. It may still have initialized operation state during construction, which is needed by baseline-dependent operations such as `difference`.

This lower-bound behavior is structural and must not depend on `initial-condition-present`.

## Initial Condition Option

The active option is:

```yaml
initial-condition-present: true
```

There is no `solver-send-initial-condition` alias in this action.

The option remains in use for now because removing it would be too disruptive. It controls operation initialization and restart duplicate suppression. Duplicate suppression must only apply to an existing/restored temporal statistic; it must not skip the first valid sample of a newly created window.

Future work may remove this option, but that is explicitly deferred.

## Implemented Changes

The current working changes implement the agreed model:

- Added `OutputStepInSeconds` and `IntegrationStepInSeconds` to `datamod/MarsMiscGeo.h`.
- Kept the global legacy default for `TimeIncrementInSeconds` to avoid affecting unrelated data-model consumers.
- Overrode `timeIncrementInSeconds` as optional in the action-local field record so presence can be validated correctly.
- Added `stattype` to the parsed field metadata record.
- Added input classification and metadata validation to `StatisticsConfiguration`.
- Updated current-time calculation to use the absolute `step` duration.
- Removed ambiguous `OperationWindow` conversions from elapsed time to output-step indices.
- Removed the forward-window one-second alignment adjustment.
- Made `OperationWindow::updateData()` honor forward/backward boundary semantics.
- Added the explicit newly-created forward lower-bound skip.
- Restricted `instant` to instantaneous input.
- Added strict statistical-input extent validation.
- Added output `timeIncrementInSeconds` injection according to the rules above.
- Updated emitted `step` values to remain absolute MARS durations, preserving seconds when needed.
- Changed maximum initialization from `std::numeric_limits<T>::min()` to `std::numeric_limits<T>::lowest()` so entirely negative fields are handled correctly.
- Added `distanceFromPreviousStepInSeconds` metadata to describe instantaneous sample distance or statistical field extent.
- Added complete-window detection using declared and observed distance histograms.
- Added `emit-incomplete-statistics`, `allow-non-uniform-statistics`, and window-dynamics `debug` options.
- Incomplete output uses the nominal calendar-window extent; disallowed non-uniform input is a hard error.

No build or configure was run. Verification was limited to static inspection and consistency searches, as required.

## Deferred And Low-Priority Areas

- Restart I/O behavior and restart-format compatibility.
- Synoptic filtering and disabled synoptic sources.
- Hourly or weekly `stattype` support.
- Removal of `initial-condition-present`.
- The `logPrefix_` constructor currently uses the restart-prefix parser. Its effect appears limited to misleading logs and is not high priority.
- Existing `stddev` restart limitations are out of scope while restart is unused.

## Related Producer And Diagnostics Work

Additional changes outside this action were explicitly requested and implemented:

- The Print action supports `stream: mars` for MARS-only output and `stream: mars-misc` for MARS plus miscellaneous metadata.
- Print retains a copy of each message before forwarding it. If a downstream action throws, Print logs the offending message and rethrows the original exception unchanged.
- The `ifs2mars` output-manager hierarchy now includes a `FLUSH_START` operation for MultIO, dump, and noop implementations.
- The MultIO start flush carries `flushKind`, `date`, `time`, and absolute hourly `step`, providing the simulation epoch required by this action.
- Dump files preserve `FLUSH_START` as the new TOC event type 7. Existing event values 0-6 and `TOC_SIM_INIT_T` remain unchanged, so new readers remain compatible with old dumps.
- Shared model-parameter date/time extraction lives in `EXTRACT_DATETIME_FROM_PAR`; `ATM2MARS_SET_DATETIME` reuses it while retaining its existing analysis and timespan error handling.
- `MULTIO_FILL_MARS_METADATA` injects `misc-outputStepInSeconds=3600` and `misc-integrationStepInSeconds=TSTEP` for every field.
- It injects `misc-timeIncrementInSeconds=TSTEP` only when `timespan` is present.
- The previous unconditional parametrization-level `misc-timeIncrementInSeconds` injection was removed.

## Important Review Points

- `integrationStepInSeconds` is required and validated but is not currently used in calculations. This is intentional: this action consumes IO-server outputs, whose cadence is `outputStepInSeconds`.
- `outputStepInSeconds` is currently used to set `timeIncrementInSeconds` when statistics are computed from instantaneous fields. It must remain constant within a statistics window; validation of future dynamic cadence changes is deferred.
- A statistic over precomputed statistics may require operation-specific weighting beyond metadata correctness. Average and standard deviation currently count incoming fields, not necessarily all underlying samples. This was identified as a broader concern but was not included in the near-term metadata/window change.
- `value-count-threshold` counts incoming values. Its interpretation for nested precomputed statistics may need future review.
- Calendar months must never be treated as a fixed number of seconds.
