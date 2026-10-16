# Changelog

All notable changes to pymultio are listed here. The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

The plan models in `multio.plans` now accept what the C++ multio accepts, as it behaves today, and tests keep them so.
The breaking changes below have no deprecation shims.

`from_yaml`, `from_yamlfile` and `from_dict` validate strictly but convert values like C++ does: `{NAME}` placeholders
give strings (never parsed again), and each field converts like the accessor that reads it (`getBool`, `getLong`,
`getDouble`, `util::parseBool`, or the strict parser of `scale`, `encode-mtg2` and `interpolate-mtg2`). So `'true'`,
`'yes'` and `'0'` are booleans where C++ reads them as such, `true` is 1 for an integer, and an integer string such
as `'10KB'` converts like eckit's `Translator<std::string, long long>` (a string without digits is 0). Values that C++
rejects (a float with a fraction for an integer, `'maybe'` for `getBool`) are rejected.

### Changed (breaking)

Actions
- The `aggregation` action is now `aggregate` (`Aggregation` emits `aggregate`), the name C++ registers.
- `encode-mtg2` rejects `mars2grib-options`: the strict C++ parser throws on it before the encoder reads it.
- `scale`, `encode-mtg2` and `interpolate-mtg2` reject `on-error`, for the same reason. Every other action accepts it.
- `encode`: `grid_type` is removed, use `unstructured-grid-type`; `additional_metadata` defaults to `None`, not `{}`.
- `mask`: `missing-value` defaults to the largest float32, not `None`.
- `scale` needs `preset-mappings` or `custom-mappings`, as C++ finds no mapping otherwise.
- `Plan.check_validity()` counts `null` and `debug-sink` as terminal actions, as C++ does.

Sinks
- The `debug-sink`, `trigger` and `socket` sinks are removed: C++ has no such sinks. `Debug`, `Socket` and the old
  `Trigger` sink are gone. `debug-sink` is an action (`DebugSink`); triggers are the `triggers` list of the `sink` action
  and of the `multio` sink.
- `Trigger` is now the base class of the triggers (`MetadataChange`, `NotifyMetadata`); `failOnRetry` is an integer.
- `File.append` is optional and defaults to `false` (it was required).
- `FDB.config` defaults to an empty dictionary (the default was not callable, so omitting it failed).

Plans, clients and servers
- `Server.count` is removed (C++ has no reader). `Server.transport` is required and one of `mpi`, `tcp` or `thread`;
  keys of another transport are rejected, and `tcp` needs `local_port` and `servers`, `thread` needs `clientCount`.
- `Plan.name` is optional (C++ calls an unnamed plan `anonymous`).
- A plan without actions is rejected by C++, so `Plan.validate_for_cpp()` raises `ValueError` for an enabled plan with
  none. It runs when a plan is dumped or converted and in `MultioPlan`. A dissemination plan has no `actions`
  (`DisseminationPlan`). Building a `Plan` and filling it with `add_action` still works.
- A `Client` rejects two enabled plans with the same name.
- `Plan.to_server()` and `Client.to_server()` take the other server keys (`transport`) as keyword arguments, and no
  longer wrap the plans in an extra list.
- `MultioPlan` raises `ValueError` for a plan that C++ would reject.
- `multio.MultioException` lives in `multio.exceptions` (still importable as `multio.MultioException`).

### Added

- Action models: `interpolate-mtg2` (`InterpolateMTG`), `renumber-healpix`, `average-rate`, `debug-sink`
  (`DebugSink`) and `metadata-mapping`.
- Sink models: `multio` (`MultIO`, nested sinks and triggers) and `maestro`; trigger models `MetadataChange` and
  `NotifyMetadata`. Every sink accepts `enable` and `failOnError`.
- `DisseminationPlan`, `MpiCommunicator`, `TcpPeer`, `OnServerErrorOptions` and `PropagateOnErrorOptions`; plans accept
  `enable` and `on-error`; clients and servers accept `on-error`, `active-matchers` and `mpi-communicators`, and
  servers also `on-receive-error`, `on-dispatch-error` and the keys of their transport.
- Keys and values of existing actions that C++ reads: `on-error` (every action but the three above), `select`
  (`ignore`, `any`, `all`, `not`, `enforce-same-key-types`), `statistics` (`options`, `mapping-param`, more operations),
  `statistics-mtg2` (`options`), `transport` (`hash-keys`), `interpolate` (`input`, `grid`, `area`, `cache-path`,
  other MIR keys), `mask` (`offset-fields`), `print` (`mars`, `mars-misc` streams), `encode` (`encoding`, `overwrite`,
  `run`, `unstructured-grid-type`, `grid-downloader-template`), `single-field-sink` (`root_path`). Number lists of
  `interpolate` must be floats (`[1.0, 1.0]`): C++ reads `[1, 1]` as integers and rejects it.
- String fields that C++ reads with `getString` or `getStringVector` convert like eckit (`ConfigStr`): `param-is: 130` is `'130'`, `1.0` is `'1'`, `true` is `'true'`, a list of one element is that element; a map, a null or a longer list is rejected. The strings of the strict parser keep rejecting numbers.
- Each model names the C++ files that read its keys (`cpp_sources`) and the keys it ignores with a reason
  (`cpp_ignore`).

### Fixed

- `Multio.mpi_return_server_comm` called the client function (`multio_mpi_return_client_comm`).
- `Multio.start_server` passed the Python object, not the C configuration pointer.
- Writing a numpy array used `np.dtype.itemsize`, not the size of the array, so the byte count was wrong.
- A stray `print` of the error code in `_Config.__init__` is removed.
- `Metadata` tested `int` before `bool`, so booleans were stored as integers; they now use `set_bool`.
- Metadata integers are passed as `long long` (`processed_multio.h`), no longer truncated to 32 bits.
- `Client.to_server` wrapped the plans in an extra list, which made an invalid server.
- Importing `multio` no longer needs the C library: `multio.plans` and `multio.MultioPlan` work without it, and
  `multio.Multio`, `multio.Metadata`, `multio.lib` and `multio.constants` are loaded on first access, raising an
  `ImportError` that names the cause when the library is missing.

### Tests

- Alignment tests keep the models and C++ in step: a registry test (the action and sink names registered in C++ equal
  the model types), a contract test (every key C++ reads is a model field or listed with a reason, and the types,
  literal defaults and enums agree), a corpus test (every plan file of `tests/` and `share/` validates), and a round
  trip (examples of every model, negative cases and the corpus are built by the real libmultio).
- Tests of the `multio.py` and `metadata.py` fixes, with a mocked library for the MPI and server calls.
