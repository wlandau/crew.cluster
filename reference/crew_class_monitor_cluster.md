# **\[experimental\]** Abstract cluster monitor class

Abstract cluster monitor `R6` class

## Details

See [`crew_monitor_cluster()`](crew_monitor_cluster.md).

## See also

Other cluster:
[`crew_class_launcher_cluster`](crew_class_launcher_cluster.md),
[`crew_launcher_cluster()`](crew_launcher_cluster.md),
[`crew_monitor_cluster()`](crew_monitor_cluster.md),
[`crew_options_cluster()`](crew_options_cluster.md)

## Active bindings

- `verbose`:

  See [`crew_monitor_cluster()`](crew_monitor_cluster.md).

- `command_list`:

  See [`crew_monitor_cluster()`](crew_monitor_cluster.md).

- `command_terminate`:

  See [`crew_monitor_cluster()`](crew_monitor_cluster.md).

## Methods

### Public methods

- [`crew_class_monitor_cluster$new()`](#method-crew_class_monitor_cluster-new)

- [`crew_class_monitor_cluster$validate()`](#method-crew_class_monitor_cluster-validate)

------------------------------------------------------------------------

### Method `new()`

Abstract cluster monitor constructor.

#### Usage

    crew_class_monitor_cluster$new(
      verbose = NULL,
      command_list = NULL,
      command_terminate = NULL
    )

#### Arguments

- `verbose`:

  See [`crew_monitor_cluster()`](crew_monitor_cluster.md).

- `command_list`:

  See [`crew_monitor_cluster()`](crew_monitor_cluster.md).

- `command_terminate`:

  See [`crew_monitor_cluster()`](crew_monitor_cluster.md).

#### Returns

an abstract cluster monitor object.

------------------------------------------------------------------------

### Method `validate()`

Validate the monitor.

#### Usage

    crew_class_monitor_cluster$validate()

#### Returns

`NULL` (invisibly).
