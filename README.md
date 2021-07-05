elab_poller
=====

An OTP library extending [telemetry_poller](https://github.com/beam-telemetry/telemetry_poller), a tool using [telemetry](https://github.com/beam-telemetry/telemetry) library.
Both dependencies are installed with usage of rebar3.

Build
-----

    $ rebar3 compile

Getting started
---------------
To use elab_poller in your project you have to add telemetry_handler_table to your supervision tree to manage handlers

```erlang
telemetry_handler_table:start_link()
```

When handler table is working you can attach handlers for given events

```erlang
telemetry:attach(
        unique-id,
        [event type, event],
        fun handler_function/4,
        []
    )
```

and start telemetry_poller to gather data periodically

```erlang
telemetry_poller:start_link([{measurements, [{module, event, []}]}])
```

Functionalities
---------------
In `elab_poller` module there are defined two functions for gathering measurements:
* `microstate_accounting/0` that is collecting data from `erlang:statistics(microstate_accounting)` and aggregate them per microstate type
* `allocator_sizes/0` gets data from `erlang:system_info({allocator_sizes, Allocator})` for allocators returned by `erlang:system_info(alloc_util_allocators)`

Module `elab_aggregators` contains handlers for events emited by functions in `elab_poler.erl` - `[vm, microstate_accounting]`, `[vm, allocator_sizes]` and few default measurements provided by telemetry_poller, that is
`[vm, memory]`, `[vm, total_run_queue_lengths]` and `[vm, system_counts]`. 

For now the measurements are parsed and written to .csv file.

Example
-------
Example how you can collect microstate_accounting data:
```erlang
telemetry_handler_table:start_link(),
telemetry:attach(
    <<"logger-mc">>,
    [vm, microstate_accounting],
    fun elab_aggregators:microstate_accounting_aggregator/4,
    []
),
telemetry_poller:start_link([{measurements, [{elab_poller, microstate_accounting, []}]}]).
```

 