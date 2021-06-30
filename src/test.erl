-module(test).

%% API
-export([test_ma/0, test_as/0, test_mem/0, test_trql/0, test_sc/0]).

test_ma() ->
  telemetry_handler_table:start_link(),
  telemetry:attach(<<"logger-mc">>, [vm, microstate_accounting], fun elab_aggregators:microstate_accounting_aggregator/4, []),
  telemetry_poller:start_link([{measurements, [{elab_poller, microstate_accounting, []}]}]).

test_as() ->
  telemetry_handler_table:start_link(),
  telemetry:attach(<<"logger-as">>, [vm, allocator_sizes], fun elab_aggregators:allocator_sizes_aggregator/4, []),
  telemetry_poller:start_link([{measurements, [{elab_poller, allocator_sizes, []}]}]).

test_mem() ->
  telemetry_handler_table:start_link(),
  telemetry:attach(<<"logger-mem">>, [vm, memory], fun elab_aggregators:memory_aggregator/4, []),
  telemetry_poller:start_link([{measurements, [memory]}]).

test_trql() ->
  telemetry_handler_table:start_link(),
  telemetry:attach(<<"logger-trql">>, [vm, total_run_queue_lengths], fun elab_aggregators:total_run_queue_lengths_aggregator/4, []),
  telemetry_poller:start_link([{measurements, [total_run_queue_lengths]}]).

test_sc() ->
  telemetry_handler_table:start_link(),
  telemetry:attach(<<"logger-sc">>, [vm, system_counts], fun elab_aggregators:system_counts_aggregator/4, []),
  telemetry_poller:start_link([{measurements, [system_counts]}]).