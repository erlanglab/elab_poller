-module(elab_poller).

-export([microstate_accounting/0, allocator_sizes/0]).


-spec microstate_accounting() -> ok.
microstate_accounting() ->
  case erlang:statistics(microstate_accounting) of
    undefined -> ok;

    List ->
      [telemetry:execute([vm, microstate_accounting], maps:put(system_time, erlang:system_time(), Map), #{}) || Map <- List]
  end.

-spec allocator_sizes() -> ok.
allocator_sizes() ->
  Keys = erlang:system_info(alloc_util_allocators),
  SizesList = [{Alloc, erlang:system_info({allocator_sizes, Alloc})} || Alloc <- Keys],
  telemetry:execute([vm, allocator_sizes], maps:put(system_time, erlang:system_time(), maps:from_list(SizesList)), #{}).