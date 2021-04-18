-module(elab_poller).

-export([microstate_accounting/0]).


-spec microstate_accounting() -> ok.
microstate_accounting() ->
  case erlang:statistics(microstate_accounting) of
    undefined -> ok;

    List ->
      [telemetry:execute([vm, microstate_accounting], Map, #{}) || Map <- List]
  end.
