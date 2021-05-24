-module(aggregators).

%% API
-export([microstate_accounting_aggregator/4, allocator_sizes_aggregator/4]).

microstate_accounting_aggregator(
    [vm, microstate_accounting],
    #{
        counters := Counters,
        type := Type,
        system_time := Time
    },
    _metadata,
    _config
) ->
  % .csv format -> "scheduler type","scheduler state","counter","system time"
    case file:open("microstate_accounting_results.csv", [append]) of
      {ok, Fd} ->
        [io:format(
          Fd,
          "~s,~s,~w,~w~n",
          [Type, State, Counter, Time]
        ) || {State, Counter} <- maps:to_list(Counters)];

      {error, Reason} ->
        io:format("Couldn't open file due to ~w~n", [Reason])
    end.

allocator_sizes_aggregator([vm, allocator_sizes], Map, _metadata, _config) ->
    [io:format("key: ~w, value: ~w~n~n", [Key, maps:get(Key, Map)]) || Key <- maps:keys(Map)].


