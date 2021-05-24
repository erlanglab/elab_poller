-module(aggregators).

%% API
-export([microstate_accounting_aggregator/4, alocator_sizes_aggregator/4]).

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
    io:format(
        "counters: ~w, type: ~s, system_time: ~w~n~n",
        [maps:to_list(Counters), Type, Time]
    ).

alocator_sizes_aggregator([vm, allocator_sizes], Map, _metadata, _config) ->
    [io:format("key: ~w, value: ~w~n~n", [Key, maps:get(Key, Map)]) || Key <- maps:keys(Map)].


