-module(elab_poller).

-export([microstate_accounting/0, allocator_sizes/0]).

-spec microstate_accounting() -> ok.
microstate_accounting() ->
    case erlang:statistics(microstate_accounting) of
        undefined ->
            ok;
        List ->
            Types = lists:usort([Type || #{type := Type} <- List]),
            AggregatedCountersList = aggregate_counters(List, Types, []),
            lists:foreach(
                fun(Map) ->
                    telemetry:execute(
                        [vm, microstate_accounting],
                        maps:put(system_time, erlang:system_time(), Map),
                        #{}
                    )
                end,
                AggregatedCountersList
            )
    end.

-spec allocator_sizes() -> ok.
allocator_sizes() ->
    Keys = erlang:system_info(alloc_util_allocators),
    SizesList = [{Alloc, erlang:system_info({allocator_sizes, Alloc})} || Alloc <- Keys],
    telemetry:execute(
        [vm, allocator_sizes],
        maps:put(system_time, erlang:system_time(), maps:from_list(SizesList)),
        #{}
    ).

-spec aggregate_counters([map()], [atom()], [map()]) -> [map()].
aggregate_counters(_, [], AggregationList) ->
    AggregationList;
aggregate_counters(Measurements, [H | T], AggregationList) ->
    {Satisfying, NotSatisfying} = lists:partition(
        fun(X) -> maps:get(type, X) == H end,
        Measurements
    ),
    AggregatedSum = lists:foldr(fun aggregate_sum/2, #{}, Satisfying),
    aggregate_counters(
        NotSatisfying,
        T,
        AggregationList ++ [#{counters => AggregatedSum, type => H}]
    ).

-spec aggregate_sum(map(), map()) -> map().
aggregate_sum(Map, AggregatedCounters) when map_size(AggregatedCounters) == 0 ->
    maps:get(counters, Map);
aggregate_sum(Map, AggregatedCounters) ->
    Counters = maps:get(counters, Map),
    maps:map(fun(Key, Value) -> Value + maps:get(Key, Counters) end, AggregatedCounters).
