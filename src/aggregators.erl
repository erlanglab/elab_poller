-module(aggregators).

%% API
-export([microstate_accounting_aggregator/4, allocator_sizes_aggregator/4]).

-spec microstate_accounting_aggregator([atom()], map(), any(), any()) -> ok.
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
            [
                io:format(
                    Fd,
                    "~s,~s,~w,~w~n",
                    [Type, State, Counter, Time]
                )
             || {State, Counter} <- maps:to_list(Counters)
            ],
            file:close(Fd);
        {error, Reason} ->
            io:format("Couldn't open file due to ~w~n", [Reason])
    end.

-spec allocator_sizes_aggregator([atom()], map(), any(), any()) -> ok.
allocator_sizes_aggregator([vm, allocator_sizes], Map, _metadata, _config) ->
    {Time, AllocatorsMap} = maps:take(system_time, Map),
    [parse_values(Key, maps:get(Key, AllocatorsMap), Time) || Key <- maps:keys(AllocatorsMap)].

-spec parse_values(atom(), [{atom(), non_neg_integer(), [tuple()]}], non_neg_integer()) -> ok.
parse_values(_, [], _) ->
    ok;
parse_values(Key, [H | T], Time) ->
    {instance, InstanceNo, InstanceInfo} = H,
    [save_to_file(Key, Time, InstanceNo, Info) || Info <- InstanceInfo],
    parse_values(Key, T, Time).

-spec save_to_file(atom(), non_neg_integer(), non_neg_integer(), {atom(), [tuple()]}) -> ok.
save_to_file(_, _, _, {_, []}) ->
    ok;
save_to_file(Key, Time, InstanceNo, {Bcs, [H | T]}) ->
    % .csv format -> "allocator name","instance number","block carrier","bcs properties",
    % "current size","max size since last call","max size","system time"
    case file:open("allocator_sizes_results.csv", [append]) of
        {ok, Fd} ->
            case tuple_size(H) of
                2 ->
                    {Name, Size} = H,
                    case is_number(Size) of
                        true ->
                            io:format(
                                Fd,
                                "~s,~w,~s,~s,~w,~w,~w,~w~n",
                                [Key, InstanceNo, Bcs, Name, Size, Size, Size, Time]
                            );
                        _ ->
                            io:format(
                                Fd,
                                "~s,~w,~s,~s,-1,-1,-1,~w~n",
                                [Key, InstanceNo, Bcs, Name, Time]
                            )
                    end;
                3 ->
                    {Name, CurrentSize, MaxSize} = H,
                    io:format(
                        Fd,
                        "~s,~w,~s,~s,~w,~w,~w,~w~n",
                        [Key, InstanceNo, Bcs, Name, CurrentSize, MaxSize, MaxSize, Time]
                    );
                4 ->
                    {Name, CurrentSize, MaxSize, MaxSizeEver} = H,
                    io:format(
                        Fd,
                        "~s,~w,~s,~s,~w,~w,~w,~w~n",
                        [Key, InstanceNo, Bcs, Name, CurrentSize, MaxSize, MaxSizeEver, Time]
                    );
                _ ->
                    io:format("Unsupported tuple size: ~w~n", [H])
            end,
            file:close(Fd);
        {error, Reason} ->
            io:format("Couldn't open file due to ~w~n", [Reason])
    end,
    save_to_file(Key, Time, InstanceNo, {Bcs, T}).
