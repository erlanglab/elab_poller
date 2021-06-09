-module(elab_aggregators).

%% API
-export([
    microstate_accounting_aggregator/4,
    allocator_sizes_aggregator/4,
    memory_aggregator/4,
    total_run_queue_lengths_aggregator/4,
    system_counts_aggregator/4
]).

-spec microstate_accounting_aggregator([atom()], map(), map(), term()) -> ok.
microstate_accounting_aggregator(
    [vm, microstate_accounting],
    #{
        counters := Counters,
        type := Type,
        system_time := SysTime
    },
    _metadata,
    _config
) ->
    % .csv format -> "scheduler type","scheduler state","counter","system time"
    case file:open("microstate_accounting_results.csv", [append]) of
        {ok, Fd} ->
            lists:foreach(fun({State, Counter}) ->
                io:format(
                    Fd,
                    "~s,~s,~w,~w~n",
                    [Type, State, Counter, SysTime]) end,
                maps:to_list(Counters)),
            file:close(Fd);
        {error, Reason} ->
            logger:error("Couldn't open file due to ~w~n", [Reason])
    end.

-spec allocator_sizes_aggregator([atom()], map(), map(), term()) -> ok.
allocator_sizes_aggregator([vm, allocator_sizes], Map, _metadata, _config) ->
    {SysTime, AllocatorsMap} = maps:take(system_time, Map),
    lists:foreach(fun(Key) -> parse_values(Key, maps:get(Key, AllocatorsMap), SysTime) end, maps:keys(AllocatorsMap)).

-spec parse_values(atom(), [{atom(), non_neg_integer(), [tuple()]}], non_neg_integer()) -> ok.
parse_values(_, [], _) ->
    ok;
parse_values(Key, [H | T], SysTime) ->
    {instance, InstanceNo, InstanceInfo} = H,
    lists:foreach(fun(Info) -> save_to_file(Key, SysTime, InstanceNo, Info) end, InstanceInfo),
    parse_values(Key, T, SysTime).

-spec save_to_file(atom(), non_neg_integer(), non_neg_integer(), {atom(), [tuple()]}) -> ok.
% .csv format -> "allocator name","instance number","block carrier","bcs properties",
% "current size","max size since last call","max size","system time"
save_to_file(_, _, _, {_, []}) ->
    ok;
save_to_file(_, _, _, {H, _}) when tuple_size(H) > 4 ->
    logger:error("Unsupported tuple size: ~w~n", [H]);
save_to_file(Key, SysTime, InstanceNo, {Bcs, [{Name, Size} | T]}) when is_number(Size) ->
    case file:open("allocator_sizes_results.csv", [append]) of
        {ok, Fd} ->
            io:format(
                Fd,
                "~s,~w,~s,~s,~w,~w,~w,~w~n",
                [Key, InstanceNo, Bcs, Name, Size, Size, Size, SysTime]
            ),
            file:close(Fd);
        {error, Reason} ->
            logger:error("Couldn't open file due to ~w~n", [Reason])
    end,
    save_to_file(Key, SysTime, InstanceNo, {Bcs, T});
save_to_file(Key, SysTime, InstanceNo, {Bcs, [{Name, _} | T]}) ->
    case file:open("allocator_sizes_results.csv", [append]) of
        {ok, Fd} ->
            io:format(
                Fd,
                "~s,~w,~s,~s,-1,-1,-1,~w~n",
                [Key, InstanceNo, Bcs, Name, SysTime]
            ),
            file:close(Fd);
        {error, Reason} ->
            logger:error("Couldn't open file due to ~w~n", [Reason])
    end,
    save_to_file(Key, SysTime, InstanceNo, {Bcs, T});
save_to_file(Key, SysTime, InstanceNo, {Bcs, [{Name, CurrentSize, MaxSize} | T]}) ->
    case file:open("allocator_sizes_results.csv", [append]) of
        {ok, Fd} ->
            io:format(
                Fd,
                "~s,~w,~s,~s,~w,~w,~w,~w~n",
                [Key, InstanceNo, Bcs, Name, CurrentSize, MaxSize, MaxSize, SysTime]
            ),
            file:close(Fd);
        {error, Reason} ->
            logger:error("Couldn't open file due to ~w~n", [Reason])
    end,
    save_to_file(Key, SysTime, InstanceNo, {Bcs, T});
save_to_file(Key, SysTime, InstanceNo, {Bcs, [{Name, CurrentSize, MaxSize, MaxSizeEver} | T]}) ->
    case file:open("allocator_sizes_results.csv", [append]) of
        {ok, Fd} ->
            io:format(
                Fd,
                "~s,~w,~s,~s,~w,~w,~w,~w~n",
                [Key, InstanceNo, Bcs, Name, CurrentSize, MaxSize, MaxSizeEver, SysTime]
            ),
            file:close(Fd);
        {error, Reason} ->
            logger:error("Couldn't open file due to ~w~n", [Reason])
    end,
    save_to_file(Key, SysTime, InstanceNo, {Bcs, T}).

-spec memory_aggregator([atom()], map(), map(), term()) -> ok.
memory_aggregator([vm, memory], Map, _metadata, _config) ->
    SysTime = erlang:system_time(),
    % .csv format -> "type","size","system time"
    case file:open("memory_results.csv", [append]) of
        {ok, Fd} ->
            lists:foreach(fun(Key) -> io:format(Fd, "~s,~w,~w~n", [Key, maps:get(Key, Map), SysTime]) end, maps:keys(Map)),
            file:close(Fd);
        {error, Reason} ->
            logger:error("Couldn't open file due to ~w~n", [Reason])
    end.

-spec total_run_queue_lengths_aggregator([atom()], map(), map(), term()) -> ok.
total_run_queue_lengths_aggregator([vm, total_run_queue_lengths], Map, _metadata, _config) ->
    SysTime = erlang:system_time(),
    % .csv format -> "cpu","io","total","system time"
    case file:open("total_run_queue_lengths_results.csv", [append]) of
        {ok, Fd} ->
            io:format(
                Fd,
                "~w,~w,~w,~w~n",
                [maps:get(cpu, Map), maps:get(io, Map), maps:get(total, Map), SysTime]
            ),
            file:close(Fd);
        {error, Reason} ->
            io:format("Couldn't open file due to ~w~n", [Reason])
    end.

-ifdef(OTP19).
-spec system_counts_aggregator([atom()], map(), map(), term()) -> ok.
system_counts_aggregator([vm, system_counts], Map, _metadata, _config) ->
    SysTime = erlang:system_time(),
    % .csv format -> "process count","port count","system time"
    case file:open("system_counts_results.csv", [append]) of
        {ok, Fd} ->
            io:format(
                Fd,
                "~w,~w,~w~n",
                [maps:get(process_count, Map), maps:get(port_count, Map), SysTime]
            ),
            file:close(Fd);
        {error, Reason} ->
            logger:error("Couldn't open file due to ~w~n", [Reason])
    end.
-else.
-spec system_counts_aggregator([atom()], map(), map(), term()) -> ok.
system_counts_aggregator([vm, system_counts], Map, _metadata, _config) ->
    SysTime = erlang:system_time(),
    % .csv format -> "process count","atom count","port count","system time"
    case file:open("system_counts_results.csv", [append]) of
        {ok, Fd} ->
            io:format(
                Fd,
                "~w,~w,~w,~w~n",
                [
                    maps:get(process_count, Map),
                    maps:get(atom_count, Map),
                    maps:get(port_count, Map),
                    SysTime
                ]
            ),
            file:close(Fd);
        {error, Reason} ->
            logger:error("Couldn't open file due to ~w~n", [Reason])
    end.
-endif.
