-module('lfu').
-author('VSolenkov').

-behaviour('gen_statem').

-export([
    start_link/0
]).
-export([
    point/1,
    cheat/1,
    state/0,
    count/1,
    score/0,
    fetch/1,
    clean/1
]).
-export([
    score/2,
    fetch/2,
    clean/2
]).
-export([
    init/1,
    callback_mode/0
]).
-export([
    common/3,
    offset/3,
    select/3,
    delete/3
]).
-include("include/lfu.hrl").



start_link() ->
    gen_statem:start_link(
        {local,?MODULE},
        ?MODULE,[?MIN_ORDER,0,?ETS_KEYS_TABLE_NAME],
    [{spawn_opt,?SPAWN_OPT_LFU}]).


init([O,_,T]) ->
    Q = restorage(T),
    {ok,common,[O,Q]}.

callback_mode() ->
    state_functions.

point(K) ->
    case lfu_utils:key_validation(K) of
        BK when is_binary(BK) ->
            gen_statem:cast(?MODULE,{point,BK});
        -1 ->
            "type_error";
        -2 ->
            "size_key_error"
    end.
cheat(KVL) ->
    case is_list(KVL) of
        true ->
            VKVL = lists:filtermap(
                fun({K,V}) ->
                    case lfu_utils:key_validation(K) of
                        BK when is_binary(BK) ->
                            {true,{BK,V}};
                        _ ->
                            false
                    end
                end,
            KVL),
            if
                length(VKVL) > 0 ->
                    gen_statem:cast(?MODULE,{cheat,VKVL});
                true ->
                    "data_error"
            end;
        false ->
            "type_error"
    end.
count(K) ->
    case lfu_utils:key_validation(K) of
        BK when is_binary(BK) ->
            gen_statem:call(?MODULE,{count,BK});
        -1 ->
            "type_error";
        -2 ->
            "size_key_error"
    end.
state() ->
    gen_statem:call(?MODULE,state).
score() ->
    gen_statem:call(?MODULE,score).
fetch(T) ->
    gen_statem:call(?MODULE,{fetch,T}).
clean(T) ->
    gen_statem:call(?MODULE,{clean,T}).

score(R,C) ->
    gen_statem:cast(?MODULE,{{score,R},C}).
fetch(R,C) ->
    gen_statem:cast(?MODULE,{{fetch,R},C}).
clean(R,T) ->
    gen_statem:cast(?MODULE,{{clean,R},T}).


common(cast,{point,K},[O,Q]) ->
    case get(K) of
        undefined ->
            N = list_to_atom("o0" ++ integer_to_list(0)),
            case whereis(N) of
                undefined ->
                     lfu_exact_score_sup:start([0,0]),
                     lfu_exact_score:point(N,K);
                _ ->
                     lfu_exact_score:point(N,K)
            end,
            put(K,1),
            ets:insert(?ETS_KEYS_TABLE_NAME,{K,1}),
            ?SUPPORT andalso erlang:apply(?AUXILIARY,point,[K]),
            {keep_state,[O,if ?SCORE_OFFSET == 0 -> Q+1; true -> Q end]};
        C when C < ?MAX_ORDER ->
            if
                %% before MAX LIMIT
                C div ?MAX_LIMIT == 0 ->
                    N = list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT)),
                    case whereis(N) of
                        undefined ->
                            lfu_exact_score_sup:start([C div ?MIN_LIMIT,0]),
                            lfu_exact_score:point(N,K);
                        _ ->
                            lfu_exact_score:point(N,K)
                    end,
                    %% reset
                    if
                        C rem ?MIN_LIMIT == 0 ->
                            NR = list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT - 1)),
                            case whereis(NR) of
                                undefined ->
                                    lfu_exact_score_sup:start([C div ?MIN_LIMIT - 1,0]),
                                    lfu_exact_score:reset(NR,K);
                                _ ->
                                    lfu_exact_score:reset(NR,K)
                              end;
                        true -> skip
                    end;
                %% after MAX LIMIT
                C rem ?MAX_LIMIT == 0 ->
                    N = list_to_atom("o" ++ integer_to_list(C div ?MAX_LIMIT)),
                    case whereis(N) of
                        undefined ->
                            lfu_quick_score_sup:start([C div ?MAX_LIMIT,0]),
                            lfu_quick_score:point(N,K);
                        _ ->
                            lfu_quick_score:point(N,K)
                    end,
                    %% reset
                    if
                        C div ?MAX_LIMIT > 1 ->
                            NR = list_to_atom("o" ++ integer_to_list(C div ?MAX_LIMIT - 1)),
                            case whereis(NR) of
                                undefined ->
                                    lfu_quick_score_sup:start([C div ?MAX_LIMIT - 1,0]),
                                    lfu_quick_score:reset(NR,K);
                                _ ->
                                    lfu_quick_score:reset(NR,K)
                            end;
                        true ->
                            NR = list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT - 1)),
                            case whereis(NR) of
                                undefined ->
                                    lfu_quick_score_sup:start([C div ?MIN_LIMIT - 1,0]),
                                    lfu_exact_score:reset(NR,K);
                                _ ->
                                    lfu_exact_score:reset(NR,K)
                            end
                    end;
                true -> skip
            end,
            put(K,C+1),
            ets:insert(?ETS_KEYS_TABLE_NAME,{K,C+1}),
            ?SUPPORT andalso erlang:apply(?AUXILIARY,point,[K]),
            {keep_state,[O,if (C+1) / (?SCORE_OFFSET+1) == 1 -> Q+1; true -> Q end]};
        _ ->
            ?SUPPORT andalso erlang:apply(?AUXILIARY,point,[K]),
            {keep_state,[O,Q]}
    end;
common(cast,{cheat,KVL},[O,Q]) ->
    NQ = length(lists:filter(
        fun({K,V}) when V =< ?MAX_ORDER ->
            case get(K) of
                undefined -> skip;
                C ->
                    if
                        (C-1) div ?MAX_LIMIT == 0 ->
                            NR = list_to_atom("o0" ++ integer_to_list((C-1) div ?MIN_LIMIT)),
                            case whereis(NR) of
                                undefined ->
                                    lfu_exact_score_sup:start([(C-1) div ?MIN_LIMIT,0]),
                                    lfu_exact_score:reset(NR,K);
                                _ ->
                                    lfu_exact_score:reset(NR,K)
                            end;
                        true ->
                            NR = list_to_atom("o" ++ integer_to_list((C-1) div ?MAX_LIMIT)),
                            case whereis(NR) of
                                undefined ->
                                    lfu_quick_score:start([(C-1) div ?MAX_LIMIT,0]),
                                    lfu_quick_score:reset(NR,K);
                                _ ->
                                    lfu_quick_score:reset(NR,K)
                            end
                    end
            end,
            if
                V > 0 ->
                    if
                        (V-1) div ?MAX_LIMIT == 0 ->
                            lfu_utils:for(0,(V-1) div ?MIN_LIMIT,
                                fun(I) ->
                                    N = list_to_atom("o0" ++ integer_to_list(I)),
                                    case whereis(N) of
                                        undefined ->
                                            lfu_exact_score_sup:start([I,0]);
                                        _ -> skip
                                    end,
                                    if
                                        I == (V-1) div ?MIN_LIMIT ->
                                            lfu_exact_score:cheat(N,K,V);
                                        true -> skip
                                    end
                                end
                            );
                        true ->
                            lfu_utils:for(0,(V-1) div ?MIN_LIMIT,
                                fun(I) ->
                                    N = list_to_atom("o0" ++ integer_to_list(I)),
                                    case whereis(N) of
                                        undefined ->
                                            lfu_exact_score_sup:start([I,0]);
                                        _ -> skip
                                    end
                                end
                            ),
                            lfu_utils:for(1,(V-1) div ?MAX_LIMIT,
                                fun(I) ->
	                            N = list_to_atom("o" ++ integer_to_list(I)),
                                    case whereis(N) of
                                        undefined ->
                                            lfu_quick_score_sup:start([I,0]);
                                        _ -> skip
                                    end,
                                    if
                                        I == (V-1) div ?MAX_LIMIT ->
                                            lfu_quick_score:cheat(N,K,1);
                                        true -> skip
                                    end
                                end
                            )
                     end;
                true -> skip
            end,
            case get(clean) of
                undefined -> put(clean,0);
                _ -> skip
            end,
            case get(K) of
                undefined ->
                    if
                        V > 0 ->
                            put(K,V),
                            ets:insert(?ETS_KEYS_TABLE_NAME,{K,V}),
                            if
                                V > ?SCORE_OFFSET -> true;
                                true -> false
                            end;
                        true -> false
                    end;
                OV ->
                    if
                        V > 0 ->
                            put(K,V),
                            ets:insert(?ETS_KEYS_TABLE_NAME,{K,V}),
                            if
                                OV =< ?SCORE_OFFSET andalso V > ?SCORE_OFFSET -> true;
                                true -> false
                            end;
                        true ->
                            if
                                OV > ?SCORE_OFFSET ->
                                    erase(K),ets:delete(?ETS_KEYS_TABLE_NAME,K),put(clean,get(clean)+1),false;
                                true ->
                                    erase(K),ets:delete(?ETS_KEYS_TABLE_NAME,K),false
                            end
                    end
            end;
        (_) ->
            false
        end,
    KVL))+Q-erase(clean),
    {keep_state,[O,NQ]};
common({call,From},{count,K},[O,Q]) ->
    {keep_state,[O,Q],[{reply,From,get(K)}]};
common({call,From},state,[O,Q]) ->
    {keep_state,[O,Q],[{reply,From,[O,Q]}]};
common({call,From},score,[O,Q]) ->
    {next_state,offset,[O,Q,#{from => From, order => score}],[{next_event,internal,{score,{previous,{?SCORE_OFFSET,O}}}}]};
common({call,From},{fetch,T},[O,Q]) ->
    {next_state,offset,[O,Q,#{from => From, tid => T, order => fetch}],[{next_event,internal,{score,{previous,{?SCORE_OFFSET,O}}}}]};
common({call,From},{clean,T},[O,Q]) ->
    {next_state,offset,[O,Q,#{from => From, tid => T, order => clean}],[{next_event,internal,{score,{previous,{?SCORE_OFFSET,O}}}}]};
common(cast,{reset,T},[O,Q]) ->
    NQ = resetting(T,Q),
    {keep_state,[O,NQ]};
common({call,From},{reset,T},[O,Q]) ->
    NQ = resetting(T,Q),
    {keep_state,[O,NQ],[{reply,From,ready}]};
common(cast,{{score,_R},_S},_StateData) ->
    keep_state_and_data;
common(cast,{{fetch,_R},ready},_StateData) ->
    keep_state_and_data;
common(cast,{{clean,_R},_T},_StateData) ->
    keep_state_and_data;
common(cast,EventContent,_StateData) ->
    io:format('State: common~nEventType: cast~nEventContent: ~p~n',[EventContent]),
    keep_state_and_data;
common({call,_From},EventContent,_StateData) ->
    io:format('State: common~nEventType: cast~nEventContent: ~p~n',[EventContent]),
    keep_state_and_data;
common(info,_EventContent,_StateData) ->
    keep_state_and_data.

offset(internal,{score,{Step,{L,U}}},[O,Q,MD]) ->
    R = make_ref(),
    N = scoring(L,U,R),
   % io:format("State:~p~nCommand:~p~nO:~p~nQ:~p~nL:~p~nU:~p~nStep:~p~nN:~p~nMD~p~n~n",[offset,score,O,Q,L,U,Step,N,MD]),
    if
        N > 0 ->
            {keep_state,[O,Q,MD#{number => N, step => Step, ref => R}],[{state_timeout,?TIMEOUT_STATE_OFFSET,Step}]};
        true ->
            {keep_state,[O,Q,MD#{number => N, step => Step, ref => R}],[{state_timeout,0,Step}]}
    end;
offset(state_timeout,Step,[O,Q,#{step := Step} = MD]) ->
   % io:format("TimeoutState:~p~nMD:~p~nO:~p~nQ~p~n~n",[offset,MD,O,Q]),
    if
        Step =:= previous ->
            case maps:is_key(current,MD) of
                false ->
                    {keep_state,[O,Q,MD#{Step => maps:get(Step,MD,0)}],[{next_event,internal,{score,{current,{O,O*10}}}}]};
                true ->
                    {keep_state,[O,Q,MD#{Step => maps:get(Step,MD,0) + maps:get(current,MD)}],[{next_event,internal,count}]}
            end;
        Step =:= current ->
            {keep_state,[O,Q,MD#{Step => maps:get(Step,MD,0) + maps:get(previous,MD)}],[{next_event,internal,{score,{following,{O*10,O*100}}}}]};
        Step =:= following ->
            {keep_state,[O,Q,MD#{Step => maps:get(Step,MD,0) + maps:get(current,MD)}],[{next_event,internal,count}]}
    end;
offset(cast,{{score,R},S},[O,Q,#{number := N, step := Step, ref := R} = MD]) ->
    if
        N > 1 ->
            {keep_state,[O,Q,MD#{Step => S + maps:get(Step,MD,0), number => N-1}],[{state_timeout,?TIMEOUT_STATE_OFFSET,Step}]};
        true ->
            if
                Step =:= previous ->
                    case maps:is_key(current,MD) of
                        false ->
                            {keep_state,[O,Q,MD#{Step => S + maps:get(Step,MD,0)}],[{state_timeout,cancel},{next_event,internal,{score,{current,{O,O*10}}}}]};
                        true ->
                            {keep_state,[O,Q,MD#{Step => S + maps:get(Step,MD,0)}],[{state_timeout,cancel},{next_event,internal,count}]}
                    end;
                Step =:= current ->
                    {keep_state,[O,Q,MD#{Step => S + maps:get(Step,MD,0) + maps:get(previous,MD)}],[{state_timeout,cancel},{next_event,internal,{score,{following,{O*10,O*100}}}}]};
                Step =:= following ->
                    {keep_state,[O,Q,MD#{Step => S + maps:get(Step,MD,0) + maps:get(current,MD)}],[{state_timeout,cancel},{next_event,internal,count}]}
            end
    end;
offset(internal,count,[O,Q,#{previous := P, current := C, following := F, from := From, order := Order} = MD]) ->
   % io:format("State:~p~nCommand:~p~nP:~p~nC:~p~nF:~p~nO:~p~nQ~p~n~n",[offset,count,P,C,F,O,Q]),
    if
        Q < 1 ->
            if
                Order =:= score ->
                    {next_state,common,[O,Q],[{reply,From,ready}]};
                Order =:= fetch ->
                    {next_state,select,[O,Q,#{from => From, tid => maps:get(tid,MD), order => fetch}],[{next_event,internal,fetch}]};	%% idle iteration but neccesary
                Order =:= clean ->
                    {next_state,select,[O,Q,#{from => From, tid => maps:get(tid,MD), order => clean}],[{next_event,internal,fetch}]}	%% idle iteration but necessary
            end;
        C / Q * 100 < ?MIN_OFFSET andalso O*10 =< ?MAX_ORDER andalso F / Q * 100 =< ?MAX_OFFSET ->
            {keep_state,[O*10,Q,MD#{previous => C, current => F, following => 0}],[{next_event,internal,{score,{following,{O*100,O*1000}}}}]};
        C / Q * 100 > ?MAX_OFFSET andalso O div 10 >= ?MIN_ORDER andalso P / Q * 100 >= ?MIN_OFFSET ->
            {keep_state,[O div 10,Q,MD#{previous => 0, current => P, following => C}],[{next_event,internal,{score,{previous,{?SCORE_OFFSET,O div 10}}}}]};
        true ->
            if
                Order =:= score ->
                    {next_state,common,[O,Q],[{reply,From,ready}]};
                Order =:= fetch ->
                    {next_state,select,[O,Q,#{from => From, tid => maps:get(tid,MD), order => fetch}],[{next_event,internal,fetch}]};
                Order =:= clean ->
                    {next_state,select,[O,Q,#{from => From, tid => maps:get(tid,MD), order => clean}],[{next_event,internal,fetch}]}
            end
    end;
offset(cast,{{score,_},_S},_StateData) ->
    keep_state_and_data;
offset(cast,{{fetch,_R},_S},_StateData) ->
    keep_state_and_data;
offset(cast,{{clean,_R},_T},_StateData) ->
    keep_state_and_data;
offset(cast,{point,_K},_StateData) ->
    {keep_state_and_data,[postpone]};
offset(cast,{cheat,_KVL},_StateData) ->
    {keep_state_and_data,[postpone]};
offset({call,_From},{count,_K},_StateData) ->
    {keep_state_and_data,[postpone]};
offset({call,_From},state,_StateData) ->
    {keep_state_and_data,[postpone]};
offset({call,_From},score,_StateData) ->
    {keep_state_and_data,[postpone]};
offset({call,_From},{fetch,_T},_StateData) ->
    {keep_state_and_data,[postpone]};
offset({call,_From},{clean,_T},_StateData) ->
    {keep_state_and_data,[postpone]};
offset(cast,_EventContent,_StateData) ->
    keep_state_and_data;
offset({call,_From},_EventContent,_StateData) ->
    keep_state_and_data;
offset(info,_EventContent,_StateData) ->
    keep_state_and_data.

select(internal,fetch,[O,Q,#{tid := T} = MD]) ->
   % io:format("State:~p~nCommand:~p~nMD:~p~nO:~p~nQ~p~n~n",[select,fetch,MD,O,Q]),
    R = make_ref(),
    N = fetching(O*10,T,R),
    if
        N > 0 ->
            {keep_state,[O,Q,MD#{number => N, ref => R}],[{state_timeout,?TIMEOUT_STATE_SELECT,T}]};
        true ->
            {keep_state,[O,Q,MD#{number => N, ref => R}],[{state_timeout,0,T}]}
    end;
select(state_timeout,T,[O,Q,#{tid := T, from := From, order := Order} = _MD]) ->
   % io:format("TimeoutState:~p~nMD:~p~nO:~p~nQ~p~n~n",[select,MD,O,Q]),
    if
        Order =:= fetch ->
            {next_state,common,[O,Q],[{reply,From,ready}]};
        Order =:= clean ->
            R1 = make_ref(),
            {next_state,delete,[O,Q,#{tid => T, ref => R1}],[{reply,From,R1},{state_timeout,?TIMEOUT_STATE_DELETE,T}]}
    end;
select(cast,{{fetch,R},ready},[O,Q,#{number := N, tid := T, from := From, order := Order, ref := R} = MD]) ->
    if
        N > 1 ->
            {keep_state,[O,Q,MD#{number => N - 1}],[{state_timeout,?TIMEOUT_STATE_SELECT,T}]};
        true ->
            if
                Order =:= fetch ->
                    {next_state,common,[O,Q],[{reply,From,ready}]};
                Order =:= clean ->
                    R1 = make_ref(),
                    {next_state,delete,[O,Q,#{tid => T, ref => R1}],[{reply,From,R1},{state_timeout,?TIMEOUT_STATE_DELETE,T}]}
            end
    end;
select(cast,{{fetch,_R},ready},_StateData) ->
    keep_state_and_data;
select(cast,{{score,_R},_S},_StateData) ->
    keep_state_and_data;
select(cast,{{clean,_R},_T},_StateData) ->
    keep_state_and_data;
select(cast,{point,_K},_StateData) ->
    {keep_state_and_data,[postpone]};
select(cast,{cheat,_KVL},_StateData) ->
    {keep_state_and_data,[postpone]};
select({call,_From},{count,_K},_StateData) ->
    {keep_state_and_data,[postpone]};
select({call,_From},state,_StateData) ->
    {keep_state_and_data,[postpone]};
select({call,_From},score,_StateData) ->
    {keep_state_and_data,[postpone]};
select({call,_From},{fetch,_T},_StateData) ->
    {keep_state_and_data,[postpone]};
select({call,_From},{clean,_T},_StateData) ->
    {keep_state_and_data,[postpone]};
select(cast,_EventContent,_StateData) ->
    keep_state_and_data;
select({call,_From},_EventContent,_StateData) ->
    keep_state_and_data;
select(info,_EventContent,_StateData) ->
    keep_state_and_data.

delete(state_timeout,T,[O,Q,#{tid := T, ref := _R}]) ->
   % io:format("TimeoutState:~p~nT:~p~nO:~p~nQ~p~n~n",[delete,T,O,Q]),
    {next_state,common,[O,Q]};
delete(cast,{{clean,R},T},[O,Q,#{tid := T, ref := R}]) ->
    NQ = resetting(T,Q),
    ?SUPPORT andalso erlang:apply(?AUXILIARY,reset,[T]),
    {next_state,common,[O,NQ]};
delete(cast,{{clean,_R},_T},_StateData) ->
    keep_state_and_data;
delete(cast,{{fetch,_R},ready},_StateData) ->
    keep_state_and_data;
delete(cast,{{score,_R},_S},_StateData) ->
    keep_state_and_data;
delete(cast,{point,_K},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(cast,{cheat,_KVL},_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_From},{count,_K},_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_From},state,_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_From},score,_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_From},{fetch,_T},_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_From},{clean,_T},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(cast,_EventContent,_StateData) ->
    keep_state_and_data;
delete({call,_From},_EventContent,_StateData) ->
    keep_state_and_data;
delete(info,_EventContent,_StateData) ->
    keep_state_and_data.


fetching(O,T,R) ->
    if
        (O-1) div ?MAX_LIMIT == 0 ->
            lfu_utils:for(0,(O-1) div ?MIN_LIMIT,
                fun(I) ->
                    N = list_to_atom("o0" ++ integer_to_list(I)),
                    case whereis(N) of
                        undefined -> skip;
                        _ ->
                            lfu_exact_score:fetch(N,R,T,
                                if
                                    I == 0 ->
                                        ?SCORE_OFFSET+1;
                                    true -> ?MIN_LIMIT*I+1
                                end,
                                if
                                    O > ?MIN_LIMIT*(I+1) ->
                                        ?MIN_LIMIT*(I+1);
                                    true -> O
                                end
                            )
                    end
                end
            ),
            length(lists:filter(
                fun(I) ->
                    case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                        undefined -> false;
                        _ -> true
                    end
                end,
            lists:seq(0,(O-1) div ?MIN_LIMIT)));
        true ->
            lfu_utils:for(0,(?MAX_LIMIT-1) div ?MIN_LIMIT,
                fun(I) ->
                    N = list_to_atom("o0" ++ integer_to_list(I)),
                    case whereis(N) of
                        undefined -> skip;
                        _ ->
                            lfu_exact_score:fetch(N,R,T,
                                if
                                    I == 0 ->
                                        ?SCORE_OFFSET+1;
                                    true -> ?MIN_LIMIT*I+1
                                end,
                                if
                                    O > ?MIN_LIMIT*(I+1) ->
                                        ?MIN_LIMIT*(I+1);
                                    true ->	O %% never hit in this branch
                                end
                            )
                    end
                end
            ),
            L1 = length(lists:filter(
                fun(I) ->
                    case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                        undefined -> false;
                        _ -> true
                    end
                end,
            lists:seq(0,(?MAX_LIMIT-1) div ?MIN_LIMIT))),

            lfu_utils:for(1,(O-1) div ?MAX_LIMIT,
                fun(I) ->
                    N = list_to_atom("o" ++ integer_to_list(I)),
                    case whereis(N) of
                        undefined -> skip;
                        _ ->
                            lfu_quick_score:fetch(N,R,T)
                    end
                end
            ),
            L2 = length(lists:filter(
                fun(I) ->
                    case whereis(list_to_atom("o" ++ integer_to_list(I))) of
                        undefined -> false;
                        _ -> true
                    end
                end,
            lists:seq(1,(O-1) div ?MAX_LIMIT))),
            L1 + L2
    end.

scoring(L,U,R) ->
    if
        U =< ?MAX_LIMIT ->
            lfu_utils:for(L div ?MIN_LIMIT,(U-1) div ?MIN_LIMIT,
                fun(I) ->
                    case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                        undefined ->
                            skip;
                        N ->
                            if
                                I == 0 ->
                                    if
                                        U > ?MIN_LIMIT*(I+1) ->
                                            lfu_exact_score:score(N,R,L+1,?MIN_LIMIT*(I+1));
                                        true ->
                                            lfu_exact_score:score(N,R,L+1,U)
                                    end;
                                true ->
                                    lfu_exact_score:score(N,R,?MIN_LIMIT*I+1,?MIN_LIMIT*(I+1))
                            end
                    end
                end
            ),
            length(lists:filter(
                fun(I) ->
                    case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                        undefined -> false;
                        _ -> true
                    end
                end,
            lists:seq(L div ?MIN_LIMIT,(U-1) div ?MIN_LIMIT)));
        true ->
            lfu_utils:for(L div ?MIN_LIMIT,(?MAX_LIMIT-1) div ?MIN_LIMIT,
                fun(I) ->
                    case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                        undefined ->
                            skip;
                        N ->
                            if
                                I == 0 ->
                                    lfu_exact_score:score(N,R,L+1,?MIN_LIMIT*(I+1));
                                true ->
                                    lfu_exact_score:score(N,R,?MIN_LIMIT*I+1,?MIN_LIMIT*(I+1))
                            end
                    end
                end
            ),
            L1 =
                if
                    L < ?MAX_LIMIT ->
                        length(lists:filter(
                            fun(I) ->
                                case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                                    undefined -> false;
                                    _ -> true
                                end
                            end,
                         lists:seq(L div ?MIN_LIMIT,(?MAX_LIMIT-1) div ?MIN_LIMIT)));
                     true -> 0
                 end,

            lfu_utils:for(if L div ?MAX_LIMIT > 0 -> L div ?MAX_LIMIT; true -> 1 end,U div ?MAX_LIMIT - 1,
                fun(I) ->
                    case whereis(list_to_atom("o" ++ integer_to_list(I))) of
                        undefined -> skip;
                        N -> lfu_quick_score:score(N,R)
                    end
                end
            ),
            L2 = length(lists:filter(
                fun(I) ->
                    case whereis(list_to_atom("o" ++ integer_to_list(I))) of
                        undefined -> false;
                        _ -> true
                    end
                end,
            lists:seq(if L div ?MAX_LIMIT > 0 -> L div ?MAX_LIMIT; true -> 1 end,U div ?MAX_LIMIT - 1))),
            L1 + L2
    end.

resetting(T,Q) ->
    TL = case ets:whereis(T) of
        undefined -> [];
        _ -> ets:tab2list(T)
    end,
    put(reset,0),
    lists:foreach(
        fun({_,KL}) ->
            lists:foreach(
                fun(K) ->
                    ets:delete(?ETS_KEYS_TABLE_NAME,K),
                    C = erase(K),
                    if
                        (C-1) div ?MAX_LIMIT == 0 ->
                            N  = list_to_atom("o0" ++ integer_to_list((C-1) div ?MIN_LIMIT)),
                            case whereis(N) of
                                undefined ->
                                    lfu_exact_score_sup:start([(C-1) div ?MIN_LIMIT,0]),
                                    lfu_exact_score:reset(N,K);
                                _ ->
                                    lfu_exact_score:reset(N,K)
                            end;
                        true ->
                            N = list_to_atom("o" ++ integer_to_list((C-1) div ?MAX_LIMIT)),
                            case whereis(N) of
                                undefined ->
                                    lfu_quick_score_sup:start([(C-1) div ?MAX_LIMIT,0]),
                                    lfu_quick_score:reset(N,K);
                                true ->
                                    lfu_quick_score:reset(N,K)
                            end
                    end,
                    put(reset,get(reset)+1)
                end,
            KL)
        end,
    TL),
    Q - erase(reset).

restorage(T) ->
    TL = case ets:whereis(T) of
        undefined -> [];
        _ -> ets:tab2list(T)
    end,
    %io:format("+!!!!!TL:~p!!!!!+~n",[TL]),

    put(quantity,0),
    lists:foreach(
        fun({K,V}) ->
            if
                V =< ?MAX_ORDER ->
                    if
                        V div ?MAX_LIMIT =< 1 ->
                            N = list_to_atom("o0" ++ integer_to_list((V -1)div ?MIN_LIMIT)),
                            whereis(N) =:= undefined andalso lfu_exact_score_sup:start([(V-1) div ?MIN_LIMIT,0]);
                        true ->
                            N = list_to_atom("o" ++ integer_to_list((V-1) div ?MAX_LIMIT)),
                            whereis(N) =:= undefined andalso lfu_quick_score_sup:start([(V-1) div ?MAX_LIMIT,0])
                    end,
                    put(K,V),
                    V > ?SCORE_OFFSET andalso put(quantity,get(quantity)+1),
                    ?SUPPORT andalso erlang:apply(?AUXILIARY,cheat,[[{K,V}]]);
                true ->
                    ?SUPPORT andalso erlang:apply(?AUXILIARY,cheat,[[{K,V}]])
            end
        end,
    TL),
    erase(quantity).
