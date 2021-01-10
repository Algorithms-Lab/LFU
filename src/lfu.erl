-module('lfu').
-author('VSolenkov').

-behaviour('gen_statem').

-export([
    start_link/0
]).
-export([
    point/1,
    cheat/1,
    count/1,
    state/0,
    store/0,
    fetch/0,
    clean/0,
    clean/1,
    clean/2
]).
-export([
    reset/1
]).
-export([
    init/1,
    callback_mode/0
]).
-export([
    common/3,
    delete/3
]).
-include("include/lfu.hrl").



start_link() ->
    gen_statem:start_link(
        {local,?MODULE},
        ?MODULE,[0,0,0],
    [{spawn_opt,?SPAWN_OPT_LRU}]).



init([_,_,_]) ->
    [L,M,Q] = restorage(?ETS_KEYS_STORE_TABLE_NAME),
    {ok,common,[L,M,Q]}.

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
store() ->
    gen_statem:cast(?MODULE,store).
fetch() ->
    gen_statem:call(?MODULE,fetch).
clean() ->
    gen_statem:call(?MODULE,{clean,async}).
clean(async) ->
    gen_statem:call(?MODULE,{clean,async});
clean(sync) ->
    gen_statem:call(?MODULE,{clean,sync}).

clean(R,K) ->
    gen_statem:cast(?MODULE,{{clean,R},K}).

reset(D) ->
    gen_statem:cast(?MODULE,{reset,D}).


common(cast,{point,K},[L,M,Q]) ->
    [NL,NM,NQ] = point_handler(K,L,M,Q),
    {keep_state,[NL,NM,NQ]};
common(cast,{cheat,KVL},[L,M,Q]) ->
    [NL,NM,NQ] = cheat_handler(KVL,L,M,Q),
    {keep_state,[NL,NM,NQ]};
common(cast,store,[L,M,Q]) ->
    lfu_utils:ets_reset([?ETS_KEYS_STORE_TABLE_NAME]),
    {keep_state,[L,M,Q]};
common(cast,{reset,D},[L,M,Q]) ->
    [NL,NM,NQ] = reset_handler(D,L,M,Q),
    {keep_state,[NL,NM,NQ]};
common({call,F},{count,K},[L,M,Q]) ->
    {keep_state,[L,M,Q],[{reply,F,get(K)}]};
common({call,F},state,[L,M,Q]) ->
    {keep_state,[L,M,Q],[{reply,F,[L,M,Q]}]};
common({call,F},fetch,[L,M,Q]) ->
    {keep_state,[L,M,Q],[{reply,F,fetch_handler(L,Q)}]};
common({call,F},{clean,async},[L,M,Q]) ->
    {next_state,delete,[L,M,Q,#{from => F}],[{next_event,internal,clean}]};
common({call,F},{clean,sync},[L,M,Q]) ->
    K = fetch_handler(L,Q),
    R = make_ref(),
    {next_state,delete,[L,M,Q,#{key => K, ref => R}],[{reply,F,{K,R}},{state_timeout,?TIMEOUT_STATE_DELETE,K}]};
common(cast,{{clean,_R},_K},_StateData) ->
    keep_state_and_data;
common(cast,EventContent,_StateData) ->
    io:format('State: common~nEventType: cast~nEventContent: ~p~n',[EventContent]),
    keep_state_and_data;
common({call,_F},EventContent,_StateData) ->
    io:format('State: common~nEventType: cast~nEventContent: ~p~n',[EventContent]),
    keep_state_and_data;
common(info,_EventContent,_StateData) ->
    keep_state_and_data.

delete(internal,clean,[L,M,Q,#{from := F}]) ->
    [K,NL,NM,NQ] = clean_handler(L,M,Q),
    {next_state,common,[NL,NM,NQ],[{reply,F,K}]};
delete(state_timeout,K,[L,M,Q,#{key := K, ref := _}]) ->
    {next_state,common,[L,M,Q]};
delete(cast,{{clean,R},K},[L,M,Q,#{key := K, ref := R}]) ->
    [K,NL,NM,NQ] = clean_handler(L,M,Q),
    {next_state,common,[NL,NM,NQ]};
delete(cast,{{clean,_R},_K},_StateData) ->
    keep_state_and_data;
delete(cast,{point,_K},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(cast,{cheat,_KVL},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(cast,store,_StateData) ->
    {keep_state_and_data,[postpone]};
delete(cast,reset,_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_F},{count,_K},_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_F},state,_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_F},fetch,_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_F},{clean,async},_StateData) ->
    {keep_state_and_data,[postpone]};
delete({call,_F},{clean,sync},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(cast,_EventContent,_StateData) ->
    keep_state_and_data;
delete({call,_F},_EventContent,_StateData) ->
    keep_state_and_data;
delete(info,_EventContent,_StateData) ->
    keep_state_and_data.


point_handler(K,L,M,Q) ->
    case get(K) of
        undefined ->
            put(K,1),
            case get(0) of
                undefined -> put(0,<<"1">>);
                QS -> put(0,integer_to_binary(binary_to_integer(QS)+1))
            end,
            ets:insert(?ETS_KEYS_STORE_TABLE_NAME,{K,1}),
            ?SUPPORT andalso erlang:apply(?AUXILIARY,point,[K]),
            [1,if Q == 0 -> 1; true -> M end,Q+1];
        OC ->
            if
                OC =< ?MAX_COUNTER ->
                    if
                        OC rem ?SERIE_SIZE == 0 ->
                            case get((OC-1) div ?SERIE_SIZE) of
                                <<"1">> -> erase((OC-1) div ?SERIE_SIZE);
                                QS1 -> put((OC-1) div ?SERIE_SIZE,integer_to_binary(binary_to_integer(QS1)-1))
                            end,
                            put(K,OC+1),
                            case get(OC div ?SERIE_SIZE) of
                                undefined -> put(OC div ?SERIE_SIZE,<<"1">>);
                                QS2 -> put(OC div ?SERIE_SIZE,integer_to_binary(binary_to_integer(QS2)+1))
                            end;
                        true ->
                            put(K,OC+1)
                    end,
                    ets:insert(?ETS_KEYS_STORE_TABLE_NAME,{K,OC+1}),
                    ?SUPPORT andalso erlang:apply(?AUXILIARY,point,[K]),
                    [
                        if
                            L == OC ->
                                case length(get_keys(L)) of
                                    0 -> L + 1;
                                    _ -> L
                                end;
                            true -> L
                        end,
                        if
                            M == OC -> M + 1;
                            true -> M
                        end,Q
                     ];
                true ->
                    ?SUPPORT andalso erlang:apply(?AUXILIARY,point,[K]),
                    [L,M,Q]
            end
    end.

cheat_handler(KVL,L,M,Q) ->
    [NL,NM,NQ] = lists:foldl(
        fun({K,V},[NL,NM,NQ]) when V =< ?MAX_COUNTER ->
            case get(K) of
                undefined ->
                    if
                        V > 0 ->
                            put(K,V),
                            put((V-1) div ?SERIE_SIZE,
                                case get((V-1) div ?SERIE_SIZE) of
                                    undefined -> <<"1">>;
                                    QS -> integer_to_binary(binary_to_integer(QS) + 1)
                                end
                            ),
                            ets:insert(?ETS_KEYS_STORE_TABLE_NAME,{K,V}),
                            if
                                NQ == 0 ->
                                    [V,V,1];
                                NM < V ->
				    [NL,V,NQ+1];
                                NL > V ->
                                    [V,NM,NQ+1];
                                true ->
                                    [NL,NM,NQ+1]
                            end;
                        true ->
                            [NL,NM,NQ]
                    end;
                OV ->
                    if
                        V > 0 ->
                            put(K,V),
                            get((V-1) div ?SERIE_SIZE) /= get((OV-1) div ?SERIE_SIZE) andalso
                            case get((OV-1) div ?SERIE_SIZE) of
                                <<"1">> -> erase((OV-1) div ?SERIE_SIZE),true;
                                QS1 -> put((OV-1) div ?SERIE_SIZE,integer_to_binary(binary_to_integer(QS1)-1)),true
                            end andalso
                            case get((V-1) div ?SERIE_SIZE) of
                                undefined -> put((V-1) div ?SERIE_SIZE,<<"1">>);
                                QS2 -> put((V-1) div ?SERIE_SIZE,integer_to_binary(binary_to_integer(QS2)-1))
                            end,
                            ets:insert(?ETS_KEYS_STORE_TABLE_NAME,{K,V}),
                            if
                                NM < V ->
                                    if
                                        NL == OV ->
                                            [
                                                case length(get_keys(NL)) of
                                                    0 -> compute_oldest_key((NL-1) div ?SERIE_SIZE,(V-1) div ?SERIE_SIZE);
                                                    _ -> NL
                                                end,
                                                V,NQ
                                            ];
                                        true ->
                                            [NL,V,NQ]
                                    end;
                                NL > V ->
                                    [V,NM,NQ];
                                NL == OV ->
                                    [
                                        case length(get_keys(NL)) of
                                            0 -> compute_oldest_key((NL-1) div ?SERIE_SIZE,(NM-1) div ?SERIE_SIZE);
                                            _ -> NL
                                        end,
                                        NM,NQ
                                    ];
                                true ->
                                    [NL,NM,NQ]
                            end;
                        true ->
                            erase(K),
                            case get((OV-1) div ?SERIE_SIZE) of
                                <<"1">> -> erase((OV-1) div ?SERIE_SIZE);
                                QS3 -> put((OV-1) div ?SERIE_SIZE,integer_to_binary(binary_to_integer(QS3)-1))
                            end,
                            ets:delete(?ETS_KEYS_STORE_TABLE_NAME,K),
                            if
                                NL == OV ->
                                    [
                                        case length(get_keys(NL)) of
                                            0 -> compute_oldest_key((NL-1) div ?SERIE_SIZE,(NM-1) div ?SERIE_SIZE);
                                            _ -> NL
                                        end,NM,NQ-1
                                    ];
                                true ->
                                    [NL,NM,NQ-1]
                            end
                    end
            end;
        (_,[NL,NM,NQ]) ->
            [NL,NM,NQ]
        end,
    [L,M,Q],KVL),
    [NL,if NQ == 0 -> 0; true -> NM end,NQ].

fetch_handler(L,Q) ->
    if
        Q =:= 0 ->
            {L,[]};								%% that is: {0,[]};
        true ->
            [K|_] = get_keys(L),
            {L,[K]}
    end.

clean_handler(L,M,Q) ->
    if
        Q =:= 0 ->
            [{L,[]},L,M,Q];							%% that is: [{0,[]},0,0,0];
        true ->
            [K|_] = get_keys(L),
            erase(K),
            case get((L-1) div ?SERIE_SIZE) of
                <<"1">> -> erase((L-1) div ?SERIE_SIZE);
                QS -> put((L-1) div ?SERIE_SIZE,integer_to_binary(binary_to_integer(QS)-1))
            end,
            ets:delete(?ETS_KEYS_STORE_TABLE_NAME,K),
            ?SUPPORT andalso erlang:apply(?AUXILIARY,reset,[[{L,[K]}]]),
            [
                {L,[K]},
                case length(get_keys(L)) of
                    0 -> compute_oldest_key((L-1) div ?SERIE_SIZE,(M-1) div ?SERIE_SIZE);
                    _ -> L
                end,
                if
                    Q-1 == 0 -> 0;
                    true -> M
                end,
                Q-1
            ]
    end.

reset_handler({_,KL},L,M,Q) ->
    [NL,NQ] = lists:fold1(
        fun(K,[NL,NQ]) ->
            case erase(K) of
                undefined -> [NL,NQ];
                OC ->
                    case get((OC-1) div ?SERIE_SIZE) of
                        <<"1">> -> erase((OC-1) div ?SERIE_SIZE);
                        QS -> put((OC-1) div ?SERIE_SIZE,integer_to_binary(binary_to_integer(QS)-1))
                    end,
                    ets:delete(?ETS_KEYS_STORE_TABLE_NAME,K),
                    if
                        OC == NL ->
                            [
                                case length(get_keys(NL)) of
                                    0 -> compute_oldest_key((NL-1) div ?SERIE_SIZE,(M-1) div ?SERIE_SIZE);
                                    _ -> NL
                                end,NQ-1
                            ];
                        true ->
                            [NL,NQ-1]
                    end
            end
        end,
    [L,Q],KL),
    [NL,if NQ == 0 -> 0; true -> M end,NQ];
reset_handler(T,L,M,Q) ->
    [NL,NQ] = case ets:info(T) of
        undefined -> [L,Q];
        _ ->
            ets:foldl(
                fun({_,KL},[NL,NQ]) ->
                    lists:foldl(
                        fun(K,[NL,NQ]) ->
                            case erase(K) of
                                undefined -> [NL,NQ];
                                OC ->
                                    case get((OC-1) div ?SERIE_SIZE) of
                                        <<"1">> -> erase((OC-1) div ?SERIE_SIZE);
                                        QS -> put((OC-1) div ?SERIE_SIZE,integer_to_binary(binary_to_integer(QS)-1))
                                    end,
                                    ets:delete(?ETS_KEYS_STORE_TABLE_NAME,K),
                                    if
                                        OC == NL ->
                                            [
                                                case length(get_keys(NL)) of
                                                    0 -> compute_oldest_key((NL-1) div ?SERIE_SIZE,(M-1) div ?SERIE_SIZE);
                                                    _ -> NL
                                                end,NQ-1
                                            ];
                                        true ->
                                            [NL,NQ-1]
                                    end
                            end
                        end,
                    [NL,NQ],KL)
                end,
            [L,Q],T)
    end,
    [NL,if NQ == 0 -> 0; true -> M end,NQ].


compute_oldest_key(U,U) ->
    case get(U) of
        undefined -> 0;
        _ -> check_keys_serie((U*?SERIE_SIZE)+1,(U+1)*?SERIE_SIZE)
    end;
compute_oldest_key(L,U) ->
    case get(L) of
        undefined -> compute_oldest_key(L+1,U);
        _ -> check_keys_serie((L*?SERIE_SIZE)+1,(L+1)*?SERIE_SIZE)
    end.

check_keys_serie(U,U) ->
    case get_keys(U) of
        [] -> 0;
        [_|_] -> U
    end;
check_keys_serie(L,U) ->
    case get_keys(L) of
        [] -> check_keys_serie(L+1,U);
        [_|_] -> L
    end.


restorage(T) ->
    case ets:whereis(T) of
        undefined -> [0,0,0];
        _ ->
            ets:foldl(
                fun({K,C},[NL,NM,NQ]) ->
                    if
                        C =< ?MAX_COUNTER andalso C > 0 ->
                            QS = get((C-1) div ?SERIE_SIZE),
                            put((C-1) div ?SERIE_SIZE,
                                if
                                    QS == undefined -> <<"1">>;
                                    true -> integer_to_binary(binary_to_integer(QS) + 1)
                                end
                            ),
                            put(K,C),
                            if
                                NQ == 0 ->
                                    [C,C,1];
                                C < NL ->
                                    [C,NM,NQ+1];
                                C > NM ->
                                    [NL,C,NQ+1];
                                true ->
                                    [NL,NM,NQ+1]
                            end;
                        true ->
                            [NL,NM,NQ]
                    end
                end,
            [0,0,0],T)
    end.
