-module('lfu_exact_score').
-author('VSolenkov').

-behavior('gen_server').

-export([
    start/1,
    state/1,
    point/2,
    reset/2,
    cheat/3,
    score/4,
    fetch/5
]).

-export([
    init/1
]).
-export([
    handle_cast/2,
    handle_call/3,
    handle_info/2
]).
-include("include/lfu.hrl").


start([O,Q]) ->
    gen_server:start_link(
        {local,list_to_atom("o0" ++ integer_to_list(O))},
        ?MODULE,[O,Q],
    [{spawn_opt,?SPAWN_OPT_EXACT_SCORE}]).


init([O,Q]) ->
    case ets:lookup(?ETS_PIDS_TABLE_NAME,list_to_binary(?PREFIX_KEY ++ "o0" ++ integer_to_list(O) ++ ?POSTFIX_KEY)) of
        [] ->
            ets:insert(?ETS_PIDS_TABLE_NAME,{
                list_to_binary(?PREFIX_KEY ++ "o0" ++ integer_to_list(O) ++ ?POSTFIX_KEY),
            self()}),
	    {ok,[O,Q]};
        _ ->
            NQ = restorage(?ETS_KEYS_TABLE_NAME,?MIN_LIMIT*O+1,?MIN_LIMIT*(O+1),O),
            ets:insert(?ETS_PIDS_TABLE_NAME,{
                list_to_binary(?PREFIX_KEY ++ "o0" ++ integer_to_list(O) ++ ?POSTFIX_KEY),
            self()}),
            {ok,[O,NQ]}
    end.


point(N,K) ->
    gen_server:cast(N,{point,K}).
reset(N,K) ->
    gen_server:cast(N,{reset,K}).
cheat(N,K,V) ->
    gen_server:cast(N,{cheat,{K,V}}).
score(N,R,L,U) ->
    gen_server:cast(N,{score,{L,U,R}}).
fetch(N,R,T,L,U) ->
    gen_server:cast(N,{fetch,{L,U,T,R}}).
state(N) ->
    gen_server:call(N,state,?TIMEOUT_CALL).



handle_cast({point,K},[O,Q]) ->
    [NO,NQ] = point_handler(K,O,Q),
    {noreply,[NO,NQ]};
handle_cast({cheat,{K,V}},[O,Q]) ->
    [NO,NQ] = cheat_handler(K,V,O,Q),
    {noreply,[NO,NQ]};
handle_cast({reset,K},[O,Q]) ->
    [NO,NQ] = reset_handler(K,O,Q),
    {noreply,[NO,NQ]};
handle_cast({score,{L,U,R}},[O,Q]) ->
    [NO,NQ] = score_handler(L,U,R,O,Q),
    {noreply,[NO,NQ]};
handle_cast({fetch,{L,U,T,R}},[O,Q]) ->
    [NO,NQ] = fetch_handler(L,U,T,R,O,Q),
    {noreply,[NO,NQ]}.

handle_call(state,_From,[O,Q]) ->
    {reply,[O,Q],[O,Q]}.

handle_info({'EXIT',_P,normal},[O,Q]) ->
    {noreply,[O,Q]};
handle_info({'EXIT',P,R},[O,Q]) ->
    io:format("Process: ~p over by reason: ~p~n",[P,R]),
    {noreply,[O,Q]};
handle_info(_,[O,Q]) ->
    {noreply,[O,Q]}.


point_handler(K,O,Q) ->
    case get(K) of
        undefined ->
            put(K,?MIN_LIMIT*O+1),
            if
                O == 0 ->
                    if
                        ?SCORE_OFFSET == 0 ->
                            [O,Q+1];
                        true ->
                            [O,Q]
                    end;
                true ->
                    [O,Q+1]
            end;
        C ->
            put(K,C+1),
            if
                (C+1) / (?SCORE_OFFSET+1) == 1 ->
                    [O,Q+1];
                true ->
                    [O,Q]
            end
    end.
cheat_handler(K,V,O,Q) ->
    put(K,V),
    if
        V > ?SCORE_OFFSET ->
            [O,Q+1];
        true ->
            [O,Q]
    end.
reset_handler(K,O,Q) ->
    V = erase(K),
    if
        V > ?SCORE_OFFSET ->
            [O,Q-1];
        true ->
            [O,Q]
    end.
score_handler(L,U,R,O,Q) ->
    C = if O > 0 orelse (L == ?SCORE_OFFSET+1 andalso U == ?MIN_LIMIT*(O+1)) -> Q; true -> scoring(L,U) end,
    lfu:score(R,C),
    [O,Q].
fetch_handler(L,U,T,R,O,Q) ->
    if
        Q > 0 ->
            insert(L,U,T);
        true -> skip
    end,
    lfu:fetch(R,ready),
    [O,Q].


scoring(L,U) ->
    put(counter,0.0),
    for(L,U,
        fun(I) ->
            put(counter,get(counter) + length(get_keys(I)))
        end
    ),
    get(counter).

insert(L,U,T) ->
    for(L,U,
        fun(I) ->
            KL = get_keys(I),
            KL =/= [] andalso ets:insert(T,{I,KL})
        end
    ).

restorage(T,L,U,O) ->
    TL = case ets:info(T) of
        undefined -> [];
        _ -> ets:tab2list(T)
    end,
    %io:format("+!!!!!TL:~p!!!!!+~n",[TL]),

    put(quantity,0),
    if
        O == 0 ->
            lists:foreach(
                fun({K,V}) ->
                    put(K,V),
                    V > ?SCORE_OFFSET andalso put(quantity,get(quantity)+1)
                end,
                lists:filter(
                    fun({_,V}) ->
                        V >= L andalso V =< U
                    end,
                TL)
            );
        true ->
            lists:foreach(
                fun({K,V}) ->
                    put(K,V),
                    put(quantity,get(quantity)+1)
                end,
                lists:filter(
                    fun({_,V}) ->
                        V >= L andalso V =< U
                    end,
                TL)
            )
    end,
    erase(quantity).

for(N,N,F) -> F(N);
for(I,N,_) when I > N -> null;
for(I,N,F) -> F(I),for(I+1,N,F).
