-module(lfu_quick_score).
-author('VSolenkov').

-behavior('gen_server').

-export([
    start/1,
    state/1,
    point/2,
    reset/2,
    cheat/3,
    score/2,
    fetch/3
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
        {local,list_to_atom("o" ++ integer_to_list(O))},
        ?MODULE,[O,Q],
    [{spawn_opt,?SPAWN_OPT_QUICK_SCORE}]).


init([O,Q]) ->
    case ets:lookup(?ETS_PIDS_STORE_TABLE_NAME,list_to_binary(?PREFIX_KEY ++ "o" ++ integer_to_list(O) ++ ?POSTFIX_KEY)) of
        [] ->
            ets:insert(?ETS_PIDS_STORE_TABLE_NAME,{
                list_to_binary(?PREFIX_KEY ++ "o" ++ integer_to_list(O) ++ ?POSTFIX_KEY),
            self()}),
	    {ok,[O,Q]};
        _ ->
            NQ = restorage(?ETS_KEYS_STORE_TABLE_NAME,?MAX_LIMIT*O+1,?MAX_LIMIT*(O+1)),
            ets:insert(?ETS_PIDS_STORE_TABLE_NAME,{
                list_to_binary(?PREFIX_KEY ++ "o" ++ integer_to_list(O) ++ ?POSTFIX_KEY),
            self()}),
            {ok,[O,NQ]}
    end.

point(N,K) ->
    gen_server:cast(N,{point,K}).
reset(N,K) ->
    gen_server:cast(N,{reset,K}).
cheat(N,K,V) ->
    gen_server:cast(N,{cheat,{K,V}}).
score(N,R) ->
    gen_server:cast(N,{score,R}).
fetch(N,R,T) ->
    gen_server:cast(N,{fetch,{T,R}}).
state(N) ->
    gen_server:call(N,state,90000).



handle_cast({point,K},[O,Q]) ->
    NQ = point_handler(K,Q),
    {noreply,[O,NQ]};
handle_cast({cheat,{K,V}},[O,Q]) ->
    NQ = cheat_handler(K,V,Q),
    {noreply,[O,NQ]};
handle_cast({reset,K},[O,Q]) ->
    NQ = reset_handler(K,Q),
    {noreply,[O,NQ]};
handle_cast({score,R},[O,Q]) ->
    score_handler(R,Q),
    {noreply,[O,Q]};
handle_cast({fetch,{T,R}},[O,Q]) ->
    fetch_handler(T,R,O,Q),
    {noreply,[O,Q]}.

handle_call(state,_From,[O,Q]) ->
    {reply,[O,Q],[O,Q]}.

handle_info({'EXIT',_P,normal},[O,Q]) ->
    {noreply,[O,Q]};
handle_info({'EXIT',P,R},[O,Q]) ->
    io:format("Process: ~p over by reason: ~p~n",[P,R]),
    {noreply,[O,Q]};
handle_info(_,[O,Q]) ->
    {noreply,[O,Q]}.


point_handler(K,Q) ->
    case get(K) of
        undefined ->
            put(K,1),
            Q+1;
        C ->
            Q
    end.
cheat_handler(K,V,Q) ->
    put(K,V),
    Q+1.
reset_handler(K,Q) ->
    erase(K),
    Q-1.
score_handler(R,Q) ->
    lfu:score(R,Q).
fetch_handler(T,R,O,Q) ->
    if
        Q > 0 ->
            catch insert(O,T),
            lfu:fetch(R,ready);
        true ->
            lfu:fetch(R,ready)
    end.


insert(I,T) ->
    KL = get_keys(1),
    KL =/= [] andalso ets:insert(T,{I*?MAX_LIMIT,KL}).

restorage(T,L,U) ->
    put(quantity,0),
    ets:foldl(
        fun({K,V},[]) ->
            if
                V >= L andalso V =< U ->
                    put(K,1),
                    put(quantity,get(quantity)+1),
                    [];
                true ->
                    []
            end
        end,
    [],T),
    erase(quantity).
