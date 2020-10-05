-module(lfu_quick_score).
-author('VSolenkov').

-behavior('gen_server').

-export([
    start/1,
    state/1,
    point/2,
    reset/2,
    cheat/3,
    score/3,
    fetch/4
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
    [{spawn_opt,?SPAWN_OPT_SIMPLE_SCORE}]).


init([O,Q]) ->
    case ets:lookup(?ETS_PIDS_TABLE_NAME,list_to_binary(?PREFIX_KEY ++ "o" ++ integer_to_list(O) ++ ?POSTFIX_KEY)) of
        [] ->
            ets:insert(?ETS_PIDS_TABLE_NAME,{
                list_to_binary(?PREFIX_KEY ++ "o" ++ integer_to_list(O) ++ ?POSTFIX_KEY),
            self()}),
	    {ok,[O,Q]};
        _ ->
            NQ = restorage(?ETS_KEYS_TABLE_NAME,?MAX_LIMIT*O+1,?MAX_LIMIT*(O+1)),
            ets:insert(?ETS_PIDS_TABLE_NAME,{
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
score(N,P,R) ->
    gen_server:cast(N,{score,{P,R}}).
fetch(N,P,R,T) ->
    gen_server:cast(N,{fetch,{T,P,R}}).
state(N) ->
    gen_server:call(N,state,?TIMEOUT_CALL).



handle_cast({point,K},[O,Q]) ->
    NQ = point_handler(K,Q),
    {noreply,[O,NQ]};
handle_cast({cheat,{K,V}},[O,Q]) ->
    NQ = cheat_handler(K,V,Q),
    {noreply,[O,NQ]};
handle_cast({reset,K},[O,Q]) ->
    NQ = reset_handler(K,Q),
    {noreply,[O,NQ]};
handle_cast({score,{P,R}},[O,Q]) ->
    score_handler(P,R,Q),
    {noreply,[O,Q]};
handle_cast({fetch,{T,P,R}},[O,Q]) ->
    fetch_handler(T,P,R,O,Q),
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
            put(K,C+1),
            Q
    end.
cheat_handler(K,V,Q) ->
    put(K,V),
    Q+1.
reset_handler(K,Q) ->
    erase(K),
    Q-1.
score_handler(P,R,Q) ->
    catch P ! {{score,R},Q}.  %% necessary using client interface function of lfu module for cast message
fetch_handler(T,P,R,O,Q) ->
    if
        Q > 0 ->
            insert(O,T),
            catch P ! {{fetch,R},ready};  %% necessary using client interface function of lfu module for cast message
        true ->
            catch P ! {{fetch,R},ready}   %% necessary using client interface function of lfu module for cast message
    end.


insert(I,T) ->
    case get_keys(1) of
        [] -> 1;
        KL -> ets:insert(T,{I*?MAX_LIMIT,KL})
    end.

restorage(T,L,U) ->
    TL = case ets:info(T) of
        undefined -> [];
        _ -> ets:tab2list(T)
    end,
    %io:format("+!!!!!TL:~p!!!!!+~n",[TL]),

    put(quantity,0),
    lists:foreach(
        fun({K,_}) ->
            put(K,1),
            put(quantity,get(quantity)+1)
        end,
        lists:filter(
            fun({_,V}) ->
                V >= L andalso V =< U
            end,
        TL)
    ),
    erase(quantity).
