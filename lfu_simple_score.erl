-module(lfu_simple_score).
-author('VSolenkov').

-export([
    start/1,
    point/2,
    reset/2,
    cheat/3,
    score/5,
    fetch/6
]).

-export([
    init/1
]).
-include("include/lfu.hrl").


point(N,K) ->
    cast(N,point,K).
reset(N,K) ->
    cast(N,reset,K).
cheat(N,K,V) ->
    cast(N,cheat,{K,V}).
score(N,R,P,L,U) ->
    cast(N,score,{R,P},{L,U}).
fetch(N,R,P,T,L,U) ->
    cast(N,fetch,{R,P},{T,L,U}).


cast(Pid,Event,Data) ->
    catch Pid ! {Event,Data}.
cast(Pid,Event,Data1,Data2) ->
    catch Pid ! {Event,Data1,Data2}.


start([O,Q]) ->
    P = spawn(?MODULE,init,[[O,Q]]),
    register(list_to_atom("o0" ++ integer_to_list(O)),P),P;
start([O,Q,S]) ->
    P = spawn(?MODULE,init,[[O,Q,S]]),
    register(list_to_atom("o0" ++ integer_to_list(O)),P),P;
start([O,Q,S,E]) ->
    P = spawn(?MODULE,init,[[O,Q,S,E]]),
    register(list_to_atom("o0" ++ integer_to_list(O)),P),P.


init([O,Q]) ->
    loop([O,Q]);
init([O,Q,S]) when S =:= ready ->
    loop([O,Q]);
init([O,_,S]) when S =:= reboot ->
    Q = restorage(?ETS_TABLE_NAME,if O == 0 -> ?SCORE_OFFSET+1; true -> ?MIN_LIMIT*O+1 end,?MIN_LIMIT*(O+1)),
    loop([O,Q]);
init([O,_,S,E]) when S =:= reboot ->
    Q = restorage(?ETS_TABLE_NAME,if O == 0 -> ?SCORE_OFFSET+1; true -> ?MIN_LIMIT*O+1 end,?MIN_LIMIT*(O+1),E),
    loop([O,Q]).


loop([O,Q]) ->
    receive
        {point,K} ->
            case get(K) of
                undefined ->
                    put(K,?MIN_LIMIT*O+1),
                    if
                        ?SCORE_OFFSET == 0 ->
                            loop([O,Q+1]);
                        true ->
                            loop([O,Q])
                    end;
                C ->
                    put(K,C+1),
                    if
                        (C+1) / (?SCORE_OFFSET+1) == 1 ->
                            loop([O,Q+1]);
                        true ->
                            loop([O,Q])
                    end
            end;
        {cheat,{K,V}} ->
            put(K,V),
            if
                V > ?SCORE_OFFSET ->
                    loop([O,Q+1]);
                true ->
                    loop([O,Q])
            end;
        {reset,K} ->
            V = erase(K),
            if
                V > ?SCORE_OFFSET ->
                    loop([O,Q-1]);
                true ->
                    loop([O,Q])
            end;
        {score,{R,P},{L,U}} ->
            C = if O > 0 orelse (L == ?SCORE_OFFSET+1 andalso U == ?MIN_LIMIT*(O+1)) -> Q; true -> scoring(L,U) end,
            catch P ! {{score,R},C},
            loop([O,Q]);
        {fetch,{R,P},{TID,L,U}} ->
            if Q > 0 -> insert(L,U,TID); true -> skip end,
            catch P ! {{fetch,R},ready},
            loop([O,Q])
    end.
    
scoring(L,U) ->
    put(counter,0.0),
    for(L,U,fun(I) -> put(counter,get(counter) + length(get_keys(I))) end),
    get(counter).

insert(L,U,TID) ->
    for(L,U,fun(I) -> case get_keys(I) of [] -> 1; KL -> ets:insert(TID,{I,KL}) end end).

restorage(T,L,U) ->
    TL = case ets:info(T) of
        undefined -> [];
        _ -> ets:tab2list(T)
    end,
    io:format("+!!!!!TL:~p!!!!!+~n",[TL]),

    put(quantity,0),
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
    ),
    erase(quantity).
restorage(T,L,U,E) ->
    TL = case ets:info(T) of
        undefined -> [];
        _ -> ets:tab2list(T)
    end,
    io:format("+!!!!!TL:~p!!!!!+~n",[TL]),

    put(quantity,0),
    lists:foreach(
        fun({K,V}) ->
            put(K,V),
            put(quantity,get(quantity)+1)
        end,
        lists:filter(
            fun({K,V}) ->
                V >= L andalso V =< U andalso K =/= E
            end,
        TL)
    ),
    erase(quantity).

for(N,N,F) -> F(N);
for(I,N,_) when I > N -> null;
for(I,N,F) -> F(I),for(I+1,N,F).
