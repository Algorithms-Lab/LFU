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
    register(list_to_atom("o0" ++ integer_to_list(O)),spawn(?MODULE,init,[[O,Q]]));
start([O,Q,S]) ->
    register(list_to_atom("o0" ++ integer_to_list(O)),spawn(?MODULE,init,[[O,Q,S]])).


init([O,Q]) ->
    loop([O,Q]);
init([O,Q,S]) when S =:= ready ->
    loop([O,Q]);
init([O,_,S]) when S =:= reboot ->
    NQ = 0, %% recovery function
    loop([O,NQ]).

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

for(N,N,F) -> F(N);
for(I,N,_) when I > N -> null;
for(I,N,F) -> F(I),for(I+1,N,F).
