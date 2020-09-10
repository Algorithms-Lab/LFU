-module(lfu_quick_score).
-author('VSolenkov').

-export([
    start/1,
    point/2,
    reset/2,
    score/3,
    fetch/4
]).

-export([
    init/1
]).
-include("include/lfu.hrl").


point(N,K) ->
    cast(N,point,K).
reset(N,K) ->
    cast(N,reset,K).
score(N,R,P) ->
    cast(N,score,{R,P}).
fetch(N,R,P,T) ->
    cast(N,fetch,{R,P},T).


cast(Pid,Event,Data) ->
    catch Pid ! {Event,Data}.
cast(Pid,Event,Data1,Data2) ->
    catch Pid ! {Event,Data1,Data2}.


start([O,Q]) ->
    register(list_to_atom("o" ++ integer_to_list(O)),spawn(?MODULE,init,[[O,Q]]));
start([O,Q,S]) ->
    register(list_to_atom("o" ++ integer_to_list(O)),spawn(?MODULE,init,[[O,Q,S]])).


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
                    put(K,1),
                    loop([O,Q+1]);
                 C ->
                     put(K,C+1),
                     loop([O,Q])
            end;
        {reset,K} ->
            erase(K),
            loop([O,Q-1]);
        {score,{R,P}} ->
            catch P ! {{score,R},Q},
            loop([O,Q]);
        {fetch,{R,P},TID} ->
            if Q > 0 -> insert(O,TID); true -> skip end,
            catch P ! {{fetch,R},ready},
            loop([O,Q])
    end.

insert(I,TID) ->
    case get_keys(1) of [] -> 1; KL -> ets:insert(TID,{I*?MAX_LIMIT,KL}) end.
