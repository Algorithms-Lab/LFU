-module(lfu_quick_score).
-author('VSolenkov').

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
-include("include/lfu.hrl").


point(N,K) ->
    cast(N,point,K).
reset(N,K) ->
    cast(N,reset,K).
cheat(N,K,V) ->
    cast(N,cheat,{K,V}).
score(N,R,P) ->
    cast(N,score,{R,P}).
fetch(N,R,P,T) ->
    cast(N,fetch,{R,P},T).
state(N) ->
    call(N,state,[]).

call(Name,Event,Data) ->
    Ref = erlang:monitor(process,Name),
    catch whereis(Name) ! {Event,Data,{Ref,self()}},
    receive
        {{Event,Data,Ref},Reply} -> erlang:demonitor(Ref,[flush]),Reply;
        {'DOWN',Ref,proceess,_Name,_Reason} -> {error,no_proc};
        Other -> io:format("Other:~p~n",[Other])
    end.

cast(Pid,Event,Data) ->
    catch Pid ! {Event,Data}.
cast(Pid,Event,Data1,Data2) ->
    catch Pid ! {Event,Data1,Data2}.


start([O,Q]) ->
    P = spawn(?MODULE,init,[[O,Q]]),
    register(list_to_atom("o" ++ integer_to_list(O)),P),P;
start([O,Q,S]) ->
    P = spawn(?MODULE,init,[[O,Q,S]]),
    register(list_to_atom("o" ++ integer_to_list(O)),P),P;
start([O,Q,S,E]) ->
    P = spawn(?MODULE,init,[[O,Q,S,E]]),
    register(list_to_atom("o" ++ integer_to_list(O)),P),P.


init([O,Q]) ->
    loop([O,Q]);
init([O,Q,S]) when S =:= ready ->
    loop([O,Q]);
init([O,_,S]) when S =:= reboot ->
    Q = restorage(?ETS_TABLE_NAME,?MAX_LIMIT*O+1,?MAX_LIMIT*(O+1)),
    loop([O,Q]);
init([O,_,S,E]) when S =:= reboot ->
    Q = restorage(?ETS_TABLE_NAME,?MAX_LIMIT*O+1,?MAX_LIMIT*(O+1),E),
    loop([O,Q]).


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
        {cheat,{K,V}} ->
            put(K,V),
            loop([O,Q+1]);
        {reset,K} ->
            erase(K),
            loop([O,Q-1]);
        {score,{R,P}} ->
            catch P ! {{score,R},Q},
            loop([O,Q]);
        {state,S,{R,P}} ->
            catch P ! {{state,S,R},[O,Q]},
            loop([O,Q]);
        {fetch,{R,P},TID} ->
            if Q > 0 -> insert(O,TID); true -> skip end,
            catch P ! {{fetch,R},ready},
            loop([O,Q])
    end.

insert(I,TID) ->
    case get_keys(1) of [] -> 1; KL -> ets:insert(TID,{I*?MAX_LIMIT,KL}) end.

restorage(T,L,U) ->
    TL = case ets:info(T) of
        undefined -> [];
        _ -> ets:tab2list(T)
    end,
    io:format("+!!!!!TL:~p!!!!!+~n",[TL]),

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
restorage(T,L,U,E) ->
    TL = case ets:info(T) of
        undefined -> [];
        _ -> ets:tab2list(T)
    end,
    io:format("+!!!!!TL:~p!!!!!+~n",[TL]),

    put(quantity,0),
    lists:foreach(
        fun({K,_}) ->
            put(K,1),
            put(quantity,get(quantity)+1)
        end,
        lists:filter(
            fun({K,V}) ->
                V >= L andalso V =< U andalso K =/= E
            end,
        TL)
    ),
    erase(quantity).
