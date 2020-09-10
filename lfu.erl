-module(lfu).
-export([
    event/2,
    event/1,
    alg/0,
    for/3
]).
-author('VSolenkov').
-describe('Least Frequently Used').
-include("include/lfu.hrl").


    event(Event) ->
        if
            Event =:= state ->
                event(Event,[]);
            Event =:= score ->
                event(Event,[]);
            true -> throw(unknow_parameters)
        end.
    event(Event,Data) ->
        if
            Event =:= point ->
                cast(Event,Data);
            Event =:= state ->
                call(Event,Data);
            Event =:= score ->
                call(Event,Data);
            Event =:= count ->
                call(Event,Data);
            Event =:= cheat ->
                case is_list(Data) of
                    true -> cast(Event,Data);
                    _ -> throw(must_be_list)
                end;
            Event =:= apply ->
                case ets:info(Data) of
                    undefined -> throw(unknow_table);
                    _ -> cast(Event,Data)
                end;
            Event =:= fetch; Event =:= reset; Event =:= clean ->
                case ets:info(Data) of
                    undefined -> throw(unknow_table);
                    _ -> call(Event,Data)
                end;
            true -> throw(unknow_event)
        end.

    cast(Event,Data) ->
        cast(?MODULE,Event,Data).
    cast(Module,Event,Data) ->
        catch whereis(Module) ! {Event,Data}.

    call(Event,Data) ->
        call(?MODULE,Event,Data).
    call(Module,Event,Data) ->
        Ref = erlang:monitor(process,Module),
        catch whereis(Module) ! {Event,Data,{Ref,self()}},
        receive
            {{Event,Data,Ref},Reply} -> erlang:demonitor(Ref,[flush]),Reply;
            {'DOWN',Ref,proceess,_Name,_Reason} -> {error,no_proc};
            Other -> io:format("Other:~p~n",[Other])
        end.

    alg() ->
        register(?MODULE,spawn(fun loop/0)),
        ?SUPPORT andalso apply(?SECONDARY,alg,[]).

    %% Least Frequently Used
    loop() -> loop([?MIN_ORDER,0]).
    loop([O,Q]) ->
        receive
            {point,K} ->
                case get(K) of
                    undefined ->
                        N = list_to_atom("o0" ++ integer_to_list(0)),
                        case whereis(N) of
                            undefined ->
                                lfu_simple_score:start([0,0]),
                                lfu_simple_score:point(N,K);
                            _ ->
                                lfu_simple_score:point(N,K)
                        end,
                        put(K,1),
                        ?SUPPORT andalso cast(?SECONDARY,point,K),
                        loop([O,if ?SCORE_OFFSET == 0 -> Q+1; true -> Q end]);
                    C when C < ?MAX_ORDER ->
                        if
                            %% before MAX LIMIT
                            C div ?MAX_LIMIT == 0 ->
                                if
%%                                  C rem ?MIN_LIMIT == 0 andalso C div ?MIN_LIMIT > 0 ->
                                    C rem ?MIN_LIMIT == 0 ->
                                        N = list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT)),
                                        case whereis(N) of
                                            undefined ->
                                                lfu_simple_score:start([C div ?MIN_LIMIT,0]),
                                                lfu_simple_score:point(N,K);
                                            _ ->
                                                lfu_simple_score:point(N,K)
                                        end,
                                        lfu_simple_score:reset(list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT - 1)),K);
                                    true ->
                                        N = list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT)),
                                        case whereis(N) of
                                            undefined ->
                                                lfu_simple_score:start([C div ?MIN_LIMIT,0]),
                                                lfu_simple_score:point(N,K);
                                            _ ->
                                                lfu_simple_score:point(N,K)
                                        end
                                end;
                            %% after MAX LIMIT
%%                          C rem ?MAX_LIMIT == 0 andalso C div ?MAX_LIMIT > 0 ->
                            C rem ?MAX_LIMIT == 0 ->
                                N = list_to_atom("o" ++ integer_to_list(C div ?MAX_LIMIT)),
                                case whereis(N) of
                                    undefined ->
                                        lfu_quick_score:start([C div ?MAX_LIMIT,0]),
                                        lfu_quick_score:point(N,K);
                                    _ ->
                                        lfu_quick_score:point(N,K)
                                end,
                                if
                                    C div ?MAX_LIMIT > 1 -> lfu_quick_score:reset(list_to_atom("o" ++ integer_to_list(C div ?MAX_LIMIT - 1)),K);
                                    true -> lfu_simple_score:reset(list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT - 1)),K)
                                end;
                            true -> skip
                        end,
                        put(K,C+1),
                        ?SUPPORT andalso cast(?SECONDARY,point,K),
                        loop([O,if (C+1) / (?SCORE_OFFSET+1) == 1 -> Q+1; true -> Q end]);
                    _ ->
                        ?SUPPORT andalso cast(?SECONDARY,point,K),
                        loop([O,Q])
                end;
            {cheat,KVL} ->
                loop([O,length(lists:filter(
                    fun({K,V}) when V =< ?MAX_ORDER ->
                        case get(K) of
                            undefined -> skip;
                            C ->
                                if
                                    (C-1) div ?MAX_LIMIT == 0 ->
                                        lfu_simple_score:reset(list_to_atom("o0" ++ integer_to_list((C-1) div ?MIN_LIMIT)),K);
                                    true ->
                                        lfu_quick_score:reset(list_to_atom("o" ++ integer_to_list((C-1) div ?MAX_LIMIT)),K)
                                end
                         end,
                         if
                             V > 0 ->
                                 if
                                     (V-1) div ?MAX_LIMIT == 0 ->
                                         for(0,(V-1) div ?MIN_LIMIT,
                                             fun(I) ->
                                                 N = list_to_atom("o0" ++ integer_to_list(I)),
                                                 case whereis(N) of
                                                     undefined ->
                                                         lfu_simple_score:start([I,0]);
                                                     _ -> skip
                                                 end,
                                                 if
                                                     I == (V-1) div ?MIN_LIMIT ->
                                                         lfu_simple_score:cheat(N,K,V);
                                                     true -> skip
                                                 end
                                             end
                                         );
                                     true ->
                                         for(0,(?MAX_LIMIT-1) div ?MIN_LIMIT,
                                             fun(I) ->
                                                 N = list_to_atom("o0" ++ integer_to_list(I)),
                                                 case whereis(N) of
                                                     undefined ->
                                                         lfu_simple_score:start([I,0]);
                                                     _ -> skip
                                                 end
                                             end
                                         ),
                                         for(1,(V-1) div ?MAX_LIMIT,
                                             fun(I) ->
				                 N = list_to_atom("o" ++ integer_to_list(I)),
                                                 case whereis(N) of
                                                     undefined ->
                                                         lfu_quick_score:start([I,0]);
                                                     _ -> skip
                                                 end,
                                                 if
                                                     I == (V-1) div ?MAX_LIMIT ->
                                                         lfu_quick_score:point(N,K);
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
                                         if
                                             OV =< ?SCORE_OFFSET andalso V > ?SCORE_OFFSET -> true;
                                             true -> false
                                         end;
                                     true ->
                                         if
                                             OV > ?SCORE_OFFSET ->
                                                 erase(K),put(clean,get(clean)+1),false;
                                             true ->
                                                 erase(K),false
                                         end
                                 end
                         end;
                    (_) ->
                         false
                    end,
                KVL))+Q-erase(clean)]);
            {count,K,{Ref,Pid}} ->
                catch Pid ! {{count,K,Ref},get(K)},
                loop([O,Q]);
            {state,Stub,{Ref,Pid}} ->
                catch Pid ! {{state,Stub,Ref},[O,Q]},
                loop([O,Q]);
            {score,Stub,{Ref,Pid}} ->
                NO = offset(O,Q,null,null,null),
                catch Pid ! {{score,Stub,Ref},ready},
                loop([NO,Q]);
            {fetch,TID,{Ref,Pid}} ->
                NO = offset(O,Q,null,null,null),
                collect(NO*10,TID),
                catch Pid ! {{fetch,TID,Ref},ready},
                loop([NO,Q]);
            {reset,TID,{Ref,Pid}} ->
                NQ = reset(TID,Q),
                catch Pid ! {{reset,TID,Ref},ready},
                loop([O,NQ]);
            {reset,TID} ->
                NQ = reset(TID,Q),
                loop([O,NQ]);
            {clean,TID,{Ref,Pid}} ->
                NO = offset(O,Q,null,null,null),
                collect(NO*10,TID),
                catch Pid ! {{clean,TID,Ref},ready},
                clean_loop([NO,Q,TID]);
            {apply,_TID} ->
                loop([O,Q]);
            {apply,_TID,{_Ref,_PidID}} ->
                loop([O,Q])
        end.

    clean_loop([O,Q,TID]) ->
        receive
            {apply,TID} ->
                NQ = reset(TID,Q),
                ?SUPPORT andalso cast(?SECONDARY,reset,TID),
                loop([O,NQ]);
            {apply,TID,{Ref,Pid}} ->
                NQ = reset(TID,Q),
                ?SUPPORT andalso call(?SECONDARY,reset,TID),
                catch Pid ! {{apply,TID,Ref},ready},
                loop([O,NQ])
        after ?TIMEOUT_CLEAN ->
            loop([O,Q])
        end.


    offset(O,Q,P0,C0,F0) ->
        P = if P0 =/= null -> P0; true -> count(?SCORE_OFFSET,O) end,
        C = if C0 =/= null -> C0; true -> count(O,O*10) + P end,
        F = if F0 =/= null -> F0; true -> count(O*10,O*100) + C end,
        io:format("P:~p~nC:~p~nF:~p~nO:~p~nQ~p~n",[P,C,F,O,Q]),
        if
            Q < 1 -> O;
            C / Q * 100 < ?MIN_OFFSET andalso O*10 =< ?MAX_ORDER andalso F / Q * 100 =< ?MAX_OFFSET -> offset(O*10,Q,C,F,null);
            C / Q * 100 > ?MAX_OFFSET andalso O div 10 >= ?MIN_ORDER andalso P / Q * 100 >= ?MIN_OFFSET -> offset(O div 10,Q,null,P,C);
            true -> O
        end.

    count(L,U) ->
        put(counter,0.0),
        Ref = make_ref(),
        if
            U =< ?MAX_LIMIT ->
                for(L div ?MIN_LIMIT,(U-1) div ?MIN_LIMIT,
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                            undefined -> "skip";
                            N ->
                                if
                                    U >= ?MIN_LIMIT*(I+1) ->
                                        if
                                            I == 0 ->
                                                lfu_simple_score:score(N,Ref,self(),L+1,?MIN_LIMIT*(I+1));
                                            true ->
                                                lfu_simple_score:score(N,Ref,self(),?MIN_LIMIT*I+1,?MIN_LIMIT*(I+1))
                                        end;
                                    true ->
                                        lfu_simple_score:score(N,Ref,self(),L+1,U)
                                end
%                               if
%                                   I == 0 ->
%                                       if
%                                           U > ?MIN_LIMIT*(I+1) ->
%                                               lfu_simple_score:score(N,Ref,self(),L+1,?MIN_LIMIT*(I+1));
%                                           true ->
%                                               lfu_simple_score:score(N,Ref,self(),L+1,U)
%                                       end;
%                                   true ->
%                                       lfu_simple_score:score(N,Ref,self(),?MIN_LIMIT*I+1,?MIN_LIMIT*(I+1))
%                               end
                        end
                    end
                ),
                count_loop([length(grep(foreach(L div ?MIN_LIMIT,(U-1) div ?MIN_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,score]);
            true ->
                for(L div ?MIN_LIMIT,(?MAX_LIMIT-1) div ?MIN_LIMIT,
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                            undefined -> "skip";
                            N ->
                                if
                                    U >= ?MIN_LIMIT*(I+1) ->
                                        if
                                            I == 0 ->
                                                lfu_simple_score:score(N,Ref,self(),L+1,?MIN_LIMIT*(I+1));
                                            true ->
                                                lfu_simple_score:score(N,Ref,self(),?MIN_LIMIT*I+1,?MIN_LIMIT*(I+1))
                                        end;
                                    true ->	%% never hit in this branch
                                        lfu_simple_score:score(N,Ref,self(),L+1,U)
                                end
%                               if
%                                   I == 0 ->
%                                       lfu_simple_score:score(N,Ref,self(),L+1,?MIN_LIMIT*(I+1));
%                                   true ->
%                                       lfu_simple_score:score(N,Ref,self(),?MIN_LIMIT*I+1,?MIN_LIMIT*(I+1))
%                               end
                        end
                    end
                ),
                count_loop([length(grep(foreach(L div ?MIN_LIMIT,(?MAX_LIMIT-1) div ?MIN_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,score]),

                for(if L div ?MAX_LIMIT > 0 -> L div ?MAX_LIMIT; true -> 1 end,U div ?MAX_LIMIT - 1,
                    fun(I) ->
                        case whereis(list_to_atom("o" ++ integer_to_list(I))) of undefined -> "skip"; N -> lfu_quick_score:score(N,Ref,self()) end
                    end
                ),
                count_loop([length(grep(foreach(if L div ?MAX_LIMIT > 0 -> L div ?MAX_LIMIT; true -> 1 end,U div ?MAX_LIMIT - 1,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,score])
        end,
        list_to_integer(float_to_list(erase(counter),[{decimals,0}])).

    count_loop([Q,R,C]) ->
        if
            Q > 0 ->
                receive
                    {{C,R},Reply} when C =:= score ->
                        put(counter,get(counter)+Reply),
                        if Q > 1 -> count_loop([Q-1,R,C]); true -> "skip" end;
                    {{C,R},ready} when C =:= fetch ->
                        if Q > 1 -> count_loop([Q-1,R,C]); true -> "skip" end
                after ?TIMEOUT_COUNT ->
                    io:format("!!!TIMEOUT!!!~nQ:~pC:~p~n",[Q,C]),
                    "skip"
                end;
            true -> "skip"
        end.

    reset(TID,Q) ->
        TL = case ets:info(TID) of
            undefined -> [];
            _ -> ets:tab2list(TID)
        end,
        put(reset,0),
        lists:foreach(
            fun({_,KL}) ->
                lists:foreach(
                    fun(K) ->
                        C = erase(K),
                        if
                            (C-1) div ?MAX_LIMIT == 0 ->
                                N  = list_to_atom("o0" ++ integer_to_list((C-1) div ?MIN_LIMIT)),
                                lfu_simple_score:reset(N,K);
                            true ->
                                N = list_to_atom("o" ++ integer_to_list((C-1) div ?MAX_LIMIT)),
                                lfu_quick_score:reset(N,K)
                        end,
                        put(reset,get(reset)+1)
                    end,
                KL)
            end,
        TL),
        Q - erase(reset).


    collect(O,TID) ->
        Ref = make_ref(),
        if
            (O-1) div ?MAX_LIMIT == 0 ->
                for(0,(O-1) div ?MIN_LIMIT,
                    fun(I) ->
                        N = list_to_atom("o0" ++ integer_to_list(I)),
                        case whereis(N) of
                            undefined -> skip;
                            _ ->
                                lfu_simple_score:fetch(N,Ref,self(),TID,
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
                count_loop([length(grep(foreach(0,(O-1) div ?MIN_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,fetch]);
            true ->
                for(0,(?MAX_LIMIT-1) div ?MIN_LIMIT,
                    fun(I) ->
                        N = list_to_atom("o0" ++ integer_to_list(I)),
                        case whereis(N) of
                            undefined -> skip;
                            _ ->
                                lfu_simple_score:fetch(N,Ref,self(),TID,
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
                count_loop([length(grep(foreach(0,(?MAX_LIMIT-1) div ?MIN_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,fetch]),

                for(1,(O-1) div ?MAX_LIMIT,
                    fun(I) ->
                        N = list_to_atom("o" ++ integer_to_list(I)),
                        case whereis(N) of
                            undefined -> skip;
                            _ ->
                                lfu_quick_score:fetch(N,Ref,self(),TID)
                        end
                    end
                ),
                count_loop([length(grep(foreach(1,(O-1) div ?MAX_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,fetch])
        end.

    foreach(N,N,F) -> [F(N)];
    foreach(I,N,_) when I > N -> [];
    foreach(I,N,F) -> [F(I)|foreach(I+1,N,F)].

    for(N,N,F) -> F(N);
    for(I,N,_) when I > N -> null;
    for(I,N,F) -> F(I),for(I+1,N,F).

    grep([H|T],F) -> case F(H) of true -> [H|grep(T,F)]; false -> grep(T,F) end;
    grep([],_) -> [].
