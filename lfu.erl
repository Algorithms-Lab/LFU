-module(lfu).
-export([
    event/2,
    event/1,
    alg/0,
    for/3
]).
-author('VSolenkov').
-describe('Least Frequently Used').

-define(MIN_LIMIT,10000000).
-define(MAX_LIMIT,1000000000).
-define(MAX_ORDER,100000000000000).
-define(MIN_ORDER,100).
-define(MIN_OFFSET,10).			%% low limit for step to next rank
-define(MAX_OFFSET,30).			%% up limit for step to prev rank
-define(SCORE_OFFSET,0).		%% must be less than ?MIN_ORDER && for example if it`s necessary begin score from 100 then need setting to 99
-define(TIMEOUT,90000).

-ifdef(support).
    -define(SUPPORT,true).
    -define(SECONDARY,kit).
-else.
    -define(SUPPORT,false).
    -define(SECONDARY,any).
-endif.



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
                                register(N,spawn(fun() -> s_score_loop([0,0]) end)),
                                catch N ! {point,K};
                            _ ->
                                catch N ! {point,K}
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
                                                register(N,spawn(fun() -> s_score_loop([C div ?MIN_LIMIT,0]) end)),
                                                catch N ! {point,K};
                                            _ ->
                                                catch N ! {point,K}
                                        end,
                                        catch list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT - 1)) ! {reset,K};
                                    true ->
                                        N = list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT)),
                                        case whereis(N) of
                                            undefined ->
                                                register(N,spawn(fun() -> s_score_loop([C div ?MIN_LIMIT,0]) end)),
                                                catch N ! {point,K};
                                            _ ->
                                                catch N ! {point,K}
                                        end
                                end;
                            %% after MAX LIMIT
%%                          C rem ?MAX_LIMIT == 0 andalso C div ?MAX_LIMIT > 0 ->
                            C rem ?MAX_LIMIT == 0 ->
                                N = list_to_atom("o" ++ integer_to_list(C div ?MAX_LIMIT)),
                                case whereis(N) of
                                    undefined ->
                                        register(N,spawn(fun() -> q_score_loop([C div ?MAX_LIMIT,0]) end)),
                                        catch N ! {point,K};
                                    _ ->
                                        catch N ! {point,K}
                                end,
                                if
                                    C div ?MAX_LIMIT > 1 -> catch list_to_atom("o" ++ integer_to_list(C div ?MAX_LIMIT - 1)) ! {reset,K};
                                    true -> catch list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT - 1)) ! {reset,K}
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
                                        catch list_to_atom("o0" ++ integer_to_list((C-1) div ?MIN_LIMIT)) ! {reset,K};
                                    true ->
                                        catch list_to_atom("o" ++ integer_to_list((C-1) div ?MAX_LIMIT)) ! {reset,K}
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
                                                         register(N,spawn(fun() -> s_score_loop([I,0]) end));
                                                     _ -> skip
                                                 end,
                                                 if
                                                     I == (V-1) div ?MIN_LIMIT ->
                                                         catch N ! {cheat,{K,V}};
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
                                                         register(N,spawn(fun() -> s_score_loop([I,0]) end));
                                                     _ -> skip
                                                 end
                                             end
                                         ),
                                         for(1,(V-1) div ?MAX_LIMIT,
                                             fun(I) ->
				                 N = list_to_atom("o" ++ integer_to_list(I)),
                                                 case whereis(N) of
                                                     undefined ->
                                                         register(N,spawn(fun() -> q_score_loop([I,0]) end));
                                                     _ -> skip
                                                 end,
                                                 if
                                                     I == (V-1) div ?MAX_LIMIT ->
                                                         catch N ! {point,K};
                                                     true -> skip
                                                 end
                                             end
                                         )
                                 end;
                             true -> skip
                         end,
                         put(clean,0),
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
            {count,K,{Ref,PidS}} ->
                catch PidS ! {{count,K,Ref},get(K)},
                loop([O,Q]);
            {state,Stub,{Ref,PidS}} ->
                catch PidS ! {{state,Stub,Ref},[O,Q]},
                loop([O,Q]);
            {score,Stub,{Ref,PidS}} ->
                NO = offset(O,Q,null,null,null),
                catch PidS ! {{score,Stub,Ref},ready},
                loop([NO,Q]);
            {fetch,TabID,{Ref,PidS}} ->
                NO = offset(O,Q,null,null,null),
                collect(NO,TabID),
                catch PidS ! {{fetch,TabID,Ref},ready},
                loop([NO,Q]);
            {reset,TabID,{Ref,PidS}} ->
                NQ = reset(TabID,Q),
                catch PidS ! {{reset,TabID,Ref},ready},
                loop([O,NQ]);
            {reset,TabID} ->
                NQ = reset(TabID,Q),
                loop([O,NQ]);
            {clean,TabID,{Ref,PidS}} ->
                NO = offset(O,Q,null,null,null),
                collect(NO,TabID),
                catch PidS ! {{clean,TabID,Ref},ready},
                clean_loop([NO,Q,TabID])
        end.

    clean_loop([O,Q,TabID]) ->
        receive
            {apply,TabID} ->
                NQ = reset(TabID,Q),
                ?SUPPORT andalso cast(?SECONDARY,reset,TabID),
                loop([O,NQ]);
            {apply,TabID,{Ref,PidS}} ->
                NQ = reset(TabID,Q),
                ?SUPPORT andalso call(?SECONDARY,reset,TabID),
                catch PidS ! {{apply,TabID,Ref},ready},
                loop([O,NQ])
        after ?TIMEOUT ->
            loop([O,Q])
        end.


    offset(O,Q,P0,C0,F0) ->
        P = if P0 =/= null -> P0; true -> counting(?SCORE_OFFSET,O) end,
        C = if C0 =/= null -> C0; true -> counting(O,O*10) + P end,
        F = if F0 =/= null -> F0; true -> counting(O*10,O*100) + C end,
        io:format("P:~p~nC:~p~nF:~p~nO:~p~nQ~p~n",[P,C,F,O,Q]),
        if
            Q < 1 -> O;
            C / Q * 100 < ?MIN_OFFSET andalso O*10 =< ?MAX_ORDER andalso F / Q * 100 =< ?MAX_OFFSET -> offset(O*10,Q,C,F,null);
            C / Q * 100 > ?MAX_OFFSET andalso O div 10 >= ?MIN_ORDER andalso P / Q * 100 >= ?MIN_OFFSET -> offset(O div 10,Q,null,P,C);
            true -> O
        end.

    counting(L,O) ->
        put(counter,0.0),
        Ref = make_ref(),
        if
            O =< ?MAX_LIMIT ->
                for(L div ?MIN_LIMIT,(O-1) div ?MIN_LIMIT,
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                            undefined -> "skip";
                            N ->
                                catch N ! { score,{Ref,self()},
                                    if
                                        O >= ?MIN_LIMIT*(I+1) ->
                                            if
                                                I == 0 ->
                                                    {L+1,?MIN_LIMIT*(I+1)};
                                                true ->
                                                    {?MIN_LIMIT*I+1,?MIN_LIMIT*(I+1)}
                                            end;
                                        true ->
                                            {L+1,O}
                                    end
                                }
                        end
                    end
                ),
                count_loop(length(grep(foreach(L div ?MIN_LIMIT,(O-1) div ?MIN_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,score);
            true ->
                for(L div ?MIN_LIMIT,(?MAX_LIMIT-1) div ?MIN_LIMIT,
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of
                            undefined -> "skip";
                            N ->
                                catch N ! { score,{Ref,self()},
                                    if
                                        O >= ?MIN_LIMIT*(I+1) ->
                                            if
                                                I == 0 ->
                                                    {L+1,?MIN_LIMIT*(I+1)};
                                                true ->
                                                    {?MIN_LIMIT*I+1,?MIN_LIMIT*(I+1)}
                                            end;
                                        true ->	%% never hit in this branch
                                            {L+1,O}
                                    end
                                }
                        end
                    end
                ),
                count_loop(length(grep(foreach(L div ?MIN_LIMIT,(?MAX_LIMIT-1) div ?MIN_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,score),

                for(if L div ?MAX_LIMIT > 0 -> L div ?MAX_LIMIT; true -> 1 end,O div ?MAX_LIMIT - 1,
                    fun(I) ->
                        case whereis(list_to_atom("o" ++ integer_to_list(I))) of undefined -> "skip"; N -> catch N ! {score,{Ref,self()}} end
                    end
                ),
                count_loop(length(grep(foreach(if L div ?MAX_LIMIT > 0 -> L div ?MAX_LIMIT; true -> 1 end,O div ?MAX_LIMIT - 1,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,score)
        end,
        list_to_integer(float_to_list(erase(counter),[{decimals,0}])).

    count_loop(O,R,C) ->
        if
            O > 0 ->
                receive
                    {{C,R},Reply} when C =:= score ->
                        put(counter,get(counter)+Reply),
                        if O > 1 -> count_loop(O-1,R,C); true -> "skip" end;
                    {{C,R},ready} when C =:= fetch ->
                        if O > 1 -> count_loop(O-1,R,C); true -> "skip" end
                after ?TIMEOUT ->
                    "skip"
                end;
            true -> "skip"
        end.


    q_score_loop([O,Q]) ->
        receive
            {point,K} ->
                case get(K) of
                    undefined ->
                        put(K,1),
                        q_score_loop([O,Q+1]);
                    C ->
                        put(K,C+1),
                        q_score_loop([O,Q])
                end;
            {reset,K} ->
                erase(K),
                q_score_loop([O,Q-1]);
            {score,{Ref,PidS}} ->
                C = Q,
                catch PidS ! {{score,Ref},C},
                q_score_loop([O,Q]);
            {fetch,{Ref,PidS},TabID} ->
                if Q > 0 -> q_insert(O,TabID); true -> skip end,
                catch PidS ! {{fetch,Ref},ready},
                q_score_loop([O,Q])
        end.

    s_score_loop([O,Q]) ->
        receive
            {point,K} ->
                case get(K) of
                    undefined ->
                        put(K,?MIN_LIMIT*O+1),
                        if
                            ?SCORE_OFFSET == 0 ->
                                s_score_loop([O,Q+1]);
                            true ->
                                s_score_loop([O,Q])
                        end;
                    C ->
                        put(K,C+1),
                        if
                            (C+1) / (?SCORE_OFFSET+1) == 1 ->
                                s_score_loop([O,Q+1]);
                            true ->
                                s_score_loop([O,Q])
                        end
                end;
            {cheat,{K,V}} ->
                put(K,V),
                if
                    V > ?SCORE_OFFSET ->
                        s_score_loop([O,Q+1]);
                    true ->
                        s_score_loop([O,Q])
                end;
            {reset,K} ->
                erase(K),
                s_score_loop([O,Q-1]);
            {score,{Ref,PidS},{L,U}} ->
                C = if O > 0 orelse (L == ?MIN_LIMIT*O+1 andalso U == ?MIN_LIMIT*(O+1)) -> Q; true -> s_scoring(L,U) end,
                catch PidS ! {{score,Ref},C},
                s_score_loop([O,Q]);
            {fetch,{Ref,PidS},{TabID,L,U}} ->
                if Q > 0 -> s_insert(L,U,TabID); true -> skip end,
                catch PidS ! {{fetch,Ref},ready},
                s_score_loop([O,Q])
        end.


    s_scoring(L,U) ->
        put(counter,0.0),
        for(L,U,fun(I) -> put(counter,get(counter) + length(get_keys(I))) end),
        get(counter).

    q_insert(I,TabID) ->
        case get_keys(1) of [] -> 1; KL -> ets:insert(TabID,{I*?MAX_LIMIT,KL}) end.

    s_insert(L,U,TabID) ->
        for(L,U,fun(I) -> case get_keys(I) of [] -> 1; KL -> ets:insert(TabID,{I,KL}) end end).

    reset(TabID,Q) ->
        TL = case ets:info(TabID) of
            undefined -> [];
            _ -> ets:tab2list(TabID)
        end,
        put(reset,0),
        lists:foreach(
            fun({_,KL}) ->
                lists:foreach(
                    fun(K) ->
                        C = erase(K),
                        if
                            (C - 1) div ?MAX_LIMIT == 0 ->
                                N  = list_to_atom("o0" ++ integer_to_list((C-1) div ?MIN_LIMIT)),
                                catch N ! {reset,K};
                            true ->
                                N = list_to_atom("o" ++ integer_to_list((C-1) div ?MAX_LIMIT)),
                                catch N ! {reset,K}
                        end,
                        put(reset,get(reset)+1)
                    end,
                KL)
            end,
        TL),
        Q - erase(reset).


    collect(O,TabID) ->
        Ref = make_ref(),
        if
            (O*10-1) div ?MAX_LIMIT == 0 ->
                for(0,(O*10-1) div ?MIN_LIMIT,
                    fun(I) ->
                        N = list_to_atom("o0" ++ integer_to_list(I)),
                        case whereis(N) of
                            undefined -> skip;
                            _ ->
                                catch N ! {fetch,{Ref,self()},{TabID,
                                    if
                                        I == 0 ->
                                            ?SCORE_OFFSET+1;
                                        true -> ?MIN_LIMIT*I+1
                                    end,
                                    if
                                        O*10 >= ?MIN_LIMIT*(I+1) ->
                                            ?MIN_LIMIT*(I+1);
                                        true -> O*10
                                    end
                                }}
                        end
                    end
                ),
                count_loop(length(grep(foreach(0,(O*10-1) div ?MIN_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,fetch);
            true ->
                for(0,(?MAX_LIMIT-1) div ?MIN_LIMIT,
                    fun(I) ->
                        N = list_to_atom("o0" ++ integer_to_list(I)),
                        case whereis(N) of
                            undefined -> skip;
                            _ ->
                                catch N ! {fetch,{Ref,self()},{TabID,
                                    if
                                        I == 0 ->
                                            ?SCORE_OFFSET+1;
                                        true -> ?MIN_LIMIT*I+1
                                    end,
                                    if
                                        O*10 >= ?MIN_LIMIT*(I+1) ->
                                            ?MIN_LIMIT*(I+1);
                                        true ->	O*10 %% never hit in this branch
                                    end
                                }}
                        end
                    end
                ),
                count_loop(length(grep(foreach(0,(?MAX_LIMIT-1) div ?MIN_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o0" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,fetch),

                for(1,(O*10-1) div ?MAX_LIMIT,
                    fun(I) ->
                        N = list_to_atom("o" ++ integer_to_list(I)),
                        case whereis(N) of
                            undefined -> skip;
                            _ ->
                                catch N ! {fetch,{Ref,self()},TabID}
                        end
                    end
                ),
                count_loop(length(grep(foreach(1,(O*10-1) div ?MAX_LIMIT,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref,fetch)
        end.

    foreach(N,N,F) -> [F(N)];
    foreach(I,N,_) when I > N -> [];
    foreach(I,N,F) -> [F(I)|foreach(I+1,N,F)].

    for(N,N,F) -> F(N);
    for(I,N,_) when I > N -> null;
    for(I,N,F) -> F(I),for(I+1,N,F).

    grep([H|T],F) -> case F(H) of true -> [H|grep(T,F)]; false -> grep(T,F) end;
    grep([],_) -> [].
