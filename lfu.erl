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
            Event =:= state -> event(Event,[]);
            true -> throw(unknow_parameters)
        end.
    event(Event,Data) ->
        if
            Event =:= point ->
                cast(Event,Data);
            Event =:= state ->
                call(Event,Data);
            Event =:= count ->
                call(Event,Data);
            Event =:= fetch; Event =:= reset; Event =:= clean ->
                case ets:info(element(1,Data)) of
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
            {point,{Topic,Key}} ->
                case get({Topic,Key}) of
                    undefined ->
                        N = list_to_atom("o0" ++ integer_to_list(0)),
                        case whereis(N) of
                            undefined ->
                                register(N,spawn(fun() -> s_score_loop(0) end)),
                                catch N ! {point,{Topic,Key}};
                            _ ->
                                catch N ! {point,{Topic,Key}}
                        end,
                        put({Topic,Key},1),
                        ?SUPPORT andalso cast(?SECONDARY,point,{Topic,Key}),
                        loop([O,Q+1]);
                    C ->
                        if
                            %% before MAX LIMIT
                            C div ?MAX_LIMIT == 0 ->
                                if
%%                                  C rem ?MIN_LIMIT == 0 andalso C div ?MIN_LIMIT > 0 ->
                                    C rem ?MIN_LIMIT == 0 ->
                                        N = list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT)),
                                        case whereis(N) of
                                            undefined ->
                                                register(N,spawn(fun() -> s_score_loop(C div ?MIN_LIMIT) end)),
                                                catch N ! {point,{Topic,Key}};
                                            _ ->
                                                catch N ! {point,{Topic,Key}}
                                        end,
                                        catch list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT - 1)) ! {reset,{Topic,Key}};
                                    true ->
                                        N = list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT)),
                                        case whereis(N) of
                                            undefined ->
                                                register(N,spawn(fun() -> s_score_loop(C div ?MIN_LIMIT) end)),
                                                catch N ! {point,{Topic,Key}};
                                            _ ->
                                                catch N ! {point,{Topic,Key}}
                                        end
                                end;
                            %% after MAX LIMIT
%%                          C rem ?MAX_LIMIT == 0 andalso C div ?MAX_LIMIT > 0 ->
                            C rem ?MAX_LIMIT == 0 ->
                                N = list_to_atom("o" ++ integer_to_list(C div ?MAX_LIMIT)),
                                case whereis(N) of
                                    undefined ->
                                        register(N,spawn(fun() -> q_score_loop(C div ?MAX_LIMIT) end)),
                                        catch N ! {point,{Topic,Key}};
                                    _ ->
                                        catch N ! {point,{Topic,Key}}
                                end,
                                if
                                    C div ?MAX_LIMIT > 1 -> catch list_to_atom("o" ++ integer_to_list(C div ?MAX_LIMIT - 1)) ! {reset,{Topic,Key}};
                                    true -> catch list_to_atom("o0" ++ integer_to_list(C div ?MIN_LIMIT - 1)) ! {reset,{Topic,Key}}
                                end;
                            true -> skip
                        end,
                        put({Topic,Key},C+1),
                        ?SUPPORT andalso cast(?SECONDARY,point,{Topic,Key}),
                        loop([O,Q])
                end;
            {count,{Topic,Key},{Ref,PidS}} ->
                catch PidS ! {{count,{Topic,Key},Ref},get({Topic,Key})},
                loop([O,Q]);
            {state,Stub,{Ref,PidS}} ->
                catch PidS ! {{state,Stub,Ref},[O,Q]},
                loop([O,Q]);
            {fetch,{TabID},{Ref,PidS}} ->
                NO = offset(O,Q),
                result(NO,TabID),
                catch PidS ! {{fetch,{TabID},Ref},ready},
                loop([NO,Q]);
            {reset,{TabID},{Ref,PidS}} ->
                NQ = reset(TabID,Q),
                catch PidS ! {{reset,{TabID},Ref},ready},
                loop([O,NQ]);
            {clean,{TabID},{Ref,PidS}} ->
                NO = offset(O,Q),
                result(NO,TabID),
                NQ = reset(TabID,Q),
                ?SUPPORT andalso call(?SECONDARY,reset,{TabID}),
                catch PidS ! {{clean,{TabID},Ref},ready},
                loop([O,NQ])
        end.


    offset(O,Q) ->
        P = counting(0,O),
        C = counting(O,O*10) + P,
        F = counting(O*10,O*100) + C,
        io:format("P:~p~nC:~p~nF:~p~nO:~p~nQ~p~n",[P,C,F,O,Q]),
        if
            Q < 1 -> O;
            C / Q * 100 < ?MIN_OFFSET andalso O*10 =< ?MAX_ORDER andalso F / Q * 100 =< ?MAX_OFFSET -> offset(O*10,Q);
            C / Q * 100 > ?MAX_OFFSET andalso O div 10 >= ?MIN_ORDER andalso P / Q * 100 >= ?MIN_OFFSET -> offset(O div 10,Q);
            true -> O
        end.

    counting(L,O) ->
        put(counter,0),
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
                )),Ref);
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
                )),Ref),

                for(if L div ?MAX_LIMIT > 0 -> L div ?MAX_LIMIT; true -> 1 end,O div ?MAX_LIMIT - 1,
                    fun(I) ->
                        case whereis(list_to_atom("o" ++ integer_to_list(I))) of undefined -> "skip"; N -> catch N ! {score,{Ref,self()}} end
                    end
                ),
                count_loop(length(grep(foreach(if L div ?MAX_LIMIT > 0 -> L div ?MAX_LIMIT; true -> 1 end,O div ?MAX_LIMIT - 1,fun(I) -> I end),
                    fun(I) ->
                        case whereis(list_to_atom("o" ++ integer_to_list(I))) of undefined -> false; _ -> true end
                    end
                )),Ref)
        end,
        erase(counter).

    count_loop(O,R) ->
        if
            O > 0 ->
                receive
                    {{C,R},Reply} when C =:= score ->
                        put(counter,get(counter)+Reply),
                        if O > 1 -> count_loop(O-1,R); true -> "skip" end
                after ?TIMEOUT ->
                    "skip"
                end;
            true -> "skip"
        end.


    q_score_loop(O) ->
        receive
            {point,{Topic,Key}} ->
                case get({Topic,Key}) of
                    undefined ->
                        put({Topic,Key},1),
                        q_score_loop(O);
                    C ->
                        put({Topic,Key},C+1),
                        q_score_loop(O)
                end;
            {reset,{Topic,Key}} ->
                erase({Topic,Key}),
                q_score_loop(O);
            {score,{Ref,PidS}} ->
                C = q_scoring(),
                catch PidS ! {{score,Ref},C},
                q_score_loop(O)
        end.

    s_score_loop(O) ->
        receive
            {point,{Topic,Key}} ->
                case get({Topic,Key}) of
                    undefined ->
                        put({Topic,Key},?MIN_LIMIT*O+1),
                        s_score_loop(O);
                    C ->
                        put({Topic,Key},C+1),
                        s_score_loop(O)
                end;
            {reset,{Topic,Key}} ->
                erase({Topic,Key}),
                s_score_loop(O);
            {score,{Ref,PidS},{L,U}} ->
                C = s_scoring(L,U),
                catch PidS ! {{score,Ref},C},
                s_score_loop(O)
        end.

    q_scoring() ->
        put(counter,0),
        put(counter,get(counter) + length(get_keys(1))),
        get(counter).

    s_scoring(L,U) ->
        put(counter,0),
        for(L,U,fun(I) -> put(counter,get(counter) + length(get_keys(I))) end),
        get(counter).


    reset(TabID,Q) ->
        L = case ets:info(TabID) of
            undefined -> [];
            _ -> ets:tab2list(TabID)
        end,
        lists:foreach(
            fun(K) ->
                C = erase(K),
                if
                    (C - 1) div ?MAX_LIMIT == 0 ->
                        N = list_to_atom("o0" ++ integer_to_list((C-1) div ?MIN_LIMIT)),
                        catch N ! {reset,K};
                    true ->
                        N = list_to_atom("o" ++ integer_to_list((C-1) div ?MAX_LIMIT)),
                        catch N ! {reset,K}
                end
            end,
        L),
        Q - length(L).


    result(O,TabID) ->
        for(1,O*10,fun(I) -> ets:insert(TabID,get_keys(I)) end).


    foreach(N,N,F) -> [F(N)];
    foreach(I,N,_) when I > N -> [];
    foreach(I,N,F) -> [F(I)|foreach(I+1,N,F)].

    for(N,N,F) -> F(N);
    for(I,N,_) when I > N -> null;
    for(I,N,F) -> F(I),for(I+1,N,F).

    grep([H|T],F) -> case F(H) of true -> [H|grep(T,F)]; false -> grep(T,F) end;
    grep([],_) -> [].

    sleep(T) ->
        receive
            after T -> true
        end.
