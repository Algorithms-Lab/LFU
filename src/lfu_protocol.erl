-module(lfu_protocol).
-author('VSolenkov').

-behavior('gen_statem').
-behavior('ranch_protocol').

-export([
    start_link/3
]).
-export([
    init/1,
    callback_mode/0
]).
-export([
    common/3,
    delete/3
]).
-include("include/lfu.hrl").



start_link(R,T,_) ->
    {ok,proc_lib:spawn_link(?MODULE,init,[[R,T]])}.


init([R,T]) ->
    {ok,S} = ranch:handshake(R),
    T:setopts(S,[{active,once},{packet,line}]),
    gen_statem:enter_loop(?MODULE,[],common,[S,T]).

callback_mode() ->
	state_functions.


common(info,{tcp,S,<<"POINT",_:1/binary,P/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    K = break_binary_string(P),
    case lfu:point(K) of
        ok ->
            T:send(S,<<"OK">>);
        "type_error" ->
            T:send(S,<<"{","ERROR",":","TYPE_ERROR","}">>);
        "size_key_error" ->
            T:send(S,<<"{","ERROR",":","MAX_SIZE_KEY","}">>);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"CHEAT",_:1/binary,P/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    KVL = lists:filtermap(
        fun(KV) ->
            case binary:split(KV,<<",">>,[global]) of
                [K,V] ->
                    {true,{K,binary_to_integer(V)}};
                _ ->
                    false
            end
        end,
    binary:split(break_binary_string(P),<<";">>,[global])),
    case lfu:cheat(KVL) of
        ok ->
            T:send(S,<<"OK">>);
        "type_error" ->
            T:send(S,<<"{","ERROR",":","TYPE_ERROR","}">>);
        "data_error" ->
            T:send(S,<<"{","ERROR",":","DATA_ERROR","}">>);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"COUNT",_:1/binary,P/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    K = break_binary_string(P),
    case lfu:count(K) of
        "type_error" ->
            T:send(S,<<"{","ERROR",":","TYPE_ERROR","}">>);
        "size_key_error" ->
            T:send(S,<<"{","ERROR",":","MAX_SIZE_KEY","}">>);
        undefined ->
            T:send(S,<<"{","ERROR",":","UNDEFINED_KEY","}">>);
        C when is_integer(C) ->
            BC = integer_to_binary(C),
            T:send(S,<<BC/binary>>);
        _ ->
           T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"STATE",_/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    case lfu:state() of
        [O,Q] ->
             BO = integer_to_binary(O),
             BQ = integer_to_binary(Q),
             T:send(S,<<"{","O",":",BO/binary,",","Q",":",BQ/binary,"}">>);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"STORE",_/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    case lfu:store() of
        ok ->
            T:send(S,<<"OK">>);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"SCORE",_/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    case lfu:score() of
        ready ->
            T:send(S,<<"READY">>);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"FETCH",_/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    case catch ets:info(lfu:fetch()) of
        I when is_list(I) -> 
            BD = ets:foldl(
                fun({K,V},BA) ->
                    BK = integer_to_binary(K),
                    BV = pack_list_to_binary(V,<<>>),
                    case BA of
                        <<>> ->
                            <<"{",BK/binary,":",BV/binary,"}">>;
                        BA ->
                            <<BA/binary,",","{",BK/binary,":",BV/binary,"}">>
                    end
                end,
            <<>>,proplists:get_value(id,I)),
            T:send(S,<<"[",BD/binary,"]">>);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"CLEAN",_/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    case lfu:clean() of
        {TID,R} when is_reference(TID) andalso is_reference(R) ->
            BD = ets:foldl(
                fun({K,V},BA) ->
                    BK = integer_to_binary(K),
                    BV = pack_list_to_binary(V,<<>>),
                    case BA of
                        <<>> ->
                            <<"{",BK/binary,":",BV/binary,"}">>;
                        BA ->
                            <<BA/binary,",","{",BK/binary,":",BV/binary,"}">>
                    end
                end,
            <<>>,TID),
            BR = list_to_binary(ref_to_list(R)),
            T:send(S,<<"{","[",BD/binary,"]",":",BR/binary,"}">>),
            {next_state,delete,[S,T,#{ref => BR, tid => TID}],[{state_timeout,?TIMEOUT_STATE_DELETE,BR}]};
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>),
            keep_state_and_data
    end;
common(info,{tcp,S,<<"CLEAN",":",_P/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    T:send(S,<<"{","ERROR",":","EXPIRED_REF","}">>),
    keep_state_and_data;
common(info,{tcp,S,_B},[S,T]) ->
    T:setopts(S,[{active,once}]),
    T:send(S,<<"{","ERROR",":","UNKNOW_COMMAND","}">>),
    keep_state_and_data.

delete(state_timeout,BR,[S,T,#{ref := BR, tid := _T}]) ->
    {next_state,common,[S,T]};
delete(info,{tcp,S,<<"CLEAN",_:1/binary,P/binary>>},[S,T,#{ref := BR, tid := TID}]) ->
    T:setopts(S,[{active,once}]),
    case break_binary_string(P) =:= BR of
        true ->
            case lfu:clean(list_to_ref(binary_to_list(BR)),TID) of
                ok ->
                    T:send(S,<<"OK">>),
                    {next_state,common,[S,T]};
                _ ->
                    T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>),
                    {next_state,common,[S,T]}
            end;
        false ->
           T:send(S,<<"{","ERROR",":","UNKNOW_REF","}">>),
           keep_state_and_data
     end;
delete(info,{tcp,_S,<<"POINT",_:1/binary,_P/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"CHEAT",_:1/binary,_P/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"COUNT",_:1/binary,_P/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"STATE",_/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"STORE",_/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"SCORE",_/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"FETCH",_/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"CLEAN",_/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,S,_B},[S,T,_MD]) ->
    T:setopts(S,[{active,once}]),
    T:send(S,<<"{","ERROR",":","UNKNOW_COMMAND","}">>),
    keep_state_and_data.


pack_ets_to_binary([],B) -> <<"[",B/binary,"]">>;
pack_ets_to_binary([{K,V}|T],B) ->
   BC = integer_to_binary(K),
   BV = pack_list_to_binary(V,<<>>),
   case B of
       <<>> ->
           pack_ets_to_binary(T,<<"{",BC/binary,":",BV/binary,"}">>);
       B ->
           pack_ets_to_binary(T,<<B/binary,",","{",BC/binary,":",BV/binary,"}">>)
   end.

pack_list_to_binary([],B) -> <<"[",B/binary,"]">>;
pack_list_to_binary([H|T],B) ->
    case B of
        <<>> -> 
            pack_list_to_binary(T,<<H/binary>>);
        B ->
            pack_list_to_binary(T,<<B/binary,",",H/binary>>)
    end.
    
break_binary_string(B) ->
    hd(binary:split(B,<<"\r\n">>,[global])).
