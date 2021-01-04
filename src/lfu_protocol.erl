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


common(info,{tcp,S,<<"POINT:",P/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    K = break_binary_string(byte_size(P),P),
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
common(info,{tcp,S,<<"CHEAT:",P/binary>>},[S,T]) ->
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
    binary:split(break_binary_string(byte_size(P),P),<<";">>,[global])),
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
common(info,{tcp,S,<<"COUNT:",P/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    K = break_binary_string(byte_size(P),P),
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
common(info,{tcp,S,<<"STATE",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
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
common(info,{tcp,S,<<"STORE",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:store() of
        ok ->
            T:send(S,<<"OK">>);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"SCORE",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:score() of
        ready ->
            T:send(S,<<"READY">>);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"FETCH",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
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
common(info,{tcp,S,<<"CLEAN",":","SYNC",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:clean(sync) of
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
common(info,{tcp,S,<<"CLEAN",":","ASYNC",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:clean(async) of
        TID when is_reference(TID) ->
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
            T:send(S,<<"[",BD/binary,"]">>),
            keep_state_and_data;
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>),
            keep_state_and_data
    end;
common(info,{tcp,S,<<"CLEAN",":",_P/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    T:send(S,<<"{","ERROR",":","EXPIRED_REF","}">>),
    keep_state_and_data;
common(info,{tcp,S,<<"CLEAN",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:clean(async) of
        TID when is_reference(TID) ->
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
            T:send(S,<<"[",BD/binary,"]">>),
            keep_state_and_data;
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>),
            keep_state_and_data
    end;
common(info,{tcp,S,_B},[S,T]) ->
    T:setopts(S,[{active,once}]),
    T:send(S,<<"{","ERROR",":","UNKNOW_COMMAND","}">>),
    keep_state_and_data.

delete(state_timeout,BR,[S,T,#{ref := BR, tid := _T}]) ->
    {next_state,common,[S,T]};
delete(info,{tcp,_S,<<"CLEAN:ASYNC",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"CLEAN:SYNC",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,S,<<"CLEAN",":",P/binary>>},[S,T,#{ref := BR, tid := TID}]) ->
    T:setopts(S,[{active,once}]),
    case break_binary_string(byte_size(P),P) =:= BR of
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
delete(info,{tcp,_S,<<"POINT:",_P/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"CHEAT:",_P/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"COUNT:",_P/binary>>},_StateData) ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"STATE",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"STORE",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"SCORE",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"FETCH",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"CLEAN",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
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
    
break_binary_string(S,B) ->
    case B of
        <<B1:(S-2)/binary,"\r\n">> ->
            B1;
        <<B2:(S-1)/binary,"\n">> ->
            B2;
        <<B3:(S-1)/binary,"\r">> ->
            B3;
        _ ->
            B
    end.
