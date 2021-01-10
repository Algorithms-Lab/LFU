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
                    case catch binary_to_integer(V) of
                        {'EXIT',_} -> false;
                        I -> {true,{K,I}}
                    end;
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
        [L,M,Q] ->
             BL = integer_to_binary(L),
             BM = integer_to_binary(M),
             BQ = integer_to_binary(Q),
             T:send(S,<<"{","L",":",BL/binary,",","M",":",BM/binary,",","Q",":",BQ/binary,"}">>);
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
common(info,{tcp,S,<<"FETCH",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:fetch() of
        {C,KL} when is_list(KL) ->
            BC = integer_to_binary(C),
            BK = pack_list_to_binary(KL,<<>>),
            BD = <<"{",BC/binary,":",BK/binary,"}">>,
            T:send(S,BD);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"CLEAN:SYNC",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:clean(sync) of
        {{C,KL},R} when is_list(KL) andalso is_reference(R) ->
            BC = integer_to_binary(C),
            BK = pack_list_to_binary(KL,<<>>),
            BD = <<"{",BC/binary,":",BK/binary,"}">>,
            BR = list_to_binary(ref_to_list(R)),
            T:send(S,<<"{","[",BD/binary,"]",":",BR/binary,"}">>),
            {next_state,delete,[S,T,#{ref => BR, key => {C,KL}}],[{state_timeout,?TIMEOUT_STATE_DELETE,BR}]};
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>),
            keep_state_and_data
    end;
common(info,{tcp,S,<<"CLEAN:ASYNC",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:clean() of
        {C,KL} when is_list(KL) ->
            BC = integer_to_binary(C),
            BK = pack_list_to_binary(KL,<<>>),
            BD = <<"{",BC/binary,":",BK/binary,"}">>,
            T:send(S,BD);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,<<"CLEAN",":",_P/binary>>},[S,T]) ->
    T:setopts(S,[{active,once}]),
    T:send(S,<<"{","ERROR",":","EXPIRED_REF","}">>),
    keep_state_and_data;
common(info,{tcp,S,<<"CLEAN",E/binary>>},[S,T]) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    T:setopts(S,[{active,once}]),
    case lfu:clean() of
        {C,KL} when is_list(KL) ->
            BC = integer_to_binary(C),
            BK = pack_list_to_binary(KL,<<>>),
            BD = <<"{",BC/binary,":",BK/binary,"}">>,
            T:send(S,BD);
        _ ->
            T:send(S,<<"{","ERROR",":","UNKNOW_ERROR","}">>)
    end,
    keep_state_and_data;
common(info,{tcp,S,_B},[S,T]) ->
    T:setopts(S,[{active,once}]),
    T:send(S,<<"{","ERROR",":","UNKNOW_COMMAND","}">>),
    keep_state_and_data.

delete(state_timeout,BR,[S,T,#{ref := BR, key := _K}]) ->
    {next_state,common,[S,T]};
delete(info,{tcp,_S,<<"CLEAN:ASYNC",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"CLEAN:SYNC",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,S,<<"CLEAN",":",P/binary>>},[S,T,#{ref := BR, key := K}]) ->
    T:setopts(S,[{active,once}]),
    case break_binary_string(byte_size(P),P) =:= BR of
        true ->
            case lfu:clean(list_to_ref(binary_to_list(BR)),K) of
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
delete(info,{tcp,_S,<<"POINT:",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"CHEAT:",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"COUNT:",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"STATE",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"STORE",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"FETCH",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,_S,<<"CLEAN",E/binary>>},_StateData) when E =:= <<>> orelse E =:= <<"\r\n">> orelse E =:= <<"\n">> orelse E =:= <<"\r">> ->
    {keep_state_and_data,[postpone]};
delete(info,{tcp,S,_B},[S,T,_MD]) ->
    T:setopts(S,[{active,once}]),
    T:send(S,<<"{","ERROR",":","UNKNOW_COMMAND","}">>),
    keep_state_and_data.


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
