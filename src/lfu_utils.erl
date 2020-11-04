-module('lfu_utils').
-author('VSolenkov').

-export([
    for/3,
    key_validation/1,
    ets_create/0,
    ets_re_create/0,
    ets_reset/1
]).
-include("include/lfu.hrl").



ets_reset(TL) ->
    lists:foreach(
        fun(T) ->
            ets:whereis(T) =/= undefined andalso
            ets:tab2file(
                T,
                element(2,file:get_cwd()) ++ "/" ++ application:get_env(lfu,ets_dir,"priv") ++ "/" ++ atom_to_list(T),
            [{sync,application:get_env(lfu,ets_sync_reset,true)}])
        end,
    TL).

ets_delete(T) ->
    catch ets:delete(T).

ets_create() ->
    NT = ets:new(?ETS_KEYS_FETCH_TABLE_NAME,?ETS_KEYS_FETCH_TABLE_OPTS),
    case get(?ETS_KEYS_FETCH_TABLE_NAME) of
        undefined -> skip;
        OT ->
            ets:info(OT) =/= undefined andalso ets_delete(OT)
    end,
    put(?ETS_KEYS_FETCH_TABLE_NAME,NT),
    NT.

ets_re_create() ->
    NT = ets:new(?ETS_KEYS_FETCH_TABLE_NAME,?ETS_KEYS_FETCH_TABLE_OPTS),
    case get(?ETS_KEYS_FETCH_TABLE_NAME) of
        undefined -> skip;
        OT ->
            case ets:info(OT) of
                undefined -> skip;
                _ ->
                    ets:foldl(
                        fun({K,V},[]) ->
                            ets:insert(NT,{K,V}),[]
                        end,
                    [],OT),
                    catch ets:delete(OT)
            end
    end,
    put(?ETS_KEYS_FETCH_TABLE_NAME,NT),
    NT.

key_validation(K) ->
    BK =
        if
            is_integer(K) ->
                integer_to_binary(K);
            is_atom(K) ->
                atom_to_binary(K);
            is_list(K) ->
                list_to_binary(K);
            is_tuple(K) ->
                term_to_binary(K);
            is_binary(K) ->
                K;
            true ->
                -1
        end,
    case BK of
        -1 ->
            -1;
        BK ->
            case size(BK) > ?MAX_KEY_SIZE() of
                true ->
                    -2;
                false ->
                    BK
            end
    end.

for(N,N,F) -> F(N);
for(I,N,_) when I > N -> null;
for(I,N,F) -> F(I),for(I+1,N,F).
