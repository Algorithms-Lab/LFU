-module('lfu_utils').
-author('VSolenkov').

-export([
    for/3,
    key_validation/1
]).
-include("include/lfu.hrl").



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
