-module(lfu_sup).
-author('VSolenkov').

-behavior('supervisor').

-export([
    start_link/0
]).
-export([
    stop/0
]).

-export([
    init/1
]).
-include("include/lfu.hrl").


start_link() ->
    {ok,PID} = supervisor:start_link({local,?MODULE},?MODULE,[?ETS_KEYS_TABLE_NAME,?ETS_PIDS_TABLE_NAME]),
    {ok,PID,[?ETS_KEYS_TABLE_NAME,?ETS_PIDS_TABLE_NAME]}.


init(ETS_TABLES) ->
    init_tables(ETS_TABLES),
    {ok,{
        {rest_for_one,5,300},[
            {
                lfu_score_sups_sup,{lfu_score_sups_sup,start_link,[]},
                permanent,5000,supervisor,[lfu_score_sups_sup]
            },
            {
                lfu,{lfu,start_link,[]},
                permanent,5000,worker,[lfu]
            }
       ]
   }}.


stop() ->
    exit(whereis(?MODULE),shutdown).


init_tables(ETS_TABLES) ->
    case application:get_env(ets_recovery,false) of
        true ->
            lists:foreach(
                fun(T) ->
                    F = element(2,file:get_cwd()) ++ "/" ++ atom_to_list(application:get_env(ets_dir,priv)) ++ "/" ++ atom_to_list(T),
                    case filelib:is_file(F) of
                        true ->
                            ets:file2tab(element(2,file:get_cwd()) ++ "/" ++ atom_to_list(application:get_env(ets_dir,priv)) ++ "/" ++ atom_to_list(T));
                        false ->
                            ets:new(T,[named_table,set,public])
                    end
                end,
            ETS_TABLES);
        _ ->
            lists:foreach(
                fun(T) ->
                    ets:new(T,[named_table,set,public])
                end,
            ETS_TABLES)
    end.
