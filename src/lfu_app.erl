-module('lfu_app').
-author('VSolenkov').

-behavior(application).

-export([
    start/2,
    stop/1,
    prep_stop/1
]).


start(_StartType,_StartArgs) ->
    lfu_sup:start_link().

stop(_ETS_TABLES) ->
    ok.

prep_stop(ETS_TABLES) ->
    reset_tables(ETS_TABLES),
    ok.


reset_tables(ETS_TABLES) ->
    [
        ets:tab2file(
            ETS_TABLE,
            element(2,file:get_cwd()) ++ "/" ++ atom_to_list(application:get_env(ets_dir,priv)) ++ "/" ++ atom_to_list(ETS_TABLE),
            [{sync,application:get_env(ets_sync_reset,true)}]
        )
        || ETS_TABLE <- ETS_TABLES, ets:info(ETS_TABLE) =/= undefined
    ]. 