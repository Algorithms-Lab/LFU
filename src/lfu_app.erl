-module('lfu_app').
-author('VSolenkov').

-behavior(application).

-export([
    start/2,
    stop/1,
    prep_stop/1
]).


start(_StartType,_StartArgs) ->
    application:get_env(lfu,tcp,off) == on andalso application:ensure_all_started(ranch),
    lfu_sup:start_link().

stop(_ETS_TABLES) ->
    ok.

prep_stop(ETS_TABLES) ->
    lfu_utils:ets_reset(ETS_TABLES),
    ok.
