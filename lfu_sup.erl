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
    supervisor:start_link({local,?MODULE},?MODULE,[]).


init(_) ->
   ets:new(?ETS_KEYS_TABLE_NAME,[named_table,set,public]),
   ets:new(?ETS_PIDS_TABLE_NAME,[named_table,bag,public]),
   {ok,{
       {rest_for_one,1,300},[
           {
               lfu,{lfu,start_link,[]},
               permanent,5000,worker,[lfu]
           },
           {
               lfu_score_sups_sup,{lfu_score_sups_sup,start_link,[]},
               permanent,5000,supervisor,[lfu_score_sups_sup]
           }
       ]
   },[?ETS_KEYS_TABLE_NAME,?ETS_PIDS_TABLE_NAME]}.


stop() ->
    exit(whereis(?MODULE),shutdown).
