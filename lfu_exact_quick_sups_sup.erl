-module(lfu_exact_quick_sups_sup).
-author('VSolenkov').

-behavior(supervisor).

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
       {one_for_one,1,300},[
           {
               exact_score_sup,{lfu_exact_score_sup,start_link,[]},
               permanent,5000,supervisor,[lfu_exact_score_sup]
           },
           {
               quick_score_sup,{lfu_quick_score_sup,start_link,[]},
               permanent,5000,supervisor,[lfu_quick_score_sup]
           }
       ]
   }}.

stop() ->
    exit(whereis(?MODULE),shutdown).
