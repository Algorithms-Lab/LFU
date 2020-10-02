-module(lfu_quick_score_sup).
-author('VSolenkov').

-behavior(supervisor).

-export([
    start_link/0
]).
-export([
    start/1,
    stop/0
]).

-export([
    init/1
]).


start_link() ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init(_) ->
   {ok,{
       {simple_one_for_one,10,300},[{
           quick_score,{lfu_quick_score,start,[]},
           permanent,brutal_kill,worker,[lfu_quick_score]
       }]
   }}.

start([O,Q]) ->
    case get(O) of
        undefined ->
            put(O,1),
            supervisor:start_child(?MODULE,[[O,Q]]);
        C ->
            put(O,C+1),
            supervisor:start_child(?MODULE,[[O,Q,reboot]])
    end.

stop() ->
    exit(whereis(?MODULE),shutdown).
