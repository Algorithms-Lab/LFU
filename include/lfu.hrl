-define(MIN_LIMIT,100000).
-define(MAX_LIMIT,1000000000).
-define(MAX_ORDER,100000000000000).
-define(MIN_ORDER,100).
-define(MIN_OFFSET,10).			%% low limit for step to next rank
-define(MAX_OFFSET,30).			%% up limit for step to prev rank
-define(SCORE_OFFSET,0).		%% must be less than ?MIN_ORDER && for example if it`s necessary begin score from 100 then need setting to 99
-define(TIMEOUT_CLEAN,50000).
-define(TIMEOUT_COUNT,50000).
-define(PREFIX_KEY,"lfu___").
-define(POSTFIX_KEY,"__lfu").
-define(ETS_TABLE_NAME,lfu_).

-define(SPAWN_OPT_SIMPLE_SCORE,[
%   {max_heap_size,0},			%% Erlang/OTP > 18
%   {message_queue_data,off_heap},	%% Erlang/OTP > 18
    {fullsweep_after,65535}
]).
-define(SPAWN_OPT_QUICK_SCORE,[
%   {max_heap_size,0},			%% Erlang/OTP > 18
%   {message_queue_data,off_heap},	%% Erlang/OTP > 18
    {fullsweep_after,65535}
]).

-ifdef(support).
    -define(SUPPORT,true).
    -define(SECONDARY,kit).
-else.
    -define(SUPPORT,false).
    -define(SECONDARY,any).
-endif.