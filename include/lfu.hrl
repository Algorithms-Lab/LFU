-define(MIN_LIMIT,100000).
-define(MAX_LIMIT,1000000000).

-define(MAX_ORDER,100000000000000).	%% 1000000000000 .. 100000000000000
-define(MIN_ORDER,100).

-define(MIN_OFFSET,10).			%% low limit for step to next rank
-define(MAX_OFFSET,30).			%% up limit for step to prev rank

-define(SCORE_OFFSET,0).		%% must be less than ?MIN_ORDER && for example if it`s necessary begin score from 100 then need setting to 99

-define(TIMEOUT_STATE_OFFSET,90000).
-define(TIMEOUT_STATE_SELECT,90000).
-define(TIMEOUT_STATE_DELETE,90000).

-define(TIMEOUT_CALL,1000000).

-define(PREFIX_KEY,"lfu___").
-define(POSTFIX_KEY,"__lfu").

-define(ETS_PIDS_TABLE_NAME,lfu_pid).
-define(ETS_KEYS_TABLE_NAME,lfu_key).

-define(SPAWN_OPT_SIMPLE_SCORE,[
%   {max_heap_size,0},			%% Erlang/OTP > 18
%   {message_queue_data,off_heap},	%% Erlang/OTP > 18
%   {min_bin_vheap_size,46422},
%   {min_heap_size,233},
    {fullsweep_after,65535}
]).
-define(SPAWN_OPT_QUICK_SCORE,[
%   {max_heap_size,0},			%% Erlang/OTP > 18
%   {message_queue_data,off_heap},	%% Erlang/OTP > 18
%   {min_bin_vheap_size,46422},
%   {min_heap_size,233},
    {fullsweep_after,65535}
]).

-ifdef(support).
    -define(SUPPORT,true).
    -define(SECONDARY,kit).
-else.
    -define(SUPPORT,false).
    -define(SECONDARY,any).
-endif.
