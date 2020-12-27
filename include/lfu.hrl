-define(MIN_LIMIT,100000).
-define(MAX_LIMIT,1000000000).

-define(MAX_ORDER,100000000000000).	%% 1000000000 .. 100000000000000
-define(MIN_ORDER,100).

-define(MIN_OFFSET,10).			%% low limit for step to next rank
-define(MAX_OFFSET,30).			%% up limit for step to prev rank

-define(SCORE_OFFSET,0).		%% must be less than ?MIN_ORDER && for example if it`s necessary begin score from 100 then need setting to 99

-define(TIMEOUT_STATE_OFFSET,90000).
-define(TIMEOUT_STATE_SELECT,90000).
-define(TIMEOUT_STATE_DELETE,90000).

-define(PREFIX_KEY,"lfu___").
-define(POSTFIX_KEY,"__lfu").

-define(ETS_PIDS_STORE_TABLE_NAME,lfu_pid).
-define(ETS_KEYS_STORE_TABLE_NAME,lfu_key).
-define(ETS_KEYS_FETCH_TABLE_NAME,lfu_key_fetch).
-define(ETS_KEYS_FETCH_TABLE_OPTS,[
    public,bag,{write_concurrency,true},
    {decentralized_counters,true}
]).

-define(MAX_KEY_SIZE,fun() -> application:get_env(lfu,max_key_size,72) end).

%%
%% following settings in progress develop
%%
-define(SPAWN_OPT_LFU,[
%   {max_heap_size,0},
%   {message_queue_data,off_heap},
    {fullsweep_after,65535}
]).
-define(SPAWN_OPT_EXACT_SCORE,[
%   {max_heap_size,0},
%   {message_queue_data,on_heap},
%   {min_bin_vheap_size,46422},
%   {min_heap_size,233},
    {fullsweep_after,65535}
]).
-define(SPAWN_OPT_QUICK_SCORE,[
%   {max_heap_size,0},
%   {message_queue_data,on_heap},
%   {min_bin_vheap_size,46422},
%   {min_heap_size,233},
    {fullsweep_after,65535}
]).

-ifdef(support).
    -define(SUPPORT,true).
    -define(AUXILIARY,any).
-else.
    -define(SUPPORT,false).
    -define(AUXILIARY,any).
-endif.
