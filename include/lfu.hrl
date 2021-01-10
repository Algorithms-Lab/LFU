-define(SERIE_SIZE,10000000).
-define(MAX_COUNTER,1000000000000000).

-define(TIMEOUT_STATE_DELETE,90000).

-define(ETS_KEYS_STORE_TABLE_NAME,lfu_key_store).
-define(ETS_KEYS_FETCH_TABLE_NAME,lfu_key_fetch).
-define(ETS_KEYS_FETCH_TABLE_OPTS,[
    public,bag,{write_concurrency,true},
    {decentralized_counters,true}
]).

-define(MAX_KEY_SIZE,fun() -> application:get_env(lfu,max_key_size,72) end).

%%
%% following settings in progress develop
%%
-define(SPAWN_OPT_LRU,[
%   {max_heap_size,0},
%   {message_queue_data,off_heap},
    {fullsweep_after,65535}
]).

-ifdef(support).
    -define(SUPPORT,true).
    -define(AUXILIARY,any).
-else.
    -define(SUPPORT,false).
    -define(AUXILIARY,any).
-endif.
