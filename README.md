# Least Frequently Used
Potent implementation of LFU algorithm based on processes-counters with support of counter by every keys up to once quadrillion hits.


Reference Guide
===============

## description
This is implementation of LFU algorithm based on processes-counters with support of counter by every keys up to once quadrillion hits.

### Tasks:

* Algorithm accumulates any actions by keys in outside system.
* Algorithm executes fetching keys for follow deletion from outside system.

### More:

* https://en.wikipedia.org/wiki/Cache_replacement_policies#Least-frequently_used_(LFU)


## launch options

    [{lfu,[
        {ets_dir,priv},		    %% !!! must be atom type !!!!!
        {ets_sync_reset,true},	%% !!! must be atom type !!!!!
        {ets_recovery,true}	    %% !!! must be atom type !!!!!
    ]}].

#### ets_dir
directory storage ets-tables

#### ets_sync_reset
it ensures that the content of the state is written to the disk

#### ets_recovery
it ensures that lfu launches with prev state


## client interface

#### launch

    application:start(lfu).

#### put key

    lfu:point(K).

#### get counter on key

    lfu:count(K).

#### get offset counter and counter all keys

    lfu:state().

#### execute scoring of bias counter

    lfu:score().

#### execute scoring of offset counter and get keys by it

    T = ets:new(t,[named_table,bag,public]).
    lfu:fetch(T).
    ets:tab2list(T).

#### execute scoring of offset counter and get keys by it for follow delete

    T = ets:new(t,[named_table,bag,public]).
    R = lfu:clean(T).  %% R = ref()
    lfu:clean(R,T).

#### put list keys with conters, for debugging

    lfu:cheat([{K1,C1},{K2,C2},{K3,C3}]).


## configuration

    -define(MIN_LIMIT,100000).
    -define(MAX_LIMIT,1000000000).

    -define(MAX_ORDER,100000000000000).     %% 1000000000 .. 100000000000000
    -define(MIN_ORDER,100).                 %%

    -define(MIN_OFFSET,10).                 %% low limit for step to next rank
    -define(MAX_OFFSET,30).                 %% up limit for step to prev rank

    -define(SCORE_OFFSET,0).                %% !!!!! must be less ?MIN_ORDER !!!!! && for example if it`s necessary begin score from 100 then need setting to 99

    -define(TIMEOUT_STATE_OFFSET,90000).
    -define(TIMEOUT_STATE_SELECT,90000).
    -define(TIMEOUT_STATE_DELETE,90000).

    -define(PREFIX_KEY,"lfu___").
    -define(POSTFIX_KEY,"__lfu").

    -define(ETS_PIDS_TABLE_NAME,lfu_pid).
    -define(ETS_KEYS_TABLE_NAME,lfu_key).

#### MIN_LIMIT

Range of values for the processes of low-order counters.

Quantity the processes of low-order counters:

    'MAX_LIMIT' / 'MIN_LIMIT'

#### MAX_LIMIT

Range of values for the processes of high-order counters.

Quantity the processes of high-order counters:

    'MAX_ORDER' / 'MAX_LIMIT'

#### MIN_ORDER

Low (initial) value offset counter.

#### MAX_ORDER

Up (end) value offset counter.
Keys counters reached this value will be no longer incremented.

Allow values depending on system performance:

    1000000000
    10000000000
    100000000000
    1000000000000
    10000000000000
    100000000000000
    1000000000000000

#### MIN_OFFSET

Defines minimum permissible percentage of the number of keys, with a counter value equal to or less than the current measured value of the offset counter, of the total number of keys.
When the value is reached, the offset counter is incremented by one digit (provided that the following calculate value does not exceed 'MAX_OFFSET' value) and so on until an acceptable percentage is reached.

The smaller it is, the fewer keys will be available for follow deletion.

#### MAX_OFFSET

Defines maximum permissible percentage of the number of keys, with a counter value equal to or less than the current measured value of the offset counter, of the total number of keys.
When the value is reached, the offset counter is decreases by one digit (provided that the following calculate value will more 'MIN_OFFSET' value) and so on until an acceptable percentage is reached.


The larger it is, the more keys will be available for follow deletion.

#### SCORE_OFFSET

The value of the key counter when a key begins to take into account by the algorithm.

Must be less:
    
    '?MIN_ORDER'

example:

    if it`s necessary begin score from 100 then need set to 99

#### TIMEOUT_STATE_OFFSET

The timeout in timing that 'lfu' main process will waiting response on 'score' command from counter processes.

This value can be incresed provided overload system.

#### TIMEOUT_STATE_SELECT

The timeout in timing that 'lfu' main process will waiting response on 'fetch' command from counter processes.

This value can be incresed provided overload system.

#### TIMEOUT_STATE_DELETE

The timeout in timing that 'lfu' main process will waiting confirming response on 'clean' command from outside client.

This value can be control depending on how long external client will handle the keys list.

#### PREFIX_KEY

The prefix key for service name of key for to store process counter pids in 'lfu_key' ets.

#### POSTFIX_KEY

The postfix key for service name of key for to store process counter pids in 'lfu_key' ets.

#### ETS_PIDS_TABLE_NAME

The ets for to store process counter pids.

#### ETS_KEYS_TABLE_NAME

The ets for to store counters by keys.
