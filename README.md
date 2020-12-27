# Least Frequently Used
Potent implementation of LFU algorithm based on processes-counters with support of counter by every keys up to once quadrillion hits.


Reference Guide
===============

## description
This is implementation of LFU algorithm based on processes-counters with support of counter by every keys up to once quadrillion hits.

#### tasks:

* Algorithm accumulates any actions by keys in outside system.
* Algorithm executes fetching keys for follow deletion from outside system.

#### more:

* https://en.wikipedia.org/wiki/Cache_replacement_policies#Least-frequently_used_(LFU)

#### notes:
##### note №1:
Note that the implementation of algorithm support two interaction modes:

###### internal

    like OTP application into your Erlang node

###### external

    like daemon into your Unix-like OS

But actually nothing forbidens to interact in both modes at same time.


##### note №2:
Note that the implementation of algorithm stores keys in binary, that is, for set of keys from the first example bellow key will be stored as in second example:

###### example №1

    <<"moscow">>
    ["moscow"]
    "moscow"
    moscow

###### example №2

    <<"moscow">>

#### notice:
Note this implementation lfu algorithm use named processes-counters, that is atoms.
System quantity atoms is permissible 1048576 by default.
Maximum possible number named processes dynamic create, counts as follows:

###### processes of high-order counters

    (MAX_ORDER-1) div MAX_LIMIT

###### by default configuration:

    (100000000000000-1) div 1000000000

###### processes of low-order counters

    MAX_LIMIT div MIN_LIMIT

###### by default configuration:

    1000000000 div 100000

###### full expression:

    ((100000000000000-1) div 1000000000) + (1000000000 div 100000) = 109 998

But it is value may be more if MAX_ORDER raise to quadrillion:

    ((1000000000000000-1) div 1000000000) + (1000000000 div 100000) = 1 009 998

In this case you necessary is launch the Erlang-node with key '+t'.


## launch options

    [{lfu,[
        {ets_dir,"priv"},               %% !!! must be string type !!!!!
        {ets_sync_reset,true},          %% !!! must be atom type !!!!!
        {ets_recovery,true},            %% !!! must be atom type !!!!!
        {tcp,on},                       %% !!! must be atom type !!!!!
        {mode,inet},                    %% !!! must be atom type !!!!!
        {port,7777},                    %% !!! must be atom type !!!!!
        {ip,{127,0,0,1}},               %% !!! must be tuple type !!!!!
        {unix,"/var/run/lfu/unix"},     %% !!! must by string type !!!!!
        {num_acceptors,100},            %% !!! must by integer type !!!!!
        {max_connections,1024},         %% !!! must by integer type !!!!!
        {max_key_size,72}               %% !!! must be integer type !!!!!
    ]}].

#### ets_dir
path to directory storage ets-tables, relative to the root directory of application

#### ets_sync_reset
it ensures that the content of the state is written to the disk

#### ets_recovery
it ensures that lfu launches with prev state

#### tcp
on or off support of ranch interaction, by default is off

#### mode
mode work: inet|unix
by default is inet

#### port
port, by default 7777

#### ip
ip, by default 127.0.0.1

#### unix
unix_socket, by default '/var/run/lfu/unix'

#### num_acceptors
excerpt from 'ranch' documentation:

    By default Ranch will use 10 acceptor processes. Their role is to accept connections and spawn a connection process for every new connection.
    This number can be tweaked to improve performance. A good number is typically between 10 or 100 acceptors. You must measure to find the best value for your application.

#### max_connections
excerpt from 'ranch' documentation:

    The max_connections transport option allows you to limit the number of concurrent connections per connection supervisor (see below).
    It defaults to 1024. Its purpose is to prevent your system from being overloaded and ensuring all the connections are handled optimally.

    You can disable this limit by setting its value to the atom infinity.

    The maximum number of connections is a soft limit. In practice, it can reach max_connections + the number of acceptors.

    When the maximum number of connections is reached, Ranch will stop accepting connections.
    This will not result in further connections being rejected, as the kernel option allows queueing incoming connections.
    The size of this queue is determined by the backlog option and defaults to 1024. Ranch does not know about the number of connections that are in the backlog.

#### max_key_size
max key size

## quick start
#### like OTP application into your Erlang node

    erl -config lfu.config
    application:start(lfu)

#### like daemon into your Unix-like OS

    mkdir release
    tar -xf priv/lfu.tar.gz -C release/
    
    cp priv/init release/.
    cp priv/stop release/.
    
    cd release
    
    mkdir bin
    mkdir log
    mkdir pipe
    
    cp erts-11.1/bin/start.src bin/start
    cp erts-11.1/bin/start_erl.src bin/start_erl
    cp erts-11.1/bin/run_erl bin/.
    cp erts-11.1/bin/to_erl bin/.
    cp erts-11.1/bin/erl bin/.
    cp erts-11.1/bin/heart bin/.
    cp erts-11.1/bin/escript bin/.
    
    perl -i -pe "s#%FINAL_ROOTDIR%#$PWD#" bin/start
    
    sed -i 's/\/tmp/$ROOTDIR\/pipe/' bin/start
    sed -i 's/\(.*run_erl.*\)".*$/\1 -sname lfu -init_debug +t 10485760\"/' bin/start
    
    echo "11.1 1" > releases/start_erl.data
    
    ./init startd
    ./init stop
    
    
## client interface
###### This section describes two types interfaces:

    internal - erlang interface for inner interaction in Erlang node
    external - outside interface for interaction from the world outside

#### put key
###### internal:

    lfu:point(K).

###### external:

    POINT:key               %% "OK"

#### get counter on key
###### internal:

    lfu:count(K).

###### external:

    COUNT:key               %% "NUMBER"

#### get offset counter and counter all keys
###### internal:

    lfu:state().

###### external:

    STATE                   %% JSON: "{O:NUMBER,Q:NUMBER}"

#### store algorithm state to disk
###### Please pay attantion, 'store' call executes asynchronously!
###### internal:

    lfu:store().

###### external:

    STORE                   %% "OK"

#### execute scoring of offset counter
###### internal:

    lfu:score().

###### external:

    SCORE                   %% "READY"

#### execute scoring of offset counter and get keys by it into internal table
###### internal:
###### Please pay attantion, that exist of internal table expires after following request to fetching 'fetch/0' or to clean 'clean/0'!

    T = lfu:fetch().        %% tid()
    ets:tab2list(T).

###### external:

    FETCH                   %% JSON: "[{number1:[key1,key2,key3]},{number2:[key1,key2,key3]},{number3:[key1,key2,key3]},...]"

#### execute scoring of offset counter and get keys by it into external table
###### Please pay attantion, that it`s preferably using interface with internal table 'fetch/0', because it ensures a data consistency with your system!
###### internal:

    T = ets:new(stub,[      %% tid()
        bag,public,{write_concurrency,true},
        {decentralized_counters,true}
    ]).
    lfu:fetch(T).
    ets:tab2list(T).
    
#### execute scoring of offset counter and get keys by it into internal table for follow delete
###### internal:
###### Please pay attantion, that exist of internal table expires after following request to fetching 'fetch/0' or to clean 'clean/0'!

    {T,R} = lfu:clean().    %% {tid(),ref()}
    lfu:clean(R,T).
    
###### external:

    CLEAN                   %% JSON: "{[{number1:[key1,key2,key3]},{number2:[key1,key2,key3]},{number3:[key1,key2,key3]},...]:UNIQ_REF}"
    CLEAN:UNIQ_REF          %% OK

#### execute scoring of offset counter and get keys by it into external table for follow delete
######  Please pay attantion, that it`s preferably using interface with internal table 'clean/0', because it ensures a data consistency with your system!
###### internal:

    T = ets:new(stub,[      %% tid()
        bag,public,{write_concurrency,true},
        {decentralized_counters,true}
    ]).
    R = lfu:clean(T).       %% ref()
    lfu:clean(R,T).

#### put list keys with conters
###### initialization of state, for example, transfer of state from other implementation 'lfu'
###### internal:

    lfu:cheat([{K1,C1},{K2,C2},{K3,C3}]).

###### external:

    CHEAT:key1,counter1;key2,counter2;key3,counter3 %% OK


## configuration (under the hood)
#### Before corrects settings make sure you understand the implementation!

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

    -define(ETS_PIDS_STORE_TABLE_NAME,lfu_pid).
    -define(ETS_KEYS_STORE_TABLE_NAME,lfu_key).
    
    -define(ETS_KEYS_FETCH_TABLE_NAME,lfu_key_fetch).
    -define(ETS_KEYS_FETCH_TABLE_OPTS,[
        public,bag,{write_concurrency,true},
    {decentralized_counters,true}]).

#### MIN_LIMIT

Range of values for the processes of low-order counters.

###### Quantity the processes of low-order counters:

    'MAX_LIMIT' div 'MIN_LIMIT'

#### MAX_LIMIT

Range of values for the processes of high-order counters.

###### Quantity the processes of high-order counters:

    ('MAX_ORDER'-1) div 'MAX_LIMIT'

#### MIN_ORDER

Low (initial) value offset counter.

#### MAX_ORDER

Up (end) value for key counters and offset counter.
Keys counters reached this value will be no longer incremented.

###### Allow values depending on system performance:

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

###### Must be less:
    
    'MIN_ORDER'

###### example:

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

#### ETS_PIDS_STORE_TABLE_NAME

The ets for to store process counter pids.

#### ETS_KEYS_STORE_TABLE_NAME

The ets for to store counters by keys.

#### ETS_KEYS_FETCH_TABLE_NAME

The ets for fetching keys into internal table by commands: 'fetch/0', 'clean/0'.

#### ETS_KEYS_FETCH_TABLE_OPTS

The list of options for creating 'ETS_KEYS_FETCH_TABLE_OPTS' ets.
