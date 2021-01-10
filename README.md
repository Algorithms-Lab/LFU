# Least Frequently Used
Potent implementation of LFU algorithm based counters with support of counter by every keys up to once quadrillion hits.


Reference Guide
===============

## description
This is implementation of LFU algorithm based on counters with support of counter by every keys up to once quadrillion hits.

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

Both types of interface developed to IPACR (Interaction Protocol of Algorithms Cache Replacement) conformance.


##### note №2:
Note that the implementation of algorithm stores keys in binary, that is, for set of keys from the first example bellow key will be stored as in second example:

###### example №1

    <<"moscow">>
    ["moscow"]
    "moscow"
    moscow

###### example №2

    <<"moscow">>

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

###### Both types of interface developed to IPACR (Interaction Protocol of Algorithms Cache Replacement) conformance.

#### put key
###### internal:

    lfu:point(K).                                   %% ok

###### external:

    POINT:key                                       %% "OK"

#### get counter on key
###### internal:

    lfu:count(K).                                   %% counter

###### external:

    COUNT:key                                       %% "NUMBER"

#### get least counter, most counter and quantity of keys
###### internal:

    lfu:state().                                    %% [least counter,most counter,quantity of keys]

###### external:

    STATE                                           %% JSON: "{L:NUMBER,M:NUMBER,Q:NUMBER}"

#### store algorithm state to disk
###### Please pay attantion, 'store' call executes asynchronously!
###### internal:

    lfu:store().                                    %% ok

###### external:

    STORE                                           %% "OK"

#### get key with least counter
###### internal:

    lfu:fetch().                                    %% {counter,[<<"key">>]}

###### external:

    FETCH                                           %% JSON: "{counter:[key]}"

#### get and delete key with least counter
##### without confirm
###### internal:

    lfu:clean().                                    %% {counter,[<<"key">>]}
    or
    lfu:clean(async).                               %% {counter,[<<"key">>]}

###### external

    CLEAN                                           %% JSON: "{counter:[key]}"
    or
    CLEAN:ASYNC                                     %% JSON: "{counter:[key]}"

##### with confirm
###### Please, pay attention timeout exists to confirm, equal '90' seconds by default
###### internal:

    {K,R} = lfu:clean(sync).                        %% {{counter,[<<"key">>]},ref()}
    lfu:clean(R,K).
    
###### external:

    CLEAN:SYNC                                      %% JSON: "{{counter:[key]}:UNIQ_REF}"
    CLEAN:UNIQ_REF                                  %% "OK"

#### put list keys with conters
###### initialization of state, for example, transfer of state from other implementation 'lfu'
###### internal:

    lfu:cheat([{K1,C1},{K2,C2},{K3,C3}]).           %% ok

###### external:

    CHEAT:key1,counter1;key2,counter2;key3,counter3 %% "OK"
