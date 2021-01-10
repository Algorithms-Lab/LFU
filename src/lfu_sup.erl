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
    {ok,PID} = supervisor:start_link({local,?MODULE},?MODULE,[?ETS_KEYS_STORE_TABLE_NAME]),
    {ok,PID,[?ETS_KEYS_STORE_TABLE_NAME]}.


init(ETS_TABLES) ->
    init_tables(ETS_TABLES),
    {ok,{
        {rest_for_one,5,300},[
            {
                lfu,{lfu,start_link,[]},
                permanent,5000,worker,[lfu]
            }
       ] ++
       case application:get_env(lfu,tcp,off) of
           on ->
               [
                   {
                       ranch,{ranch_app,start,[0,0]},
                       permanent,5000,worker,[ranch_app]
                   }
               ] ++
               [ranch:child_spec(
                   {?MODULE,lfu_protocol},ranch_tcp,
                   #{
                       socket_opts => init_socket_opts(),
                       num_acceptors => init_acceptors(),
                       max_connections => init_connections()
                   },lfu_protocol,
               [])];
           _ ->
               []
       end
   }}.


stop() ->
    exit(whereis(?MODULE),shutdown).


init_tables(ETS_TABLES) ->
    case application:get_env(lfu,ets_recovery,false) of
        true ->
            lists:foreach(
                fun(T) ->
                    F = element(2,file:get_cwd()) ++ "/" ++ application:get_env(lfu,ets_dir,"priv") ++ "/" ++ atom_to_list(T),
                    case filelib:is_file(F) of
                        true ->
                            ets:file2tab(element(2,file:get_cwd()) ++ "/" ++ application:get_env(lfu,ets_dir,"priv") ++ "/" ++ atom_to_list(T));
                        false ->
                            ets:new(T,[named_table,set,public])
                    end
                end,
            ETS_TABLES);
        _ ->
            lists:foreach(
                fun(T) ->
                    ets:new(T,[named_table,set,public])
                end,
            ETS_TABLES)
    end.

init_socket_opts() ->
    init_socket().
init_socket() ->
    case application:get_env(lfu,mode,inet) of
        unix -> init_unix();
        inet -> init_port()
    end.
init_unix() ->
    [{ip,{local,application:get_env(lfu,unix)}},{port,0}].
init_port() ->
    [{ip,application:get_env(lfu,ip,{127,0,0,1})},{port,application:get_env(lfu,port,7777)}].
init_acceptors() ->
    application:get_env(lfu,num_acceptors,100).
init_connections() ->
    application:get_env(lfu,max_connections,1024).
