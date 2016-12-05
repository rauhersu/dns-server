%-*-Mode:erlang;coding:utf-8-*-

-module(dns_db_server).
-export([dns_db_init/0,
         dns_db_start/0,
         dns_db_provision/1,
         dns_db_add_queryA_response/1,
         show_stat_response/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(dns_queryA_response_TABLE, {hostname, responses}).

-define(DNS_DB_TIMEOUT,
        20000).

-define(IS_TUPLE_OK(Tuple),
        (ok == element(1,(Tuple)))).

% Hidden 'lock' file to hint the schema has already been created.
% Created in the same dir as the code (MODULE trick).
%
-define(DNS_DB_SCHEMA_FILE_LOCK,
        filename:join(filename:dirname(code:which(?MODULE)),
                      ".Mnesia.schema.lock")).       

dns_db_is_schema_locked() ->
    case file:read_file_info(?DNS_DB_SCHEMA_FILE_LOCK,[read]) of
        {ok,_IoDevice} -> true;
        {_error,_IoDevice} -> false
    end.

dns_db_set_schema_locked(Force) ->
    case dns_db_is_schema_locked() of
        true -> 
            (Force and (?IS_TUPLE_OK(file:open(?DNS_DB_SCHEMA_FILE_LOCK,read))));
        false ->
            (?IS_TUPLE_OK(file:open(?DNS_DB_SCHEMA_FILE_LOCK,write)))
    end.

dns_format_queryA_response_TABLE(Rows)->
    Fun = fun (Row) ->
                  io:format("~-15s : ~-50s ~n~-15s : ~50p ~n~n",
                            ["HOSTNAME",
                             Row#dns_queryA_response_TABLE.hostname,
                             "RESPONSES",
                             Row#dns_queryA_response_TABLE.responses])
          end,
    lists:map(Fun, Rows).

show_stat_response() ->
    _Rows = do(qlc:q([X || X <- mnesia:table(dns_queryA_response_TABLE)])),
    dns_format_queryA_response_TABLE(_Rows).

dns_db_add_queryA_response(Hostname) ->
    Transaction =
        fun() ->
                [Host] = mnesia:read({dns_queryA_response_TABLE,Hostname}),
                Host_responses = Host#dns_queryA_response_TABLE.responses,
                Host_responses_now = Host#dns_queryA_response_TABLE{responses =
                                                              Host_responses+1},
                mnesia:write(Host_responses_now)
        end,
    mnesia:transaction(Transaction).


do(Q) ->
    Transaction = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(Transaction),
    Val.

dns_db_provision(Hosts_by_name) ->
    Hostnames = maps:keys(Hosts_by_name),
    Populate = fun(Hostname) ->
                       {dns_queryA_response_TABLE,Hostname,_Response=0} 
               end,
    Rows = lists:map(Populate, Hostnames),

    Transaction = fun() -> 
                          lists:foreach(fun mnesia:write/1, Rows) 
                  end,
    mnesia:transaction(Transaction).

dns_db_start() ->
    mnesia:start(),
    mnesia:wait_for_tables([dns_queryA_response_TABLE], ?DNS_DB_TIMEOUT).

dns_db_init() ->                                            
    case dns_db_set_schema_locked(false) of
        false ->
            mnesia:create_schema([node()]),
            mnesia:start(),
            mnesia:create_table(
              dns_queryA_response_TABLE,
              [{attributes, record_info(fields,
                                        dns_queryA_response_TABLE)}]),
            mnesia:stop(),
            {ok,"lock not created"};
        true -> 
            {ok,"lock created"}
    end.

