%-*-Mode:erlang;coding:utf-8-*-

-module(dns_db_server).
-export([init/0,
         start/0,
         reset/0,
         provision/1,
         insert_queryA_response/1,
         show_stat_queryA_response/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(dns_queryA_response_TABLE, {hostname, responses}).

-define(DNS_DB_TIMEOUT,
        20000).

format_queryA_response_TABLE(Rows)->
    Fun = fun (Row) ->
                  io:format("~-15s : ~-50s ~n~-15s : ~50p ~n~n",
                            ["HOSTNAME",
                             Row#dns_queryA_response_TABLE.hostname,
                             "RESPONSES",
                             Row#dns_queryA_response_TABLE.responses])
          end,
    lists:map(Fun, Rows).

show_stat_queryA_response() ->
    _Rows = do(qlc:q([X || X <- mnesia:table(dns_queryA_response_TABLE)])),
    format_queryA_response_TABLE(_Rows).

insert_queryA_response(Hostname) ->
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
    Transaction = fun() -> 
                          qlc:e(Q) 
                  end,
    {atomic, Val} = mnesia:transaction(Transaction),
    Val.

provision(Hosts_by_name) ->
    Hostnames = maps:keys(Hosts_by_name),
    Populate = fun(Hostname) ->
                       {dns_queryA_response_TABLE,Hostname,_Response=0} 
               end,
    Rows = lists:map(Populate, Hostnames),

    Transaction = fun() -> 
                          lists:foreach(fun mnesia:write/1, Rows) 
                  end,
    mnesia:transaction(Transaction).

reset() ->
    mnesia:clear_table(dns_queryA_response_TABLE).

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([dns_queryA_response_TABLE], ?DNS_DB_TIMEOUT).

init() ->                                            
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(
      dns_queryA_response_TABLE,
      [{attributes, record_info(fields,
                                dns_queryA_response_TABLE)}]),
    mnesia:stop().
