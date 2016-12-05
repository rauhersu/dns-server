%-*-Mode:erlang;coding:utf-8-*-xx

-module(dns_server).
-export([start/2,
         start/1]).

-define(BYTE,
        8).

-define(NULL_TERMINATION_LEN,
        ?BYTE).

-define(DNS_SERVER_NAME,
        dns_server).

% Request (DNS Query A)
%
-define(DNS_ID_LEN(),
        (2*?BYTE)).
-define(DNS_FLAGS_LEN(),
        (2*?BYTE)).
-define(DNS_NUM_QUESTIONS_LEN(),
        (2*?BYTE)).
-define(DNS_QUESTIONS_LEN(),
        (1*?BYTE)).
-define(DNS_QTYPE_LEN(),
        (2*?BYTE)).
-define(DNS_QCLASS_LEN(),
        (2*?BYTE)).
-define(DNS_NUM_ANSWERS_LEN(),
        (2*?BYTE)).
-define(DNS_NUM_AUTH_LEN(),
        (2*?BYTE)).
-define(DNS_NUM_ADD_LEN(),
        (2*?BYTE)).
-define(DNS_HEADER_LEN(),
        (?DNS_ID_LEN()+
         ?DNS_FLAGS_LEN()+
         ?DNS_NUM_QUESTIONS_LEN()+
         ?DNS_NUM_ANSWERS_LEN()+
         ?DNS_NUM_AUTH_LEN()+
         ?DNS_NUM_ADD_LEN())).
-define(DNS_QUESTION_ONE, %  only one question is allowed
        1).

% Response (DNS Query A)
%
-define(DNS_ANSWER_POINTER,
        16#C0).
-define(DNS_ANSWER_OFFSET(),
        (?DNS_HEADER_LEN() div ?BYTE)).
-define(DNS_ANSWER_TYPE_LEN(),
        (2*?BYTE)).
-define(DNS_ANSWER_CLASS_LEN(),
        (2*?BYTE)).
-define(DNS_ANSWER_TTL_LEN(),
        (4*?BYTE)).
-define(DNS_ANSWER_DATA_LENGTH_LEN(),
        (2*?BYTE)).
-define(DNS_ANSWER_ADDR_LEN(),
        (4*?BYTE)).
-define(DNS_ANSWER_ZERO,
        0).

% Response hardcoded values 
%
-define(DNS_FLAGS,                  % qr rd ra
        16#8180).
-define(DNS_NUM_AUTH,               % No authority 
        16#0000).
-define(DNS_NUM_ADD,                % No additional
        16#0000).
-define(DNS_ANSWER_TYPE,            % A (host address)
        16#0001).
-define(DNS_ANSWER_CLASS,           % IN
        16#0001).
-define(DNS_ANSWER_TTL,             % 10 seconds
        16#0000000A).
-define(DNS_ANSWER_DATA_LENGTH,     % 4 bytes (IPv4 address)
        16#0004).


% Executes a composition of functions
%
chain_exec([], Arg) ->
    Arg;
chain_exec([Fun | Funs], Arg) ->
    chain_exec(Funs, Fun(Arg)).

dns_filter_cr_lf(Data) ->
    re:replace(Data, "[\\r\\n]", "", [{return,list}]).

% TODO: TCO
%
dns_convert_file_to_list(S) ->
    case io:get_line(S, '') of
        eof -> [];
        Line -> [dns_filter_cr_lf(Line) | dns_convert_file_to_list(S)]
    end.
dns_parse_address(Ip) ->
    element(2,inet:parse_address(Ip)).
    
% TODO: TCO
%   
dns_convert_list_to_map([]) ->
    #{};
dns_convert_list_to_map([Host|Hosts]) ->
    [Name|Ips] = string:tokens(Host," "),
    
    % Composes Ips in a binary (<<...>>) form.
    % Is there a shorter way rather than 3 funs?!
    Chain = [fun dns_parse_address/1,
             fun tuple_to_list/1,
             fun list_to_binary/1],
    Fun = fun (X) -> 
                  chain_exec(Chain,X) 
          end,
    
    Ips_to_binaries = lists:map(Fun,Ips),
    maps:put(Name,Ips_to_binaries,dns_convert_list_to_map(Hosts)).
    
dns_get_hosts_by_name(File) ->
    {ok, S} = file:open(File,read),
    
    % Composes a map of Host -> IPs
    %
    Chain = [fun dns_convert_file_to_list/1,
             fun dns_convert_list_to_map/1],
    chain_exec(Chain,S).
    
dns_encode_queryA_header_and_question(Dns_id,
                                      Dns_num_answers,
                                      Dns_queryA,
                                      Dns_queryA_len) ->
    
    <<Dns_id:?DNS_ID_LEN(),
      ?DNS_FLAGS:?DNS_FLAGS_LEN(),
      ?DNS_QUESTION_ONE:?DNS_NUM_QUESTIONS_LEN(),
      Dns_num_answers:?DNS_NUM_ANSWERS_LEN(),
      ?DNS_NUM_AUTH:?DNS_NUM_AUTH_LEN(),
      ?DNS_NUM_ADD:?DNS_NUM_ADD_LEN(),
      Dns_queryA:Dns_queryA_len>>.
    
dns_encode_queryA_answers(Host_ips) ->
    
    Dns_encode_one_queryA_answer = 
        fun (Host_ip) ->
                  % Pointer to the hostname (it  is 
                  % already present  in the  queryA 
                  % segment).
                  % See 4.1.4 "Message compression"
                  % on RFC1035
                <<?DNS_ANSWER_POINTER:?BYTE,
                  % Offset for that pointer: 
                  % hostname = message start + offset
                  ?DNS_ANSWER_OFFSET():?BYTE,
                  ?DNS_ANSWER_TYPE:?DNS_ANSWER_TYPE_LEN(),
                  ?DNS_ANSWER_CLASS:?DNS_ANSWER_CLASS_LEN(),
                  ?DNS_ANSWER_TTL:?DNS_ANSWER_TTL_LEN(),
                  ?DNS_ANSWER_DATA_LENGTH:?DNS_ANSWER_DATA_LENGTH_LEN(),
                  Host_ip/binary>> 
        end,
    
    All_QueryA_answers =
        erlang:iolist_to_binary(lists:map(Dns_encode_one_queryA_answer,
                                          Host_ips)),
    
    _All_QueryA_answers_plus_add = erlang:iolist_to_binary(
                                     [All_QueryA_answers,
                                      <<?DNS_NUM_ADD:?DNS_NUM_ADD_LEN()>>]).
% Host found
%   
dns_encode_queryA_response({ok,Host_ips},
                            Hostname,Dns_id,Dns_queryA,Dns_queryA_len) ->
    
    io:format("**DNS** hostname:~p found!~n",[Hostname]),
    
    dns_db_server:dns_db_add_queryA_response(Hostname),
    
    Num_answers = erlang:length(Host_ips),
    
    QueryA_header_and_question =
        dns_encode_queryA_header_and_question(Dns_id,
                                              Num_answers,
                                              Dns_queryA,
                                              Dns_queryA_len),
    
    QueryA_anwers = dns_encode_queryA_answers(Host_ips),
    _QueryA_response =
        erlang:iolist_to_binary([QueryA_header_and_question,QueryA_anwers]);

% Host not found
%
dns_encode_queryA_response(error,
                           Hostname,Dns_id,Dns_queryA,Dns_queryA_len) ->

    io:format("**DNS** hostname:~p NOT found!~n",[Hostname]),

    _QueryA_response = dns_encode_queryA_header_and_question(Dns_id,
                                                             ?DNS_ANSWER_ZERO,
                                                             Dns_queryA,
                                                             Dns_queryA_len).

dns_decode_queryA_request(Socket,Hosts_by_name,Host,Port,Src_packet) ->

    % The request
    %
    <<Dns_id:?DNS_ID_LEN(),
      _Dns_flags_questions_answers_numauth_numadd:(?DNS_HEADER_LEN()-
                                                       ?DNS_ID_LEN()),
      Dns_rest_of_msg/binary>> = Src_packet,

    % Obtain the host name
    %
    <<Dns_hostname_len_bytes:?DNS_QUESTIONS_LEN(),
      _/binary>> = Dns_rest_of_msg,

    Dns_hostname_len = Dns_hostname_len_bytes*?BYTE,

    <<_len:?DNS_QUESTIONS_LEN(),
      Dns_hostname:Dns_hostname_len,_/binary>> = Dns_rest_of_msg,

    Hostname = binary_to_list(<<Dns_hostname:Dns_hostname_len>>),
    io:format("**DNS** queried: ~p~n",[Hostname]),

    Host_ips = maps:find(Hostname,Hosts_by_name),

    % Obtain the query A (host name + QTYPE + QCLASS))
    %
    Dns_queryA_len = ?DNS_QUESTIONS_LEN()+
                        Dns_hostname_len+
                    ?NULL_TERMINATION_LEN+
                         ?DNS_QTYPE_LEN()+
                        ?DNS_QCLASS_LEN(),

    <<Dns_queryA:Dns_queryA_len,
      _/binary>> = Dns_rest_of_msg,


    % The response
    %
    Dst_packet = dns_encode_queryA_response(Host_ips,
                                            Hostname,
                                            Dns_id,
                                            Dns_queryA,
                                            Dns_queryA_len),

    io:format("**DNS** Dst_packet:~p~n",[Dst_packet]),

    gen_udp:send(Socket, Host, Port, Dst_packet).

dns_receive(Socket,Hosts_by_name) ->
    receive
        {udp, Socket, Host, Port, Src_packet} = Src_Data ->
            io:format("**DNS** server received:~p~n",[Src_Data]),

            Fun = fun() -> 
                          dns_decode_queryA_request(Socket,
                                                    Hosts_by_name,
                                                    Host,
                                                    Port,
                                                    Src_packet) 
                  end,

            % A process (acceptor) will be dinamically spawned per query to 
            % attend it.
            %
            spawn(Fun), 
            dns_receive(Socket,Hosts_by_name)
    end.

dns_server(Port,Hosts_by_name) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),
    io:format("**DNS** server opened socket:~p~n",[Socket]),
    dns_receive(Socket,Hosts_by_name).


% To be started standalone
%
% $ erl [-noshell] -pa PATH-TO-BEAM -run dns_server start FILE PORT
% 
% Ie: 
% 
% $ erl [-noshell] -pa _build/default/lib/dns_server/ebin/ -run dns_server start "./src/dns.hosts.txt" 3535
%                           
start([File|[PortAsString]]) ->
    {Port,_} = string:to_integer(PortAsString),
    start(File,Port).   

% To be started embbeded in a shell
%
% $ erl 
% 1> c(dns_server).
% 2> dns_server:start(FILE,PORT).
%
% Ie:
%
% $ r3 compile
% 1> code:add_path("./_build/default/lib/dns_server/ebin").
% 2> dns_server:start("/home/raul/my-repos/dns_server/src/dns.hosts.txt",3535).
%
start(File,Port) ->

    Hosts_by_name = dns_get_hosts_by_name(File),

    io:format("**DNS** server file::~p:: port::~p::~n" ,[File,Port]),
    io:format("**DNS** server configured with ::~p::~n",[Hosts_by_name]),

    dns_db_server:dns_db_init(),
    dns_db_server:dns_db_start(),
    dns_db_server:dns_db_provision(Hosts_by_name),

    % A process is statically spawned to listen for incoming queries
    % 
    Name = ?DNS_SERVER_NAME,
    Fun = fun() ->
                  dns_server(Port,Hosts_by_name)
          end,
    register(Name, Pid=spawn(Fun)),
    io:format("**DNS** server resistered with name::~p:: pid::~p:: ~n",[Name,Pid]).
