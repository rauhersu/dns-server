dns_server
=====

An DNS server deployed as an OTP application, just for fun with Erlang :).

In a WIP status ...

Build
-----

    $ rebar3 compile

Start
-----

    $ erl

    1> code:add_path("./_build/default/lib/dns_server/ebin").
    true

    Start Mnesia:

    2> dns_db_server:init().
    stopped

    =INFO REPORT==== 9-Dec-2016::21:14:04 ===
        application: mnesia
        exited: stopped
        type: temporary
    3> dns_db_server:start().
    ok
    4> application:load(dns_server).
    ok

    Start the server:

    5> application:loaded_applications().
    [{dns_server,"A DNS server","0.0.0+build.11.ref1e538cc"},
    {kernel,"ERTS  CXC 138 10","5.1"},
    {stdlib,"ERTS  CXC 138 10","3.1"}]
    6> application:start(dns_server).
