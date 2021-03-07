dns-server
==========

An DNS server deployed as an OTP application, just for fun with Erlang :).

In a WIP status ...

Build
-----

    $ rebar3 compile

Start
-----

    $ rebar3 shell

    Start Mnesia:

    1> dns_db_server:init().
    stopped

    =INFO REPORT==== 9-Dec-2016::21:14:04 ===
        application: mnesia
        exited: stopped
        type: temporary
    2> dns_db_server:start().
    ok
    3> application:load(dns_server).
    ok

    Start the server:

    4> application:loaded_applications().
    ...
    [{dns_server,"A DNS server","..."},
    ...
    5> application:start(dns_server).
