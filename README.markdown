<p align="right">
    <img src="http://wiki.basho.com/images/riaklogo.png">
</p>

Overview
--------
riakpool is an application for maintaining a dynamic pool of protocol buffer
client connections to a riak database. It ensures that a given connection can
only be in use by one external process at a time.

Installation
------------
    $ git clone git@github.com:iac/riakpool.git
    $ cd riakpool
    $ make

Interface
---------
The following example gives an overview of the riakpool interface. Please see
the complete documentation by running `make doc`.

    1> application:start(riakpool).
    ok
    2> riakpool:start_pool("127.0.0.1", 8087).
    ok
    3> riakpool:execute(fun(C) -> riakc_pb_socket:ping(C) end).
    {ok,pong}
    4> riakpool_client:put(<<"groceries">>, <<"mine">>, <<"eggs">>).
    ok
    5> riakpool_client:get(<<"groceries">>, <<"mine">>).
    {ok,<<"eggs">>}
    6> riakpool_client:list_keys(<<"groceries">>).
    {ok,[<<"mine">>]}
    7> riakpool_client:delete(<<"groceries">>, <<"mine">>).
    ok
    8> riakpool:count().
    1

Note that the use of riakpool_client is completely optional - it is simply a
collection of convenience functions which call riakpool:execute/1.

Encrypting entries
-----------------
Encryption with the riakpool_client is dirt simple, it's using aes ctr and you
use it with a generated encryption key which must be either 128, 192 or 256 bits long.
Here's an example with a 256 bit key:

    1> application:start(riakpool).
    ok
    2> riakpool:start_pool("127.0.0.1", 8087).
    ok
    3> E = crypto:rand_bytes(32).
    <<145,27,186,215,66,142,235,12,55,125,158,80,6,188,238,21,
      253,102,129,127,32,106,119,20,4,121,94,95,131,...>>
    4> riakpool_client:put_encrypted(<<"groceries">>, <<"mine">>, <<"eggs">>, E).
    ok
    5> riakpool_client:get_encrypted(<<"groceries">>, <<"mine">>, E).
    {ok, <<"eggs">>}

Starting the Pool
-----------------
Prior to any calls to `riakpool:execute/1`, the pool must be started. This can
be accomplished in one of two ways:

1. Before the server is started, set the riakpool application environment
   variables `riakpool_host` and `riakpool_port`.
2. After the server is started, call `riakpool:start_pool/0` or
   `riakpool:start_pool/2` (see previous section).
