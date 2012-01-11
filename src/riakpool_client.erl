%% @author David Weldon
%% @doc riakpool_client is a collection of convenience functions for using
%% riakpool.

-module(riakpool_client).
-compile({no_auto_import,[put/2]}).
-export([delete/2, get/2, list_keys/1, put/2, put/3, put_encrypted/4, get_encrypted/3]).

%% @doc Delete `Key' from `Bucket'.
-spec delete(binary(), binary()) -> ok.
delete(Bucket, Key) ->
    riakpool:execute(fun(C) -> riakc_pb_socket:delete(C, Bucket, Key) end), ok.

%% @doc Returns the value associated with `Key' in `Bucket' as `{ok, binary()}'.
%% If an error was encountered or the value was not present, returns
%% `{error, any()}'.
-spec get(binary(), binary()) -> {ok, binary()} | {error, any()}.
get(Bucket, Key) ->
    Fun =
        fun(C) ->
            case riakc_pb_socket:get(C, Bucket, Key) of
                {ok, O} -> riakc_obj:get_value(O);
                {error, E} -> {error, E}
            end
        end,
    case riakpool:execute(Fun) of
        {ok, Value} when is_binary(Value) -> {ok, Value};
        {ok, {error, E}} -> {error, E};
        {error, E} -> {error, E}
    end.

-spec get_encrypted(binary(), binary(), binary()) -> {ok, binary()} | {error, any()}.
get_encrypted(Bucket, Key, EncryptionKey) ->
    case get(Bucket, Key) of
        {ok, Value} ->
            <<IVec:16/binary, Cipher/binary>> = Value,
            {ok, crypto:aes_ctr_decrypt(EncryptionKey, IVec, Cipher)};
        {error, E} -> {error, E}
    end.

%% @doc Returns the list of keys in `Bucket' as `{ok, list()}'. If an error was
%% encountered, returns `{error, any()}'.
-spec list_keys(binary()) -> {ok, list()} | {error, any()}.
list_keys(Bucket) ->
    Fun = fun(C) -> riakc_pb_socket:list_keys(C, Bucket) end,
    case riakpool:execute(Fun) of
        {ok, {ok, Keys}} -> {ok, Keys};
        {error, E} -> {error, E}
    end.

-spec put(binary(), binary()) -> {ok, binary()} | {error, any()}.
put(Bucket, Value) ->
    Fun =
        fun(C) ->
            Obj = riakc_obj:new(Bucket, undefined, Value),
            riakc_pb_socket:put(C, Obj)
        end,
    case riakpool:execute(Fun) of
        {ok, {ok, Keys}} -> {ok, Keys};
        {error, E} -> {error, E}
    end.

%% @doc Associates `Key' with `Value' in `Bucket'. If `Key' already exists in
%% `Bucket', an update will be performed.
-spec put(binary(), binary(), binary()) -> ok.
put(Bucket, Key, Value) ->
    Fun =
        fun(C) ->
            Obj = case riakc_pb_socket:get(C, Bucket, Key, [deletedvclock]) of
                {ok, O} ->
                    riakc_obj:update_value(O, Value);
                {error, notfound, VClock} ->
                    riakc_obj:set_vclock(riakc_obj:new(Bucket, Key, Value), VClock);
                {error, notfound} ->
                    riakc_obj:new(Bucket, Key, Value)
            end,
            riakc_pb_socket:put(C, Obj)
        end,
    riakpool:execute(Fun), ok.

-spec put_encrypted(binary(), binary(), binary(), binary()) -> ok.
put_encrypted(Bucket, Key, Value, EncryptionKey) ->
    IVec = crypto:rand_bytes(16),
    Cipher = crypto:aes_ctr_encrypt(EncryptionKey, IVec, Value),
    Data = <<IVec/binary, Cipher/binary>>,
    put(Bucket, Key, Data).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

client_test_() ->
    {foreach,
        fun() ->
            application:start(riakpool),
            riakpool:start_pool()
        end,
        fun(_) ->
            ok
        end,
        [
            {"client test",
                fun() ->
                    {B, K, V1, V2} = {<<"groceries">>, <<"mine">>, <<"eggs">>, <<"toast">>},
                    ?assertEqual({ok, []}, list_keys(B)),
                    ?assertMatch({error, _}, get(B, K)),
                    ?assertEqual(ok, put(B, K, V1)),
                    ?assertEqual({ok, V1}, get(B, K)),
                    ?assertEqual(ok, put(B, K, V2)),
                    ?assertEqual(ok, delete(B, K)),
                    ?assertEqual(ok, put(B, K, V2)),
                    ?assertEqual({ok, V2}, get(B, K)),
                    ?assertEqual({ok, [K]}, list_keys(B)),
                    ?assertEqual(ok, delete(B, K)),
                    ?assertMatch({error, _}, get(B, K))
                end
            },
            {"key generation test",
                fun() ->
                    {B, V} = {<<"groceries">>, <<"eggs">>},
                    {ok, K} = put(B, V),
                    ?assertEqual({ok, V}, get(B, K)),
                    ?assertEqual(ok, delete(B, K))
                end
            },
            {"encryption test",
                fun() ->
                    {B, K, V1} = {<<"groceries">>, <<"mine">>, <<"eggs">>},
                    E = crypto:rand_bytes(32),
                    ?assertEqual(ok, put_encrypted(B, K, V1, E)),
                    ?assertEqual({ok, V1}, get_encrypted(B, K, E)),
                    ?assertEqual(ok, delete(B, K)),
                    ?assertMatch({error, _}, get(B, K))
                end
            }
        ]
    }.

-endif.
