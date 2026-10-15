-module(attachment_storage_gateway_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

repo_chain_test_() ->
    [
        ?_test(run_chain(Result))
     || Result <- [
            true,
            false,
            null,
            missing,
            empty,
            mismatch,
            absent,
            db_error,
            body_exception,
            write_exception,
            provider_error,
            provider_exception
        ]
    ].

run_chain(Result) ->
    Modules = [cowboy_req, imboy_jwt, elib_oss, elib_pg, elib_pg_sql, attach_pending_repo],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict]) end, Modules),
    try
        setup_mocks(ok),
        meck:unload(attach_pending_repo),
        setup_chain(Result),
        {Status, Headers, Body} = attachment_storage_gateway_handler:put(<<"PUT">>, #{}, #{}),
        Expected = chain_status(Result),
        ?assertEqual(Expected, Status),
        case Expected of
            200 -> assert_response(ok, Status, Headers, Body);
            _ -> ?assert(is_binary(Body))
        end,
        Calls =
            case Result of
                true -> 1;
                provider_error -> 1;
                provider_exception -> 1;
                _ -> 0
            end,
        ?assertEqual(Calls, meck:num_calls(elib_oss, put_object_from_file, '_')),
        case Calls of
            1 -> ?assertMatch(#{<<"size_bytes">> := <<"7">>}, get(gateway_pin));
            _ -> ok
        end,
        ?assertEqual(
            [],
            filelib:wildcard(
                gateway_tmp_dir() ++ "/" ++
                    binary_to_list(get(gateway_key)) ++ ".*"
            )
        )
    after
        safe_unload(file),
        lists:foreach(fun safe_unload/1, Modules),
        erase(gateway_pin),
        erase(gateway_key)
    end.

safe_unload(Module) ->
    try
        meck:unload(Module)
    catch
        error:{not_mocked, Module} -> ok
    end.

setup_chain(Result) ->
    meck:expect(elib_pg_sql, public_tablename, fun(_) -> <<"attach_pending">> end),
    meck:expect(elib_pg, query, fun(Sql, Params) ->
        case binary:match(iolist_to_binary(Sql), <<"fn_v5_pin_pending_content">>) of
            nomatch ->
                {ok, [#{<<"object_key">> => get(gateway_key)}]};
            _ ->
                [_, Json] = Params,
                put(gateway_pin, jsone:decode(Json)),
                pin_result(Result)
        end
    end),
    setup_fault(Result).

pin_result(mismatch) ->
    {error, #error{code = <<"23510">>}};
pin_result(absent) ->
    {error, #error{code = <<"P0002">>}};
pin_result(db_error) ->
    {error, #error{code = <<"XX000">>}};
pin_result(missing) ->
    {ok, [#{}]};
pin_result(empty) ->
    {ok, []};
pin_result(Value) when Value =:= false; Value =:= null ->
    {ok, [#{<<"fn_v5_pin_pending_content">> => Value}]};
pin_result(_) ->
    {ok, [#{<<"fn_v5_pin_pending_content">> => true}]}.

setup_fault(body_exception) ->
    meck:expect(cowboy_req, read_body, fun(_, _) -> erlang:error(body_failed) end);
setup_fault(write_exception) ->
    meck:new(file, [unstick, passthrough]),
    meck:expect(file, write, fun(_, _) -> erlang:error(write_failed) end);
setup_fault(provider_error) ->
    meck:expect(elib_oss, put_object_from_file, fun(_, _, _, _) ->
        ?assertMatch(#{<<"size_bytes">> := <<"7">>}, get(gateway_pin)),
        {error, timeout}
    end);
setup_fault(provider_exception) ->
    meck:expect(elib_oss, put_object_from_file, fun(_, _, _, _) ->
        ?assertMatch(#{<<"size_bytes">> := <<"7">>}, get(gateway_pin)),
        erlang:error(provider_failed)
    end);
setup_fault(_) ->
    ok.

chain_status(true) -> 200;
chain_status(mismatch) -> 409;
chain_status(absent) -> 404;
chain_status(provider_error) -> 502;
chain_status(provider_exception) -> 502;
chain_status(_) -> 500.

gateway_put_test_() ->
    [?_test(run_put(Result)) || Result <- [ok, content_mismatch, not_found, put_failed]].

pinned_success_test() ->
    Modules = [cowboy_req, imboy_jwt, attach_pending_repo, elib_oss],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict]) end, Modules),
    Tmp = gateway_tmp_dir() ++ "/pinned-success",
    try
        ok = file:write_file(Tmp, <<"payload">>),
        setup_mocks(ok),
        {Status, Headers, Body} = attachment_storage_gateway_handler:serve_pinned(
            #{read => true}, get(gateway_key), <<"text/plain">>, Tmp, 7
        ),
        assert_response(ok, Status, Headers, Body)
    after
        file:delete(Tmp),
        lists:foreach(fun(Module) -> meck:unload(Module) end, Modules),
        erase(gateway_pin),
        erase(gateway_key)
    end.

run_put(Result) ->
    Modules = [cowboy_req, imboy_jwt, attach_pending_repo, elib_oss],
    lists:foreach(fun(Module) -> meck:new(Module, [non_strict]) end, Modules),
    try
        setup_mocks(Result),
        {Status, Headers, Body} = attachment_storage_gateway_handler:put(<<"PUT">>, #{}, #{}),
        assert_response(Result, Status, Headers, Body),
        ?assert(meck:validate(cowboy_req)),
        ?assert(meck:validate(elib_oss))
    after
        lists:foreach(fun(Module) -> meck:unload(Module) end, Modules),
        erase(gateway_pin),
        erase(gateway_key)
    end.

setup_mocks(Result) ->
    Key = integer_to_binary(erlang:unique_integer([positive])),
    put(gateway_key, Key),
    meck:expect(cowboy_req, header, fun(_, _) -> <<"capability">> end),
    meck:expect(cowboy_req, read_body, fun(Req, _) -> {ok, <<"payload">>, Req#{read => true}} end),
    meck:expect(cowboy_req, reply, fun(Status, Headers, Body, Req) ->
        case Status of
            500 -> ok;
            _ -> ?assertEqual(true, maps:get(read, Req))
        end,
        {Status, Headers, Body}
    end),
    meck:expect(imboy_jwt, verify_upload_capability, fun(_) ->
        {ok, #{
            <<"key">> => Key,
            <<"mime">> => <<"text/plain">>,
            <<"exp">> => erlang:system_time(second) + 60
        }}
    end),
    meck:expect(elib_oss, max_file_size, fun() -> 1024 end),
    meck:expect(elib_oss, get_bucket, fun(_) -> <<"bucket">> end),
    meck:expect(attach_pending_repo, register_content_pin, fun(KeyArg, Receipt) ->
        ?assertEqual(Key, KeyArg),
        put(gateway_pin, Receipt),
        case Result of
            content_mismatch -> {error, content_mismatch};
            not_found -> {error, not_found};
            _ -> ok
        end
    end),
    meck:expect(elib_oss, put_object_from_file, fun(_, KeyArg, Tmp, Mime) ->
        ?assertEqual(Key, KeyArg),
        ?assertEqual(<<"text/plain">>, Mime),
        ?assertEqual({ok, <<"payload">>}, file:read_file(Tmp)),
        ?assertMatch(#{<<"size_bytes">> := <<"7">>}, get(gateway_pin)),
        case Result of
            put_failed -> {error, put_failed};
            _ -> ok
        end
    end).

assert_response(ok, Status, Headers, Body) ->
    ?assertEqual(200, Status),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers)),
    Receipt = get(gateway_pin),
    ?assertEqual(
        <<"239f59ed55e737c77147cf55ad0c1b030b6d7ee748a7426952f9b852d5a935e5">>,
        maps:get(<<"sha256">>, Receipt)
    ),
    ?assertEqual(
        #{
            <<"pinned">> => true,
            <<"sha256">> => maps:get(<<"sha256">>, Receipt),
            <<"size">> => 7
        },
        jsone:decode(Body)
    ),
    ?assertEqual(1, meck:num_calls(elib_oss, put_object_from_file, '_'));
assert_response(Result, Status, _, Body) ->
    {ExpectedStatus, ExpectedBody, Calls} =
        case Result of
            content_mismatch -> {409, <<"content already pinned to different receipt">>, 0};
            not_found -> {404, <<"unknown session">>, 0};
            put_failed -> {502, <<"object store put failed, retry same content">>, 1}
        end,
    ?assertEqual(ExpectedStatus, Status),
    ?assertEqual(ExpectedBody, Body),
    ?assertEqual(Calls, meck:num_calls(elib_oss, put_object_from_file, '_')).

%% CI 环境合同：nightly full-eunit 不注入 IMBOY_V5_GATEWAY_TMP，裸拼接
%% os:getenv 结果（未设置时为原子 false）会 badarg。与生产
%% attachment_storage_gateway_handler:tmp_dir/0 同款回退，并确保目录存在。
gateway_tmp_dir() ->
    D =
        case os:getenv("IMBOY_V5_GATEWAY_TMP") of
            false -> "/tmp/imboy-v5-gateway";
            P -> P
        end,
    ok = filelib:ensure_dir(filename:join(D, "keep")),
    D.
