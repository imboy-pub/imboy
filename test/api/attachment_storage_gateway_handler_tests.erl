-module(attachment_storage_gateway_handler_tests).
%%%
% 受控存储网关单测（双向合并产物，两个互补套件）：
%
% A. repo 链路矩阵：真实 attach_pending_repo + mock elib_pg，12 场景覆盖
%    register_content_pin 错误映射（23510→409 / P0002→404 / 异常形态→500）
%    与读写健壮性（body/write 异常、provider 错误与异常、临时文件清理）。
% B. W2-01 合同套件（02 §14/§15）：冻结签名 Context（expires_at=capability
%    exp 的绝对时间）、多段 ObjectKey 平坦化落盘、409 时 provider PUT=0、
%    413 不触发 pin、404/502/410 状态面、403/410 能力门。
%
% 移植约定：provider PUT 统一为冻结 Context 版 put_object_from_file_ctx/5；
% 回包携带请求原始 Req（网关不把 read_body 更新后的 Req 透传给 reply）。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%% ===================================================================
%% A. repo 链路矩阵（真实 repo + mock elib_pg）
%% ===================================================================

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
        ?assertEqual(Calls, meck:num_calls(elib_oss, put_object_from_file_ctx, '_')),
        case Calls of
            1 -> ?assertMatch(#{<<"size_bytes">> := <<"7">>}, get(gateway_pin));
            _ -> ok
        end,
        ?assertEqual(
            [],
            filelib:wildcard(
                tmp_base() ++ "/" ++ binary_to_list(get(gateway_key)) ++ ".*"
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
    meck:expect(elib_oss, put_object_from_file_ctx, fun(_, _, _, _, _) ->
        ?assertMatch(#{<<"size_bytes">> := <<"7">>}, get(gateway_pin)),
        {error, timeout}
    end);
setup_fault(provider_exception) ->
    meck:expect(elib_oss, put_object_from_file_ctx, fun(_, _, _, _, _) ->
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
    Tmp = tmp_base() ++ "/pinned-success",
    try
        ok = file:write_file(Tmp, <<"payload">>),
        setup_mocks(ok),
        {Status, Headers, Body} = attachment_storage_gateway_handler:serve_pinned(
            #{read => true},
            get(gateway_key),
            <<"text/plain">>,
            erlang:system_time(second) + 600,
            Tmp,
            7
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
    meck:expect(cowboy_req, reply, fun(Status, Headers, Body, _Req) -> {Status, Headers, Body} end),
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
    meck:expect(elib_oss, put_object_from_file_ctx, fun(_, KeyArg, Tmp, Mime, _Ctx) ->
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
    ?assertEqual(1, meck:num_calls(elib_oss, put_object_from_file_ctx, '_'));
assert_response(Result, Status, _, Body) ->
    {ExpectedStatus, ExpectedBody, Calls} =
        case Result of
            content_mismatch -> {409, <<"content already pinned to different receipt">>, 0};
            not_found -> {404, <<"unknown session">>, 0};
            put_failed -> {502, <<"object store put failed, retry same content">>, 1}
        end,
    ?assertEqual(ExpectedStatus, Status),
    ?assertEqual(ExpectedBody, Body),
    ?assertEqual(Calls, meck:num_calls(elib_oss, put_object_from_file_ctx, '_')).

%% 与 handler:tmp_dir/0 同口径的兜底（环境变量未设时不用 false 拼串）。
tmp_base() ->
    case os:getenv("IMBOY_V5_GATEWAY_TMP") of
        false -> "/tmp/imboy-v5-gateway";
        D -> D
    end.

%% ===================================================================
%% B. W2-01 合同套件（冻结 Context / SafeKey / 状态面）
%% ===================================================================

-define(KEY, <<"u9/file/20261014/abc/t.bin">>).
-define(MIME, <<"application/octet-stream">>).
-define(EXP, 1893456000).

%% gregorian 秒 = unix 秒 + 62167219200（1970-01-01）。
exp_datetime() ->
    calendar:gregorian_seconds_to_datetime(?EXP + 62167219200).

ctx_setup_mocks(PinResult, PutResult, MaxBytes) ->
    application:set_env(imboy, garage, #{
        endpoint => <<"http://127.0.0.1:3900">>,
        bucket => <<"imboy">>,
        access_key => <<"GKtest">>,
        secret_key => <<"testsecret">>
    }),
    erase(gw_put_args),
    erase(gw_pin_args),
    erase(gw_reply),
    [
        {imboy_jwt, [
            {'verify_upload_capability', 1, fun(_Tok) ->
                {ok, #{<<"key">> => ?KEY, <<"mime">> => ?MIME, <<"exp">> => ?EXP}}
            end}
        ]},
        {attach_pending_repo, [
            {'register_content_pin', 2, fun(Key, Receipt) ->
                put(gw_pin_args, {Key, Receipt}),
                PinResult
            end}
        ]},
        {elib_oss, [
            {'get_bucket', 1, fun(<<"private">>) -> <<"b-priv">> end},
            {'max_file_size', 0, fun() -> MaxBytes end},
            {'put_object_from_file_ctx', 5, fun(_Bucket, Key, Path, Mime, Ctx) ->
                %% 真实转发路径断言：同一落盘文件内容进入 provider 调用
                put(gw_put_args, {Key, Mime, Ctx, file:read_file(Path)}),
                PutResult
            end}
        ]},
        {cowboy_req, [
            {'header', 2, fun(_H, _R) -> undefined end},
            {'parse_qs', 1, fun(_R) -> [{<<"capability">>, <<"TOK">>}] end},
            {'read_body', 2, fun(_R, _O) -> {ok, <<"DATA">>, req} end},
            {'reply', 4, fun(Status, _H, Body, R) ->
                put(gw_reply, {Status, Body}),
                R
            end}
        ]}
    ].

ctx_run_put() ->
    attachment_storage_gateway_handler:put(<<"PUT">>, req, #{}),
    get(gw_reply).

pin_ok_returns_200_with_frozen_ctx_test_() ->
    ?WITH_MECKS(
        ctx_setup_mocks(ok, ok, 1048576),
        fun() ->
            {200, Body} = ctx_run_put(),
            #{<<"pinned">> := true} = jsone:decode(Body),
            %% provider 恰好一次 PUT：转发同一落盘文件内容，key/mime 与声明一致
            {Key, Mime, Ctx, {ok, <<"DATA">>}} = get(gw_put_args),
            ?assertEqual(?KEY, Key),
            ?assertEqual(?MIME, Mime),
            %% 冻结 Context：expires_at 严格等于 capability exp 的绝对时间，
            %% issued_at 为合法 UTC datetime 且不晚于 expires_at
            ?assertEqual(exp_datetime(), maps:get(expires_at, Ctx)),
            Issued = maps:get(issued_at, Ctx),
            ?assert(
                calendar:datetime_to_gregorian_seconds(Issued) =<
                    calendar:datetime_to_gregorian_seconds(exp_datetime())
            )
        end
    ).

pin_mismatch_409_and_zero_provider_put_test_() ->
    ?WITH_MECKS(
        ctx_setup_mocks({error, content_mismatch}, ok, 1048576),
        fun() ->
            {409, _} = ctx_run_put(),
            %% 同 key 不同字节：provider PUT=0
            ?assertEqual(undefined, get(gw_put_args))
        end
    ).

oversize_413_pin_not_called_test_() ->
    ?WITH_MECKS(
        ctx_setup_mocks(ok, ok, 2),
        fun() ->
            {413, _} = ctx_run_put(),
            ?assertEqual(undefined, get(gw_pin_args)),
            ?assertEqual(undefined, get(gw_put_args))
        end
    ).

pin_unknown_session_404_test_() ->
    ?WITH_MECKS(
        ctx_setup_mocks({error, not_found}, ok, 1048576),
        fun() ->
            {404, _} = ctx_run_put(),
            ?assertEqual(undefined, get(gw_put_args))
        end
    ).

put_failure_502_keeps_pin_test_() ->
    ?WITH_MECKS(
        ctx_setup_mocks(ok, {error, {http_status, 500, <<"boom">>}}, 1048576),
        fun() ->
            {502, _} = ctx_run_put(),
            %% pin 已固定（重试同 receipt 合法），失败点在 provider PUT
            ?assertMatch({?KEY, _}, get(gw_pin_args))
        end
    ).

invalid_ctx_410_test_() ->
    ?WITH_MECKS(
        ctx_setup_mocks(ok, {error, invalid_signature_context}, 1048576),
        fun() ->
            {410, _} = ctx_run_put()
        end
    ).

bad_token_403_test_() ->
    ?WITH_MECKS(
        [
            {imboy_jwt, [
                {'verify_upload_capability', 1, fun(_Tok) -> {error, invalid_signature} end}
            ]},
            {cowboy_req, [
                {'header', 2, fun(_H, _R) -> undefined end},
                {'parse_qs', 1, fun(_R) -> [{<<"capability">>, <<"BAD">>}] end},
                {'reply', 4, fun(Status, _H, Body, R) ->
                    put(gw_reply, {Status, Body}),
                    R
                end}
            ]}
        ],
        fun() ->
            {403, _} = ctx_run_put()
        end
    ).

expired_token_410_test_() ->
    ?WITH_MECKS(
        [
            {imboy_jwt, [
                {'verify_upload_capability', 1, fun(_Tok) ->
                    {ok, #{<<"key">> => ?KEY, <<"mime">> => ?MIME, <<"exp">> => 1}}
                end}
            ]},
            {cowboy_req, [
                {'header', 2, fun(_H, _R) -> undefined end},
                {'parse_qs', 1, fun(_R) -> [{<<"capability">>, <<"TOK">>}] end},
                {'reply', 4, fun(Status, _H, Body, R) ->
                    put(gw_reply, {Status, Body}),
                    R
                end}
            ]}
        ],
        fun() ->
            {410, _} = ctx_run_put()
        end
    ).
