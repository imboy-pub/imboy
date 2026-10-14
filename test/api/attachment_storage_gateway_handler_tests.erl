-module(attachment_storage_gateway_handler_tests).
%%%
% W2-01 受控存储网关（02 §14 写字节边界）单测。
%
% 字节流经真实 0600 临时文件落盘并计算 SHA256（file_sha256 真路径，非 mock）；
% pin/对象存储 PUT 按场景 mock：
%   * pin ok → 冻结签名 Context（expires_at=capability exp，非底层现算）
%     传 elib_oss:put_object_from_file_ctx，200；
%   * 同 key 不同 receipt → 409 且 provider PUT=0；
%   * 超限 → 413 且 pin 未调用；
%   * pin not_found → 404；PUT 失败 → 502；Context 非法 → 410。
%%%

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(KEY, <<"u9/file/20261014/abc/t.bin">>).
-define(MIME, <<"application/octet-stream">>).
-define(EXP, 1893456000).

%% gregorian 秒 = unix 秒 + 62167219200（1970-01-01）。
exp_datetime() ->
    calendar:gregorian_seconds_to_datetime(?EXP + 62167219200).

setup_mocks(PinResult, PutResult, MaxBytes) ->
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

run_put() ->
    attachment_storage_gateway_handler:put(<<"PUT">>, req, #{}),
    get(gw_reply).

pin_ok_returns_200_with_frozen_ctx_test_() ->
    ?WITH_MECKS(
        setup_mocks(ok, ok, 1048576),
        fun() ->
            {200, Body} = run_put(),
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
        setup_mocks({error, content_mismatch}, ok, 1048576),
        fun() ->
            {409, _} = run_put(),
            %% 同 key 不同字节：provider PUT=0
            ?assertEqual(undefined, get(gw_put_args))
        end
    ).

oversize_413_pin_not_called_test_() ->
    ?WITH_MECKS(
        setup_mocks(ok, ok, 2),
        fun() ->
            {413, _} = run_put(),
            ?assertEqual(undefined, get(gw_pin_args)),
            ?assertEqual(undefined, get(gw_put_args))
        end
    ).

pin_unknown_session_404_test_() ->
    ?WITH_MECKS(
        setup_mocks({error, not_found}, ok, 1048576),
        fun() ->
            {404, _} = run_put(),
            ?assertEqual(undefined, get(gw_put_args))
        end
    ).

put_failure_502_keeps_pin_test_() ->
    ?WITH_MECKS(
        setup_mocks(ok, {error, {http_status, 500, <<"boom">>}}, 1048576),
        fun() ->
            {502, _} = run_put(),
            %% pin 已固定（重试同 receipt 合法），失败点在 provider PUT
            ?assertMatch({?KEY, _}, get(gw_pin_args))
        end
    ).

invalid_ctx_410_test_() ->
    ?WITH_MECKS(
        setup_mocks(ok, {error, invalid_signature_context}, 1048576),
        fun() ->
            {410, _} = run_put()
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
            {403, _} = run_put()
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
            {410, _} = run_put()
        end
    ).
