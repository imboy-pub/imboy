-module(olm_identity_logic_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_MECKS(Modules, Fun),
    (fun() ->
        ok = meck:new(Modules, [passthrough, no_link]),
        try
            Fun()
        after
            meck:unload(Modules)
        end
    end)()
).

%% ===================================================================
%% report_identity 参数校验
%% ===================================================================

report_identity_rejects_empty_keys_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        ?assertEqual(
            {error, <<"invalid_identity_keys">>},
            olm_identity_logic:report_identity(100, <<"dev-A">>, <<>>, <<>>, <<>>, <<"ios">>)
        )
    end).

report_identity_rejects_bad_args_test() ->
    ?assertEqual(
        {error, <<"bad_request">>},
        olm_identity_logic:report_identity(
            <<"not_int">>, <<"dev-A">>, <<"e">>, <<"c">>, <<"s">>, <<"ios">>
        )
    ).

report_identity_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, upsert_identity, 6, fun(_, _, _, _, _, _) -> {ok, 1} end),
        ?assertEqual(
            ok,
            olm_identity_logic:report_identity(
                100, <<"dev-A">>, <<"e">>, <<"c">>, <<"s">>, <<"ios">>
            )
        )
    end).

%% ===================================================================
%% report_one_time_keys 参数校验
%% ===================================================================

report_one_time_keys_rejects_empty_test() ->
    ?assertEqual(
        {error, <<"invalid_key_count">>},
        olm_identity_logic:report_one_time_keys(100, <<"dev-A">>, [], 100)
    ).

report_one_time_keys_rejects_invalid_format_test() ->
    %% 含空 key_base64 的条目应被拒
    ?assertEqual(
        {error, <<"invalid_key_format">>},
        olm_identity_logic:report_one_time_keys(100, <<"dev-A">>, [{<<"k1">>, <<>>}], 100)
    ).

report_one_time_keys_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, upsert_one_time_keys, 4, fun(_, _, Keys, _) ->
            {ok, length(Keys)}
        end),
        ?assertEqual(
            {ok, 2},
            olm_identity_logic:report_one_time_keys(
                100, <<"dev-A">>, [{<<"k1">>, <<"v1">>}, {<<"k2">>, <<"v2">>}], 100
            )
        )
    end).

%% ===================================================================
%% claim_keys 优先级：OTK 命中 → type=one_time
%% ===================================================================

claim_keys_prefers_one_time_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Identity = #{<<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"e">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, Identity} end),
        meck:expect(
            olm_identity_ds,
            claim_one_time_key,
            3,
            fun(_Uid, _Did, _By) ->
                {ok, #{<<"key_id">> => <<"otk-1">>, <<"key_base64">> => <<"A">>}}
            end
        ),
        {ok, Result} = olm_identity_logic:claim_keys(100, 200, <<"dev-B">>),
        ?assertEqual(<<"one_time">>, maps:get(<<"type">>, Result)),
        ?assertEqual(<<"otk-1">>, maps:get(<<"key_id">>, Result)),
        ?assertEqual(Identity, maps:get(<<"identity">>, Result))
    end).

%% claim_keys 优先级：OTK 耗尽 → fallback 兜底
claim_keys_falls_back_when_otk_exhausted_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Identity = #{<<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"e">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, Identity} end),
        meck:expect(
            olm_identity_ds,
            claim_one_time_key,
            3,
            fun(_Uid, _Did, _By) -> {error, exhausted} end
        ),
        meck:expect(
            olm_identity_ds,
            claim_fallback_key,
            2,
            fun(_Uid, _Did) -> {ok, #{<<"key_id">> => <<"fb-1">>, <<"key_base64">> => <<"B">>}} end
        ),
        {ok, Result} = olm_identity_logic:claim_keys(100, 200, <<"dev-B">>),
        ?assertEqual(<<"fallback">>, maps:get(<<"type">>, Result)),
        ?assertEqual(<<"fb-1">>, maps:get(<<"key_id">>, Result))
    end).

%% claim_keys：OTK + fallback 都耗尽 → no_prekey_available
claim_keys_returns_no_prekey_available_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Identity = #{<<"device_id">> => <<"dev-B">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, Identity} end),
        meck:expect(
            olm_identity_ds,
            claim_one_time_key,
            3,
            fun(_Uid, _Did, _By) -> {error, exhausted} end
        ),
        meck:expect(olm_identity_ds, claim_fallback_key, 2, fun(_Uid, _Did) ->
            {error, exhausted}
        end),
        ?assertEqual(
            {error, <<"no_prekey_available">>},
            olm_identity_logic:claim_keys(100, 200, <<"dev-B">>)
        )
    end).

%% claim_keys：对端未注册身份键 → device_not_registered
claim_keys_unknown_device_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, not_found} end),
        ?assertEqual(
            {error, <<"device_not_registered">>},
            olm_identity_logic:claim_keys(100, 200, <<"dev-B">>)
        )
    end).

%% ===================================================================
%% get_identity
%% ===================================================================

get_identity_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Row = #{<<"device_id">> => <<"dev-B">>, <<"curve25519_key">> => <<"c">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, Row} end),
        ?assertEqual({ok, Row}, olm_identity_logic:get_identity(200, <<"dev-B">>))
    end).

get_identity_not_found_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun(_Uid, _Did) -> {ok, not_found} end),
        ?assertEqual({error, <<"not_found">>}, olm_identity_logic:get_identity(200, <<"dev-B">>))
    end).

%% ===================================================================
%% list_devices（ADR 03 §8.1 统一设备列表）
%% ===================================================================

list_devices_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Devices = [
            #{<<"device_id">> => <<"phone-a">>, <<"capabilities">> => [<<"olm">>]},
            #{<<"device_id">> => <<"ipad-b">>, <<"capabilities">> => [<<"olm">>, <<"megolm">>]}
        ],
        meck:expect(olm_identity_ds, list_devices_with_identity, 1, fun(_) -> {ok, Devices} end),
        {ok, Payload} = olm_identity_logic:list_devices(200),
        ?assertEqual(200, maps:get(<<"user_id">>, Payload)),
        ?assertEqual(Devices, maps:get(<<"devices">>, Payload))
    end).

list_devices_empty_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, list_devices_with_identity, 1, fun(_) -> {ok, []} end),
        {ok, Payload} = olm_identity_logic:list_devices(200),
        ?assertEqual([], maps:get(<<"devices">>, Payload))
    end).

list_devices_rejects_bad_uid_test() ->
    ?assertEqual({error, <<"bad_request">>}, olm_identity_logic:list_devices(0)),
    ?assertEqual({error, <<"bad_request">>}, olm_identity_logic:list_devices(<<"x">>)).

list_devices_maps_ds_error_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, list_devices_with_identity, 1, fun(_) -> {error, db_down} end),
        ?assertEqual({error, <<"internal_error">>}, olm_identity_logic:list_devices(200))
    end).

%% ===================================================================
%% batch_claim_keys（ADR 03 §8.2 多设备 fan-out）
%% ===================================================================

%% 多设备各自 claim 成功，聚合到 claimed，failed 为空
batch_claim_all_ok_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Identity = #{<<"device_id">> => <<"d">>},
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) -> {ok, Identity} end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, Did, _) ->
            {ok, #{<<"key_id">> => <<"otk-", Did/binary>>, <<"key_base64">> => <<"A">>}}
        end),
        {ok, Payload} = olm_identity_logic:batch_claim_keys(100, 200, [<<"a">>, <<"b">>]),
        Claimed = maps:get(<<"claimed">>, Payload),
        ?assertEqual(2, maps:size(Claimed)),
        ?assertEqual(#{}, maps:get(<<"failed">>, Payload)),
        ?assertEqual(<<"one_time">>, maps:get(<<"type">>, maps:get(<<"a">>, Claimed)))
    end).

%% 部分设备未注册 → 该设备落 failed，不中断其他设备
batch_claim_partial_failure_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun
            (_, <<"good">>) -> {ok, #{<<"device_id">> => <<"good">>}};
            (_, <<"bad">>) -> {ok, not_found}
        end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {ok, #{<<"key_id">> => <<"otk-1">>, <<"key_base64">> => <<"A">>}}
        end),
        {ok, Payload} = olm_identity_logic:batch_claim_keys(100, 200, [<<"good">>, <<"bad">>]),
        ?assertEqual(1, maps:size(maps:get(<<"claimed">>, Payload))),
        Failed = maps:get(<<"failed">>, Payload),
        ?assertEqual(<<"device_not_registered">>, maps:get(<<"bad">>, Failed))
    end).

%% 去重：重复 device_id 只 claim 一次
batch_claim_dedups_device_ids_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, find_identity, 2, fun(_, _) ->
            {ok, #{<<"device_id">> => <<"a">>}}
        end),
        meck:expect(olm_identity_ds, claim_one_time_key, 3, fun(_, _, _) ->
            {ok, #{<<"key_id">> => <<"otk-1">>, <<"key_base64">> => <<"A">>}}
        end),
        {ok, Payload} = olm_identity_logic:batch_claim_keys(100, 200, [<<"a">>, <<"a">>, <<"a">>]),
        ?assertEqual(1, maps:size(maps:get(<<"claimed">>, Payload))),
        ?assertEqual(1, meck:num_calls(olm_identity_ds, claim_one_time_key, '_'))
    end).

batch_claim_rejects_empty_test() ->
    ?assertEqual(
        {error, <<"no_device_ids">>},
        olm_identity_logic:batch_claim_keys(100, 200, [])
    ),
    %% 全为非法元素过滤后为空
    ?assertEqual(
        {error, <<"no_device_ids">>},
        olm_identity_logic:batch_claim_keys(100, 200, [<<>>, 123])
    ).

batch_claim_rejects_too_many_test() ->
    Ids = [integer_to_binary(N) || N <- lists:seq(1, 21)],
    ?assertEqual(
        {error, <<"too_many_devices">>},
        olm_identity_logic:batch_claim_keys(100, 200, Ids)
    ).

batch_claim_rejects_bad_args_test() ->
    ?assertEqual(
        {error, <<"bad_request">>},
        olm_identity_logic:batch_claim_keys(<<"x">>, 200, [<<"a">>])
    ).

%% ===================================================================
%% cleanup_consumed_one_time_keys：retention 守卫 + days→seconds 换算 + 透传
%% ===================================================================

%% retention<=0：拒绝下探 DS（防删光审计行）
cleanup_rejects_zero_retention_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, cleanup_consumed_one_time_keys, 1, fun(_) -> {ok, 999} end),
        ?assertEqual(
            {error, <<"invalid_retention">>},
            olm_identity_logic:cleanup_consumed_one_time_keys(0)
        ),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, cleanup_consumed_one_time_keys, '_'))
    end).

cleanup_rejects_negative_retention_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, cleanup_consumed_one_time_keys, 1, fun(_) -> {ok, 999} end),
        ?assertEqual(
            {error, <<"invalid_retention">>},
            olm_identity_logic:cleanup_consumed_one_time_keys(-1)
        ),
        ?assertEqual(0, meck:num_calls(olm_identity_ds, cleanup_consumed_one_time_keys, '_'))
    end).

%% retention>0：换算 days*86400 传 DS（参数透传验证）+ 成功返回条数
cleanup_converts_days_to_seconds_and_passes_through_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        Captured = atomics:new(1, [{signed, true}]),
        meck:expect(olm_identity_ds, cleanup_consumed_one_time_keys, 1, fun(Seconds) ->
            atomics:put(Captured, 1, Seconds),
            {ok, 3}
        end),
        ?assertEqual({ok, 3}, olm_identity_logic:cleanup_consumed_one_time_keys(7)),
        %% 7 天 = 604800 秒
        ?assertEqual(604800, atomics:get(Captured, 1))
    end).

%% DS 报错：归一为 internal_error
cleanup_maps_ds_error_test() ->
    ?WITH_MECKS([olm_identity_ds], fun() ->
        meck:expect(olm_identity_ds, cleanup_consumed_one_time_keys, 1, fun(_) ->
            {error, db_down}
        end),
        ?assertEqual(
            {error, <<"internal_error">>},
            olm_identity_logic:cleanup_consumed_one_time_keys(7)
        )
    end).
