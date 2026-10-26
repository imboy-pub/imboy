-module(epgsql_codec_rfc3339_bin_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% epgsql_codec_rfc3339_bin 模块的 EUnit 测试
%%%
%%% 目标：验证 PostgreSQL RFC3339 时间编解码功能
%%% R19（IMBOY-TIMEZONE-REPAIR-V1）：
%%%   - 有符号 64 位解码（2000 年前负微秒不再误读为超大正数）
%%%   - decode 输出恒为 UTC "Z"（不泄漏 BEAM 服务器本地时区偏移）
%%%   - 非法 RFC3339 输入 encode 抛明确错误（不再伪造 PG epoch 0 日期）
%%%   - 保留 tuple 兼容、合法 offset、微秒精度
%%%===================================================================

encode_rfc3339_timestamp_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 使用 RFC3339 格式的时间戳（UTC 时区）
        Timestamp = <<"2024-01-01T12:00:00Z">>,
        % 测试编码功能（需要3个参数）
        Result =
            try
                EncodedValue = epgsql_codec_rfc3339_bin:encode(
                    Timestamp, timestamptz, epgsql_idatetime
                ),
                {ok, EncodedValue}
            catch
                _:_ -> {error, encode_failed}
            end,
        % 验证编码成功
        ?assertMatch({ok, _}, Result)
    end).

decode_rfc3339_timestamp_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 使用 64 位整数表示的 PostgreSQL 时间戳
        % 2024-01-01 12:00:00 UTC 对应的微秒数
        PgTimestamp = <<1577880000000000:64/big-signed-integer>>,
        % 测试解码功能
        Decoded = epgsql_codec_rfc3339_bin:decode(PgTimestamp, timestamptz, epgsql_idatetime),
        ?assertMatch(<<_/binary>>, Decoded),
        % 当前实现返回 RFC3339 格式（带 T 和时区偏移）
        ?assertMatch(
            <<_:4/binary, $-, _:2/binary, $-, _:2/binary, $T, _:2/binary, $:, _:2/binary, $:,
                _:2/binary, _/binary>>,
            Decoded
        )
    end).

encode_decode_roundtrip_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 使用 RFC3339 格式的时间戳
        OriginalTimestamp = <<"2024-12-25T10:30:45Z">>,
        % 编码
        {ok, Encoded} =
            try
                {ok,
                    epgsql_codec_rfc3339_bin:encode(
                        OriginalTimestamp, timestamptz, epgsql_idatetime
                    )}
            catch
                _:_ -> {error, encode_failed}
            end,
        % 解码
        Decoded = epgsql_codec_rfc3339_bin:decode(Encoded, timestamptz, epgsql_idatetime),
        % 验证往返转换成功（允许时区差异）
        ?assertMatch(<<_/binary>>, Decoded)
    end).

encode_with_invalid_input_test_() ->
    ?TEST_WITH_APP(fun() ->
        % R19：非法 RFC3339 输入必须抛明确错误（事务失败），
        % 不得再回退 <<0:64>>（= PG epoch 0 → 伪造的 2000-01-01 日期）。
        ?assertException(
            error,
            {invalid_timestamptz_input, <<"invalid">>},
            epgsql_codec_rfc3339_bin:encode(<<"invalid">>, timestamptz, epgsql_idatetime)
        )
    end).

decode_with_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试二进制解码
        TimestampBin = <<1577880000000000:64/big-signed-integer>>,
        Decoded = epgsql_codec_rfc3339_bin:decode(TimestampBin, timestamptz, epgsql_idatetime),
        ?assertMatch(<<_/binary>>, Decoded)
    end).

%%%-------------------------------------------------------------------
%%% R19-1：有符号 64 位解码——PG epoch 之前的负微秒（1999 年）
%%%-------------------------------------------------------------------
decode_pre_2000_negative_microseconds_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 1999-12-31T23:59:59.123456Z 的 Unix 微秒 = 946684799123456；
        % PG 内部值 = Unix 微秒 - 946684800000000 = -876544（负数）。
        % 原 decode_unsigned(big) 把它误读为 1.8e25 级超大正数 → 离谱未来日期。
        PgMicro = 946684799123456 - 946684800000000,
        ?assert(PgMicro < 0),
        Decoded = epgsql_codec_rfc3339_bin:decode(
            <<PgMicro:64/big-signed-integer>>, timestamptz, epgsql_idatetime
        ),
        ?assertEqual(<<"1999-12-31T23:59:59.123456Z">>, Decoded)
    end).

decode_pg_epoch_zero_is_2000_test_() ->
    ?TEST_WITH_APP(fun() ->
        % PG epoch 0 = 2000-01-01T00:00:00Z（合法零值，不得与伪造值混淆）
        Decoded = epgsql_codec_rfc3339_bin:decode(
            <<0:64/big-signed-integer>>, timestamptz, epgsql_idatetime
        ),
        ?assertEqual(<<"2000-01-01T00:00:00.000000Z">>, Decoded)
    end).

decode_output_is_utc_z_test_() ->
    ?TEST_WITH_APP(fun() ->
        % R19：decode 输出显式 "Z"，与 BEAM 服务器本地时区设置无关
        % （原实现 offset "" 会输出服务器本地偏移，如 +08:00）。
        PgMicro = 1789600000000000 - 946684800000000,
        Decoded = epgsql_codec_rfc3339_bin:decode(
            <<PgMicro:64/big-signed-integer>>, timestamptz, epgsql_idatetime
        ),
        %% calendar 微秒输出恒带 6 位小数
        ?assertEqual(<<"2026-09-16T23:06:40.000000Z">>, Decoded)
    end).

%%%-------------------------------------------------------------------
%%% R19-2：微秒精度不丢
%%%-------------------------------------------------------------------
microsecond_precision_roundtrip_test_() ->
    ?TEST_WITH_APP(fun() ->
        Original = <<"2026-09-16T23:06:40.123456Z">>,
        Encoded = epgsql_codec_rfc3339_bin:encode(Original, timestamptz, epgsql_idatetime),
        Decoded = epgsql_codec_rfc3339_bin:decode(Encoded, timestamptz, epgsql_idatetime),
        ?assertEqual(Original, Decoded)
    end).

%%%-------------------------------------------------------------------
%%% R19-3：合法 offset 与 Z 编码等价（同一瞬间 → 同一 PG 内部值）
%%%-------------------------------------------------------------------
encode_offset_equivalence_test_() ->
    ?TEST_WITH_APP(fun() ->
        Zulu = epgsql_codec_rfc3339_bin:encode(
            <<"2026-09-16T23:06:40Z">>, timestamptz, epgsql_idatetime
        ),
        Plus8 = epgsql_codec_rfc3339_bin:encode(
            <<"2026-09-17T07:06:40+08:00">>, timestamptz, epgsql_idatetime
        ),
        ?assertEqual(Zulu, Plus8)
    end).

%%%-------------------------------------------------------------------
%%% R19-5：calendar datetime 元组写入回归（agent_grant/agent_run 冻结合同）
%%%-------------------------------------------------------------------
encode_tuple_regression_test_() ->
    ?TEST_WITH_APP(fun() ->
        Encoded = epgsql_codec_rfc3339_bin:encode(
            {{2024, 1, 1}, {12, 0, 0}}, timestamptz, epgsql_idatetime
        ),
        Decoded = epgsql_codec_rfc3339_bin:decode(Encoded, timestamptz, epgsql_idatetime),
        ?assertEqual(<<"2024-01-01T12:00:00.000000Z">>, Decoded)
    end).

%%%-------------------------------------------------------------------
%%% R19-1/R19-4：真实 PG 往返（注册 codec 的池化连接）
%%% 1999 负微秒往返 + 非法参数显式失败，不产生伪造行。
%%% DB 不可用时该组按 TEST_WITH_DB 惯例 skip（不计通过）。
%%%-------------------------------------------------------------------
pg_roundtrip_1999_and_now_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, [#{<<"ts">> := Ts1999}]} = elib_pg:query(
            <<"SELECT $1::timestamptz AS ts">>, [<<"1999-12-31T23:59:59.123456Z">>]
        ),
        ?assertEqual(<<"1999-12-31T23:59:59.123456Z">>, Ts1999),

        NowMs = elib_dt:millisecond(),
        NowRfc = elib_dt:to_rfc3339(NowMs, millisecond, "Z"),
        {ok, [#{<<"ts">> := TsNow}]} = elib_pg:query(
            <<"SELECT $1::timestamptz AS ts">>, [NowRfc]
        ),
        %% 往返不得改变同一瞬间（毫秒精度经 PG 微秒列无损保留）
        ?assertEqual(
            elib_dt:rfc3339_to(NowRfc, millisecond),
            elib_dt:rfc3339_to(TsNow, millisecond)
        )
    end).

pg_invalid_input_fails_without_fake_rows_test_() ->
    ?TEST_WITH_DB(fun() ->
        %% R19-4：非法 RFC3339 参数在编码阶段抛错 → 语句失败；坏值不得变成
        %% PG epoch 0（伪造 2000-01-01）混进数据库。epgsql 同步命令里 codec
        %% 异常在连接进程传播 → 调用方收到 {error, dead_connection}（语句
        %% 未执行、不新增伪造行；死连接由 pooler 淘汰重建）。
        Result =
            (catch elib_pg:query(
                <<"SELECT $1::timestamptz AS ts">>, [<<"not-a-timestamp">>]
            )),
        case Result of
            {error, dead_connection} ->
                %% 真实传播形状（epgsql 同步命令 + 池化连接）
                ok;
            {'EXIT', _} ->
                erlang:error({unexpected_exit_shape, Result});
            Other ->
                erlang:error({unexpected_success, Other})
        end,
        %% 池恢复：死连接淘汰后新查询正常（有限重试容忍替换延迟）
        pg_wait_pool_ready(10)
    end).

pg_wait_pool_ready(0) ->
    erlang:error(pool_not_recovered);
pg_wait_pool_ready(N) ->
    case elib_pg:query(<<"SELECT 1 AS one">>, []) of
        {ok, [#{<<"one">> := 1}]} ->
            ok;
        _ ->
            timer:sleep(200),
            pg_wait_pool_ready(N - 1)
    end.
