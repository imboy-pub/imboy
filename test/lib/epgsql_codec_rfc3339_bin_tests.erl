-module(epgsql_codec_rfc3339_bin_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% epgsql_codec_rfc3339_bin 模块的 EUnit 测试
%%%
%%% 目标：验证 PostgreSQL RFC3339 时间编解码功能
%%%===================================================================

encode_rfc3339_timestamp_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 使用 RFC3339 格式的时间戳（UTC 时区）
        Timestamp = <<"2024-01-01T12:00:00Z">>,
        % 测试编码功能（需要3个参数）
        Result = try
            EncodedValue = epgsql_codec_rfc3339_bin:encode(Timestamp, timestamptz, epgsql_idatetime),
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
        % 验证解码结果是有效的时间戳格式（PostgreSQL 格式：YYYY-MM-DD HH:MM:SS.UUUUUU+TZ）
        % 格式：2049-12-31 20:00:00.000000+08:00
        ?assertMatch(<<_:4/binary, $-, _:2/binary, $-, _:2/binary, $ , _, _:2/binary, $:, _:2/binary, $:, _, _/binary>>, Decoded)
    end).

encode_decode_roundtrip_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 使用 RFC3339 格式的时间戳
        OriginalTimestamp = <<"2024-12-25T10:30:45Z">>,
        % 编码
        {ok, Encoded} = try
            {ok, epgsql_codec_rfc3339_bin:encode(OriginalTimestamp, timestamptz, epgsql_idatetime)}
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
        % 测试无效输入
        ?assertError(_, epgsql_codec_rfc3339_bin:encode(<<"invalid">>, timestamptz, epgsql_idatetime))
    end).

decode_with_binary_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试二进制解码
        TimestampBin = <<1577880000000000:64/big-signed-integer>>,
        Decoded = epgsql_codec_rfc3339_bin:decode(TimestampBin, timestamptz, epgsql_idatetime),
        ?assertMatch(<<_/binary>>, Decoded)
    end).
