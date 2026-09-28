%% @doc JSON 编解码替换契约测试：jsx -> jsone 迁移的等价性锚点。
%%%
%%% 背景：项目曾同时依赖 jsx 与 jsone 两个 JSON 库，2026-09 统一收敛到
%%% jsone（依赖清理）。本文件把迁移前用 jsx 实测的行为**固化为硬编码
%%% 期望值**（不依赖 jsx 模块——jsx 已从依赖中移除），锁定 jsone 必须
%%% 持续满足的三条契约：
%%%
%%%   1. decode 等价：`jsone:decode/1` 与旧 `jsx:decode(Bin, [return_maps])`
%%%      返回同一 Erlang term（binary 键 map、null/true/false atom、
%%%      integer/float、大整数无损）；非法 JSON（截断/尾部垃圾/空/非文本）
%%%      一律抛 `error:badarg`——与旧 jsx 相同，调用点的
%%%      `try ... catch Class:Reason` 模式无需调整。已知差异：重复键
%%%      jsone 取首值、jsx return_maps 取末值（畸形输入，无业务影响）。
%%%   2. encode 字节等价（native_utf8）：对 binary/atom/integer/proplist
%%%      输入，`jsone:encode(X, [native_utf8])` 与旧 `jsx:encode(X)` 输出
%%%      **字节相同**（含非 ASCII 原生 UTF-8、控制字符转义、undefined
%%%      atom 写成 "undefined"、TSID 大整数精确数字）。生产代码的 JSON
%%%      出口（SSE 帧、HTTP 响应、jsonb 落库、HMAC 签名原文）因此零变化。
%%%   3. float 边界（已知不等价）：float 的文本表示两家不同——jsx 用
%%%      最短表示（3.14），jsone 用 21 位有效数字科学计数法
%%%      （3.14000000000000012434e+00）。JSON **数值语义等价**（解析回的
%%%      double 相同），但字节不同。影响面：
%%%        * HMAC 签名原文（cs_identity_assertion:canonical_claims/1 的
%%%          claims 白名单 iss/aud/widget_id/sub/exp/iat/jti 均为 binary
%%%          + integer 值域，无 float 语义，不受影响）；
%%%        * 对外响应若含 float 字段，字节变化但 JSON 数值等价。
%%%      本文件用 float_value_boundary_test 显式锁定该差异，防止未来
%%%      有人误以为可以随手去掉 [native_utf8] 或声称字节恒等。
%%%
%%% 生产替换规则（与各调用点一一对应）：
%%%   `jsx:decode(Bin, [return_maps])` -> `jsone:decode(Bin)`
%%%   `jsx:encode(X)`                  -> `jsone:encode(X, [native_utf8])`
-module(json_codec_equivalence_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% 1. decode 等价
%%%-------------------------------------------------------------------

decode_returns_binary_key_map_test() ->
    ?assertEqual(
        #{<<"a">> => 1, <<"b">> => <<"x">>},
        jsone:decode(<<"{\"a\":1,\"b\":\"x\"}">>)
    ).

decode_nested_structures_test() ->
    Bin = <<
        "{\"a\":1,\"b\":[true,false,null],\"c\":\"caf\xc3\xa9\","
        "\"d\":3.14,\"e\":1e10,\"nested\":{\"x\":[{\"y\":2}]}}"
    >>,
    ?assertEqual(
        #{
            <<"a">> => 1,
            <<"b">> => [true, false, null],
            <<"c">> => <<"caf\xc3\xa9">>,
            <<"d">> => 3.14,
            <<"e">> => 1.0e10,
            <<"nested">> => #{<<"x">> => [#{<<"y">> => 2}]}
        },
        jsone:decode(Bin)
    ).

%% TSID 是 64-bit integer，解码必须无损（2^53+1 不能变 float）。
decode_big_integer_lossless_test() ->
    ?assertEqual(
        #{<<"big">> => 9007199254740993},
        jsone:decode(<<"{\"big\":9007199254740993}">>)
    ).

decode_unicode_escape_test() ->
    ?assertEqual(
        #{<<"s">> => <<"caf\xc3\xa9">>},
        jsone:decode(<<"{\"s\":\"caf\\u00e9\"}">>)
    ).

%% 重复键语义（与旧 jsx 的已知差异）：jsone 保留**第一个**键值
%% （jsx return_maps 是 put 覆盖、最后一个赢）。重复键属畸形输入（正常
%% 客户端不产生）；decode 调用点均为请求体/DB JSON，取首值无业务影响。
decode_duplicate_key_first_wins_test() ->
    ?assertEqual(
        #{<<"k">> => 1},
        jsone:decode(<<"{\"k\":1,\"k\":2}">>)
    ).

%% 非法输入行为契约：一律 error:badarg（旧 jsx 同款），调用点
%% try/catch Class:Reason 模式保持有效。
decode_invalid_json_badarg_test() ->
    [
        ?assertException(error, badarg, jsone:decode(<<"{bad">>)),
        ?assertException(error, badarg, jsone:decode(<<"{} x">>)),
        ?assertException(error, badarg, jsone:decode(<<"">>)),
        ?assertException(error, badarg, jsone:decode(<<0, 1, 2>>)),
        ?assertException(error, badarg, jsone:decode(<<"{\"a\":}">>))
    ].

%%%-------------------------------------------------------------------
%%% 2. encode 字节等价（native_utf8）——硬编码 jsx 历史输出金丝雀
%%%-------------------------------------------------------------------

%% cs_identity_assertion:canonical_claims/1 的精确形态：binary 键、
%% 按字典序排序的 {K, V} 两元组列表（jsx/jsone 均编码为 JSON 对象）。
%% 期望字节串为迁移前 jsx:encode(lists:sort(Pairs)) 的实测输出——
%% 换库后 HMAC 签名原文字节不变，存量 widget 断言继续验签通过。
canonical_proplist_bytes_test() ->
    Pairs = [
        {<<"exp">>, 1893456000},
        {<<"sub">>, <<"tk-cs-abc123">>},
        {<<"uid">>, 12345678901234567},
        {<<"wid">>, <<"widget_cn_\u6c49\u5b57">>}
    ],
    ?assertEqual(
        <<
            "{\"exp\":1893456000,\"sub\":\"tk-cs-abc123\","
            "\"uid\":12345678901234567,\"wid\":\"widget_cn_\u6c49\u5b57\"}"/utf8
        >>,
        jsone:encode(lists:sort(Pairs), [native_utf8])
    ).

%% 字典序无关性：sort 后同键值恒得同字节（canonical 的核心）。
canonical_is_deterministic_test() ->
    PairsA = [{<<"b">>, 1}, {<<"a">>, <<"x">>}],
    PairsB = [{<<"a">>, <<"x">>}, {<<"b">>, 1}],
    ?assertEqual(
        jsone:encode(lists:sort(PairsA), [native_utf8]),
        jsone:encode(lists:sort(PairsB), [native_utf8])
    ).

%% 通用 map 编码：UTF-8 原生输出（jsx 同款，非 \uXXXX 转义）。
encode_map_native_utf8_bytes_test() ->
    ?assertEqual(
        <<"{\"k\":\"caf\xc3\xa9\"}">>,
        jsone:encode(#{<<"k">> => <<"caf\xc3\xa9">>}, [native_utf8])
    ).

%% atom 值：true/false/null 出 JSON 关键字；其余 atom（undefined）出
%% 字符串（cs_http:encode_entity 依赖此行为把 undefined 显式化）。
encode_atoms_test() ->
    ?assertEqual(
        <<"[true,false,null,\"undefined\"]">>,
        jsone:encode([true, false, null, undefined], [native_utf8])
    ).

%% 控制字符与引号转义与 jsx 相同形态。
encode_escapes_test() ->
    ?assertEqual(
        <<"{\"n\":\"a\\nb\\\"c\\\\d\"}">>,
        jsone:encode(#{<<"n">> => <<"a\nb\"c\\d">>}, [native_utf8])
    ).

%% atom 键、空数组、两元组列表对象形态。
encode_shapes_test() ->
    ?assertEqual(<<"{\"k\":1}">>, jsone:encode(#{k => 1}, [native_utf8])),
    ?assertEqual(<<"[]">>, jsone:encode([], [native_utf8])),
    ?assertEqual(<<"{\"k\":\"v\"}">>, jsone:encode([{<<"k">>, <<"v">>}], [native_utf8])).

%% TSID 大整数精确数字（非科学计数法）——对前端 safeParseBigIntJson 关键。
encode_big_integer_exact_digits_test() ->
    ?assertEqual(
        <<"{\"id\":12345678901234567}">>,
        jsone:encode(#{<<"id">> => 12345678901234567}, [native_utf8])
    ).

%%%-------------------------------------------------------------------
%%% 3. float 边界（已知差异，显式锁定）
%%%-------------------------------------------------------------------

float_value_boundary_test() ->
    %% JSON 数值语义等价：解析回同一 double。
    ?assertEqual(
        3.14,
        maps:get(<<"f">>, jsone:decode(jsone:encode(#{<<"f">> => 3.14}, [native_utf8])))
    ),
    %% 但文本字节与最短表示不同（jsx 旧输出为 {"f":3.14}）——HMAC 签名
    %% 原文勿放 float；claims 值域已审计为 binary+integer。
    ?assertNotEqual(<<"{\"f\":3.14}">>, jsone:encode(#{<<"f">> => 3.14}, [native_utf8])).

%%%-------------------------------------------------------------------
%%% 4. 往返
%%%-------------------------------------------------------------------

roundtrip_test() ->
    Term = #{
        <<"int">> => 42,
        <<"bin">> => <<"值"/utf8>>,
        <<"list">> => [1, <<"a">>, null, true],
        <<"map">> => #{<<"deep">> => #{<<"x">> => 7}}
    },
    Bin = jsone:encode(Term, [native_utf8]),
    ?assertEqual(Term, jsone:decode(Bin)).
