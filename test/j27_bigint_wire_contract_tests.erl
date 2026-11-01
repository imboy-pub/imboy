-module(j27_bigint_wire_contract_tests).

%%% J27.d / J27.e（plan §9，行 678-679）Backend 段：JSON wire 大整数
%%% 解码/回传合同。
%%%
%%% J27.d：2^53−1 / 2^53 / 2^53+1 / BIGINT 上界合成原始文本，Backend
%%%   解码→（信封出口）→回传，所有 ID 十进制逐字符不变。
%%%   本文件锁定的事实：
%%%     - jsone:decode 整数字面量经 binary_to_integer/1（任意精度
%%%       bignum），不经 double/float 中转——2^53 与 2^53+1 解码后
%%%       可区分（经 float 中转二者必相等，见 not_via_float 用例）。
%%%     - jsone:encode 整数经 integer_to_binary/1，回传文本十进制
%%%       逐字符一致。
%%%     - 信封出口 message_ds:envelope_id_to_binary/1 把 integer ID
%%%       转为十进制 binary（与 message_envelope_id_contract_tests
%%%       同口径，此处以 BIGINT 上界互证链路两端）。
%%%
%%% J27.e：非 ID 数值（时间/金额/小数）、长数字字符串、Xid 经同一
%%%   解析/回传链保持原义；非法 ID（float 形态）不被伪造为 string ID。
%%%
%%% 运行前置（本批次红线：只写不执行；执行由后续验收轨道承载）：
%%%   IMBOYENV=local make eunit t=j27_bigint_wire_contract_tests
%%% 模块名无 src 同名，erlang.mk 自动发现；单跑直通不受全量过滤影响。
%%%
%%% 注：整数字面量一律以 binary 合成（模拟 wire 原始字节），源代码中
%%% 不出现会丢精度的第三方中间层；Erlang integer 字面量本身任意精度
%%% 无损，与 binary_to_integer 结果以 =:= 互证。

-include_lib("eunit/include/eunit.hrl").

%% J27.d 的四个边界 + BIGINT 下界，wire 十进制文本（raw source of truth）。

%% 2^53 - 1
-define(ID_SAFE_53_M1, <<"9007199254740991">>).
%% 2^53
-define(ID_53, <<"9007199254740992">>).
%% 2^53 + 1
-define(ID_53_P1, <<"9007199254740993">>).
%% 2^63 - 1（PG BIGINT 上界）
-define(ID_INT64_MAX, <<"9223372036854775807">>).
%% 2^63（PG BIGINT 下界）
-define(ID_INT64_MIN, <<"-9223372036854775808">>).
%% 2^63（超 BIGINT，JSON 仍合法）
-define(ID_BEYOND_64, <<"9223372036854775808">>).

%% ===================================================================
%% J27.d：decode 精确性
%% ===================================================================

j27d_decode_boundaries_exact_integer_test() ->
    lists:foreach(
        fun(WireDigits0) ->
            {WireDigits, ExpectNeg} =
                case WireDigits0 of
                    <<"-", Rest/binary>> -> {Rest, true};
                    _ -> {WireDigits0, false}
                end,
            Expected0 = binary_to_integer(WireDigits),
            Expected =
                case ExpectNeg of
                    true -> -Expected0;
                    false -> Expected0
                end,
            Decoded = jsone:decode(<<"{\"id\":", WireDigits0/binary, "}">>),
            Id = maps:get(<<"id">>, Decoded),
            %% =:= 互证：decode 值与 Erlang 任意精度整数完全同值同型
            ?assert(is_integer(Id)),
            ?assertEqual(Expected, Id)
        end,
        [
            ?ID_SAFE_53_M1,
            ?ID_53,
            ?ID_53_P1,
            ?ID_INT64_MAX,
            ?ID_INT64_MIN,
            ?ID_BEYOND_64
        ]
    ).

j27d_decode_not_via_float_test() ->
    %% 若任何环节经 double 中转，2^53 与 2^53+1 会坍缩为同一 float；
    %% 精确 integer 解码必须保持二者可区分（==/=:= 均）。
    A = maps:get(<<"id">>, jsone:decode(<<"{\"id\":", ?ID_53/binary, "}">>)),
    B = maps:get(<<"id">>, jsone:decode(<<"{\"id\":", ?ID_53_P1/binary, "}">>)),
    ?assert(is_integer(A)),
    ?assert(is_integer(B)),
    ?assertNot(A == B),
    ?assertNot(A =:= B).

j27d_decode_beyond_bigint_is_valid_json_bignum_test() ->
    %% 2^63 已超 PG BIGINT 上界：DB 层会拒绝，但 JSON wire 合法，
    %% decode 不失真；这证明越界值不是在解码层"悄悄截断"，
    %% 而是可被上层以精确值识别后显式拒绝（J27.e 拒绝语义的前提）。
    Decoded = jsone:decode(<<"{\"id\":", ?ID_BEYOND_64/binary, "}">>),
    Id = maps:get(<<"id">>, Decoded),
    ?assertEqual(binary_to_integer(?ID_BEYOND_64), Id),
    ?assert(Id > 9223372036854775807).

%% ===================================================================
%% J27.d：decode→encode 回传逐字符一致
%% ===================================================================

j27d_roundtrip_decimal_identical_test() ->
    lists:foreach(
        fun(WireDigits) ->
            Wire = <<"{\"id\":", WireDigits/binary, "}">>,
            Round = jsone:encode(jsone:decode(Wire)),
            ?assertEqual(Wire, Round),
            %% 拆开验证 id 段本身逐字符一致（含负号）
            ?assertEqual(
                WireDigits,
                integer_to_binary(
                    maps:get(<<"id">>, jsone:decode(Wire))
                )
            )
        end,
        [
            ?ID_SAFE_53_M1,
            ?ID_53,
            ?ID_53_P1,
            ?ID_INT64_MAX,
            ?ID_INT64_MIN,
            ?ID_BEYOND_64
        ]
    ).

j27d_roundtrip_envelope_shape_test() ->
    %% 常见信封形状（payload 嵌套 + 列表 + 混合字段）往返：数值 ID
    %% 十进制逐字符原样出现。注意 jsone decode 到 Erlang map 后键序
    %% 不保证（maps 无序），整文本逐字符相等不是合同；合同是
    %% 「值等价 + 每个 ID 数值 token 原样回传」（jsone 实测行为：
    %% 仅有键序重排，无任何数值/文本变化）。
    Wire = <<
        "{\"code\":0,\"msg\":\"ok\",\"payload\":{\"list\":["
        "{\"id\":",
        ?ID_53_P1/binary,
        ",\"creator_uid\":",
        ?ID_INT64_MAX/binary,
        ",\"name\":\"n\"}],"
        "\"next_cursor\":null}}"
    >>,
    Round = jsone:encode(jsone:decode(Wire)),
    %% 值等价：decode(Round) 与 decode(Wire) 同一 map
    ?assertEqual(jsone:decode(Wire), jsone:decode(Round)),
    %% 大整数 ID token 原样出现在回传文本中（十进制逐字符）
    ?assertMatch({_, _}, binary:match(Round, ?ID_53_P1)),
    ?assertMatch({_, _}, binary:match(Round, ?ID_INT64_MAX)),
    %% 且仍是 JSON number 位置（不被引号包裹成 string——信封出口
    %% 的 string 化由 message_ds:envelope_id_to_binary 显式完成）
    QuotedId = <<"\"", ?ID_53_P1/binary, "\"">>,
    ?assert(nomatch =:= binary:match(Round, QuotedId)).

%% ===================================================================
%% J27.d：信封出口（Backend→App 端）
%% ===================================================================

j27d_envelope_exit_integer_to_decimal_binary_test() ->
    %% integer ID 出信封必须转十进制 binary（JS 客户端按 string 接收），
    %% BIGINT 上界量级同精确。
    lists:foreach(
        fun(WireDigits) ->
            Int = binary_to_integer(WireDigits),
            ?assertEqual(
                WireDigits,
                message_ds:envelope_id_to_binary(Int)
            )
        end,
        [?ID_SAFE_53_M1, ?ID_53_P1, ?ID_INT64_MAX, ?ID_BEYOND_64]
    ).

j27d_envelope_exit_binary_id_passthrough_test() ->
    %% 已是 binary 的 ID（DB 以 text 出或上游已归一）原样透传，
    %% 不做二次格式化（前导零等原始形态保留——但 TSID 本身无前导零）。
    ?assertEqual(
        <<"9223372036854775807">>,
        message_ds:envelope_id_to_binary(<<"9223372036854775807">>)
    ),
    ?assertEqual(<<"42">>, message_ds:envelope_id_to_binary(<<"42">>)).

%% ===================================================================
%% J27.e：非 ID 数值/字符串同一解析回传链保持原义
%% ===================================================================

j27e_non_id_values_keep_meaning_test() ->
    Wire = <<
        "{\"created_at\":1761868800000,"
        "\"updated_at_us\":1785000000000000,"
        "\"amount_fen\":999999999999999,"
        "\"price\":1299,"
        "\"ratio\":0.6180339887498949,"
        "\"text\":\"00092233720368547758071234567890\","
        "\"remark\":\"audit 9223372036854775807, done\","
        "\"xid\":\"imboy_7h3k9x2a1b\"}"
    >>,
    Decoded = jsone:decode(Wire),
    %% 时间（13 位毫秒 / 16 位微秒）与金额（15 位分）保持精确 integer，
    %% 不被 float 化（16 位微秒已越过部分 float 精度边界）
    ?assertEqual(1761868800000, maps:get(<<"created_at">>, Decoded)),
    ?assert(is_integer(maps:get(<<"updated_at_us">>, Decoded))),
    ?assertEqual(1785000000000000, maps:get(<<"updated_at_us">>, Decoded)),
    ?assertEqual(999999999999999, maps:get(<<"amount_fen">>, Decoded)),
    ?assertEqual(1299, maps:get(<<"price">>, Decoded)),
    %% 小数 decode 为 float，值在双精度语义内原义（不做逐字符断言，
    %% jsone 浮点回传格式化不保证文本一致，原义以值相等口径）
    Ratio = maps:get(<<"ratio">>, Decoded),
    ?assert(is_float(Ratio)),
    ?assertEqual(0.6180339887498949, Ratio),
    %% 长数字字符串（>19 位，超 BIGINT 表达域）与字符串内嵌数字：
    %% binary 原样，结构区判据不误伤。文本用 ASCII 规避源文件编码
    %% 脆弱性（CJK 二进制分段是另一验收面的教训，不混入本合同）。
    ?assertEqual(
        <<"00092233720368547758071234567890">>,
        maps:get(<<"text">>, Decoded)
    ),
    ?assertEqual(
        <<"audit 9223372036854775807, done">>,
        maps:get(<<"remark">>, Decoded)
    ),
    %% Xid（字母数字混合）原样
    ?assertEqual(<<"imboy_7h3k9x2a1b">>, maps:get(<<"xid">>, Decoded)),
    %% 回传链：非 ID 字段往返不破坏（null/嵌套不在此列，见 roundtrip 用例）
    Round = jsone:encode(Decoded),
    ReDecoded = jsone:decode(Round),
    ?assertEqual(
        maps:get(<<"text">>, Decoded),
        maps:get(<<"text">>, ReDecoded)
    ),
    ?assertEqual(
        maps:get(<<"remark">>, Decoded),
        maps:get(<<"remark">>, ReDecoded)
    ),
    ?assertEqual(
        maps:get(<<"xid">>, Decoded),
        maps:get(<<"xid">>, ReDecoded)
    ),
    ?assertEqual(
        maps:get(<<"amount_fen">>, Decoded),
        maps:get(<<"amount_fen">>, ReDecoded)
    ).

%% ===================================================================
%% J27.e：非法 ID 明确不伪造
%% ===================================================================

j27e_float_id_not_forged_as_string_id_test() ->
    %% wire 非法 ID（float 形态）decode 为 float，不被解码层静默转成
    %% integer/binary ID——上层 ID 校验拿到的是可明确识别的非法形态。
    %% 信封出口 message_ds:envelope_id_to_binary/1 的第二子句对非
    %% integer 原样透传（float 属 spec 外值，不直调生产模块——dialyzer
    %% 基线纪律），即出口不会把 1.5 伪造为 <<"1.5">> 冒充 string ID；
    %% 拒绝本身由 handler/logic 层按合同承担（App 侧
    %% invalid_resource_id_contract_test 已验同一合同）。
    Decoded = jsone:decode(<<"{\"id\":1.5}">>),
    Id = maps:get(<<"id">>, Decoded),
    ?assert(is_float(Id)),
    ?assertEqual(1.5, Id),
    ?assertNot(is_binary(Id)),
    ?assertNot(is_integer(Id)).

j27e_negative_id_survives_roundtrip_test() ->
    %% 负数 ID 合法域（BIGINT 下界）逐字符一致；前导零整数字面量
    %% 是非法 JSON（01），decode 必抛 badarg——明确拒绝而非静默归一。
    Wire = <<"{\"id\":", ?ID_INT64_MIN/binary, "}">>,
    ?assertEqual(Wire, jsone:encode(jsone:decode(Wire))),
    ?assertException(error, badarg, jsone:decode(<<"{\"id\":01}">>)).

j27e_non_decimal_id_shape_rejected_at_decode_test() ->
    %% true/false/null 形态出现在 ID 位置：decode 得到对应 atom/null，
    %% 不是 integer 也不是十进制 binary——上层 ID 校验可明确识别并拒绝，
    %% 不会被解码层静默转换成数字。
    ?assertEqual(true, maps:get(<<"id">>, jsone:decode(<<"{\"id\":true}">>))),
    ?assertEqual(null, maps:get(<<"id">>, jsone:decode(<<"{\"id\":null}">>))),
    ?assert(is_atom(maps:get(<<"id">>, jsone:decode(<<"{\"id\":false}">>)))),
    %% 科学计数法 ID 位置：decode 为 float，同样不伪装 integer ID
    Scientific = maps:get(<<"id">>, jsone:decode(<<"{\"id\":9.2233720368547758e18}">>)),
    ?assert(is_float(Scientific)).
