-module(msg_store_jsonb_roundtrip_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc msg_store_repo JSONB 密文往返保真回归测试
%%%
%%% 背景（真机实测 2026-07-07，commit 04d64ce4）：E2EE 裸密文以数字开头
%%% （如 "14bVk..."）被旧版 is_likely_json_binary 首字符启发式误判为 JSON
%%% 数字 → 原样写入 JSONB 列 → PG 22P02 invalid_text_representation →
%%% websocket_handler case_clause 崩溃 WS 进程。
%%%
%%% 现有 msg_store_repo_tests.erl 的 5 个回归测试全部经 meck 拦截
%%% elib_pg_sql:insert，只验证"传给 insert 的 Data 长什么样"，never 验证
%%% 该值对真实 PostgreSQL JSONB 类型推断是否合法——这正是 22P02 与
%%% msg_read_repo 42P08 两次真 bug 都绕过全量 meck 单测、只在真机/真库集成
%%% 才暴露的共同原因。
%%%
%%% 这里在纯函数层面补对抗性 fixture：不需要真库即可钉死"try-decode
%%% 而非首字符启发式"这一修复，且显式做 jsone:decode 往返断言——
%%% 对 E2EE 而言，密文哪怕一个字节被 JSON 转义规则改写，都等价于消息永久
%%% 不可解密（服务端不持有解密所需的密钥，无法事后修复）。
%%%===================================================================

%% ===================================================================
%% payload：裸密文 binary
%% ===================================================================

digit_leading_ciphertext_survives_and_roundtrips_test() ->
    %% 真机实测原始复现形状：数字开头 + 字母混合的 base64 密文
    Cipher = <<"14bVkXq8ZpQ2mR7sT9uWaBcDeFgHiJkLmNoPqRsTuVwXyZ0123456789AB+/==">>,
    Encoded = msg_store_repo:msg_store_payload_to_jsonb(Cipher),
    ?assert(is_binary(Encoded)),
    %% 必须是合法 JSON（对真 PG JSONB 列合法的必要条件）
    Decoded = jsone:decode(Encoded),
    %% 必须原样往返，一字节都不能变
    ?assertEqual(Cipher, Decoded).

pure_digit_ciphertext_gets_json_number_treatment_test() ->
    %% ponytail: 已知残留边界——若密文（极小概率）恰好全为数字字符，
    %% try-decode 会当作合法 JSON number 成功解析，返回值不加引号包装，
    %% 落库后以数字而非字符串形式存在 JSONB 列。base64 字母表 64 个符号里
    %% 仅 10 个是数字，定长随机密文全数字的概率约 (10/64)^N，N=44 时
    %% ~1e-36，工程上可忽略，故此测试只是钉住当前行为、不作为需要修复的
    %% bug。
    %% 上限：安全性来自「密文足够长且字母数字混排」，N=44 时 ~1e-36；N 变小时
    %%   概率按 (10/64)^N 快速上升（N=6 已到 ~1e-5）。
    %% 升级触发：密文编码改为纯数字体系（如某些 KDF 输出）、或密文/摘要长度缩短到
    %%   十几字符以内时，须改为无条件加引号包装，本测试同步改成断言包装后的形状。
    AllDigits = <<"123456789012345">>,
    Encoded = msg_store_repo:msg_store_payload_to_jsonb(AllDigits),
    ?assertEqual(AllDigits, Encoded).

json_literal_lookalike_ciphertext_pinned_test() ->
    %% ponytail: 另一个已知残留边界——若密文恰好整体等于 JSON 字面量
    %% true/false/null，try-decode 成功，函数按"合法 JSON 直接原样落库"
    %% 处理（不加引号包装）。这里不走 jsone:decode 语义比较（true 会解成
    %% atom，不是原字符串），而是直接比较 JSONB 列要落的原始字节——
    %% 这才是 unwrap_staging_payload 等下游消费者实际读取/操作的形式，
    %% 字节层面本就与输入一致，管线里从未对其做语义级 term 解码。
    %% 现实中 base64 密文几乎不可能整体等于这三个字面量，风险可忽略。
    %% 上限：安全性依赖两个前提——payload 不会是短枚举值，且下游只按字节消费
    %%   staging payload（一旦做语义级解码，true 会变成 atom 而非原字符串）。
    %% 升级触发：unwrap_staging_payload 等下游改为语义级 term 解码，或 payload
    %%   开始承载短枚举/布尔类值时，须改为无条件加引号包装。
    lists:foreach(
        fun(Literal) ->
            Encoded = msg_store_repo:msg_store_payload_to_jsonb(Literal),
            ?assertEqual(Literal, Encoded)
        end,
        [<<"true">>, <<"false">>, <<"null">>]
    ).

normal_json_object_payload_passes_through_unwrapped_test() ->
    %% 非 E2EE 普通消息：payload 本就是合法 JSON object，不应被二次包装
    Normal = <<"{\"text\":\"hello\"}">>,
    Encoded = msg_store_repo:msg_store_payload_to_jsonb(Normal),
    ?assertEqual(Normal, Encoded),
    ?assertMatch(#{<<"text">> := <<"hello">>}, jsone:decode(Encoded, [{object_format, map}])).

payload_map_encodes_as_json_object_test() ->
    Map = #{<<"text">> => <<"你好"/utf8>>},
    Encoded = msg_store_repo:msg_store_payload_to_jsonb(Map),
    ?assertEqual(Map, jsone:decode(Encoded, [{object_format, map}])).

%% ===================================================================
%% e2ee：null / 空 / map envelope / 裸 binary 密文（防御性分支）
%% ===================================================================

e2ee_null_stays_null_test() ->
    ?assertEqual(null, msg_store_repo:msg_store_e2ee_to_jsonb(null)).

e2ee_empty_binary_becomes_null_test() ->
    ?assertEqual(null, msg_store_repo:msg_store_e2ee_to_jsonb(<<>>)).

e2ee_map_envelope_roundtrips_test() ->
    %% 真实生产形态：E2EE 是 map（RSA-OAEP-256 + AES-256-GCM envelope）
    Envelope = #{
        <<"ciphertext">> => <<"lO/CCWRkQ0NXUmt2QUpvV1RPeU9OZz09">>,
        <<"iv">> => <<"MTIzNDU2Nzg5MDEy">>,
        <<"tag">> => <<"YWJjZGVmZ2hpams=">>,
        <<"alg">> => <<"AES-256-GCM">>
    },
    Encoded = msg_store_repo:msg_store_e2ee_to_jsonb(Envelope),
    ?assert(is_binary(Encoded)),
    ?assertEqual(Envelope, jsone:decode(Encoded, [{object_format, map}])).

e2ee_digit_leading_raw_binary_survives_and_roundtrips_test() ->
    %% 防御性分支（msg_store_e2ee_to_jsonb 的 is_binary 分支）：与 payload
    %% 同款 try-decode 真验证，即便当前生产调用方始终传 map|null，
    %% 该分支仍需对裸密文 binary 保真，防止未来调用方回归引入同类 bug。
    Cipher = <<"4QuejMbVkXq8ZpQ2mR7sT9uWaBcDeFgH">>,
    Encoded = msg_store_repo:msg_store_e2ee_to_jsonb(Cipher),
    ?assertEqual(Cipher, jsone:decode(Encoded)).
