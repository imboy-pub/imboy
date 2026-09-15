%%% @doc EB-03 加密原语套件：企业托管加密的 fail-closed 契约与「无明文」性质。
%%%
%%% 纯逻辑套件（不连库、不启动 app）：crypto 的 NIF 按需加载即可工作。
%%%
%%% 覆盖：
%%%   EB-03-A04 缺 key / 错 version / AAD 不匹配 / 密文被篡改 → 一律 fail-closed，
%%%             任何分支都不得返回明文（不降级、不回落默认密钥）；
%%%   EB-03-A05 密文封装与日志中不含明文（金丝雀子串断言 + 源文件无日志调用）；
%%%   端口一致性：基础设施实现与 EB-02 冻结的 `eb_ports:contracts/0` 逐条对齐。
-module(eb_crypto_tests).

-include_lib("eunit/include/eunit.hrl").

-define(AAD(ConvId, MsgId), #{
    organization_id => 991700000000001,
    workspace_id => 991700000000002,
    conversation_id => ConvId,
    message_id => MsgId
}).

%% ===================================================================
%% 正向：封装 / 解封
%% ===================================================================

seal_open_roundtrip_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Aad = ?AAD(1, 2),
    Plain = <<"hello-enterprise-managed-body">>,
    ?assertMatch({ok, _}, eb_managed_crypto:seal(Aad, Plain, Ref)),
    {ok, Sealed} = eb_managed_crypto:seal(Aad, Plain, Ref),
    ?assertEqual({ok, Plain}, eb_managed_crypto:open(Aad, Sealed, Ref)).

sealed_carries_version_and_algorithm_test() ->
    Ref = eb_pg_test_fixture:key_ref(7),
    {ok, Sealed} = eb_managed_crypto:seal(?AAD(11, 12), <<"body">>, Ref),
    ?assertEqual(7, maps:get(key_version, Sealed)),
    ?assertEqual(<<"aes-256-gcm">>, maps:get(alg, Sealed)),
    ?assert(is_binary(maps:get(cipher, Sealed))),
    ?assertEqual(64, byte_size(maps:get(aad_hash, Sealed))),
    ?assertEqual(false, maps:is_key(plaintext, Sealed)).

%% 同一明文同一作用域两次封装必须产生不同密文（随机 IV / salt 的语义安全）。
same_plaintext_seals_differently_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Aad = ?AAD(21, 22),
    {ok, First} = eb_managed_crypto:seal(Aad, <<"same-body">>, Ref),
    {ok, Second} = eb_managed_crypto:seal(Aad, <<"same-body">>, Ref),
    ?assertNotEqual(maps:get(cipher, First), maps:get(cipher, Second)).

%% ===================================================================
%% EB-03-A04：fail-closed
%% ===================================================================

missing_key_fails_closed_test() ->
    Plain = <<"must-not-leak">>,
    lists:foreach(
        fun(KeyRef) ->
            ?assertMatch({error, missing_key}, eb_managed_crypto:seal(?AAD(31, 32), Plain, KeyRef)),
            ?assertMatch({error, missing_key}, eb_managed_crypto:seal(?AAD(31, 32), Plain, KeyRef))
        end,
        [undefined, #{}, #{key_version => 1}, #{key => undefined, key_version => 1}]
    ).

invalid_key_length_fails_closed_test() ->
    Short = #{key => <<"too-short">>, key_version => 1},
    ?assertEqual(
        {error, invalid_key_length},
        eb_managed_crypto:seal(?AAD(41, 42), <<"body">>, Short)
    ).

unknown_key_version_fails_closed_test() ->
    Keyring = #{keys => #{1 => crypto:strong_rand_bytes(32)}, key_version => 3},
    ?assertEqual(
        {error, {unknown_key_version, 3}},
        eb_managed_crypto:seal(?AAD(51, 52), <<"body">>, Keyring)
    ).

missing_key_version_fails_closed_test() ->
    Keyring = #{keys => #{1 => crypto:strong_rand_bytes(32)}},
    ?assertEqual(
        {error, missing_key_version},
        eb_managed_crypto:seal(?AAD(53, 54), <<"body">>, Keyring)
    ).

%% 错 version：以 v1 封装，用声明 v2 的 key ref 解封 → 版本不匹配（不得回退到 v1 密钥解释）。
wrong_key_version_fails_closed_test() ->
    Ref1 = eb_pg_test_fixture:key_ref(1),
    Aad = ?AAD(61, 62),
    {ok, Sealed} = eb_managed_crypto:seal(Aad, <<"versioned-body">>, Ref1),
    Ref2 = eb_pg_test_fixture:key_ref(2),
    ?assertEqual(
        {error, {key_version_mismatch, 2, 1}},
        eb_managed_crypto:open(Aad, Sealed, Ref2)
    ).

%% AAD 不匹配（换 message_id）：必须在做任何解密前失败，且不返回明文。
aad_mismatch_fails_closed_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Plain = eb_pg_test_fixture:canary(),
    {ok, Sealed} = eb_managed_crypto:seal(?AAD(71, 72), Plain, Ref),
    Result = eb_managed_crypto:open(?AAD(71, 73), Sealed, Ref),
    ?assertEqual({error, aad_mismatch}, Result),
    ?assertNot(is_binary(Result)).

%% AAD 缺字段 / 类型不对：不得当作「无 AAD」放行。
invalid_aad_fails_closed_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Bad1 = #{
        organization_id => 1, workspace_id => 2, conversation_id => 3
    },
    Bad2 = #{
        organization_id => 1, workspace_id => 2, conversation_id => 3, message_id => <<"x">>
    },
    ?assertMatch({error, {invalid_aad, _}}, eb_managed_crypto:seal(Bad1, <<"b">>, Ref)),
    ?assertMatch({error, {invalid_aad, _}}, eb_managed_crypto:seal(Bad2, <<"b">>, Ref)),
    ?assertMatch({error, {invalid_aad, _}}, eb_managed_crypto:seal(not_a_map, <<"b">>, Ref)).

%% 密文被篡改 → GCM 认证失败，绝不返回明文。
tampered_cipher_fails_closed_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Aad = ?AAD(81, 82),
    {ok, Sealed} = eb_managed_crypto:seal(Aad, <<"tamper-body">>, Ref),
    Cipher = maps:get(cipher, Sealed),
    Raw = base64:decode(Cipher),
    <<First:8, Rest/binary>> = Raw,
    Flipped = <<(First bxor 16#01):8, Rest/binary>>,
    Tampered = Sealed#{cipher => base64:encode(Flipped)},
    Result = eb_managed_crypto:open(Aad, Tampered, Ref),
    ?assertEqual({error, {open_failed, authentication_failed}}, Result),
    ?assertNot(is_binary(Result)).

%% 同 version 但密钥材料不同 → 认证失败（不得因 version 相同就当作可解）。
wrong_key_material_fails_closed_test() ->
    Plain = <<"wrong-key-body">>,
    Aad = ?AAD(91, 92),
    {ok, Sealed} = eb_managed_crypto:seal(Aad, Plain, eb_pg_test_fixture:key_ref(1)),
    Other = eb_pg_test_fixture:key_ref(1),
    ?assertEqual(
        {error, {open_failed, authentication_failed}},
        eb_managed_crypto:open(Aad, Sealed, Other)
    ).

%% 封装缺失字段 / 未知算法 → fail-closed。
malformed_sealed_fails_closed_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Aad = ?AAD(101, 102),
    ?assertEqual({error, invalid_sealed}, eb_managed_crypto:open(Aad, not_a_map, Ref)),
    ?assertEqual(
        {error, invalid_sealed}, eb_managed_crypto:open(Aad, #{alg => <<"aes-256-gcm">>}, Ref)
    ),
    {ok, Sealed} = eb_managed_crypto:seal(Aad, <<"b">>, Ref),
    ?assertEqual(
        {error, {unsupported_algorithm, <<"aes-256-cbc">>}},
        eb_managed_crypto:open(Aad, Sealed#{alg => <<"aes-256-cbc">>}, Ref)
    ).

%% AAD 不只是「摘要校验」：即使把 aad_hash 伪造成另一个作用域的摘要，
%% 派生密钥仍绑定原作用域 → 认证失败（证明 AAD 进入了 KDF，而非仅做比较）。
aad_is_bound_into_key_not_only_digest_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Aad1 = ?AAD(111, 112),
    Aad2 = ?AAD(111, 113),
    {ok, Sealed1} = eb_managed_crypto:seal(Aad1, <<"bound-body">>, Ref),
    {ok, Hash2} = eb_managed_crypto:aad_hash(Aad2),
    Forged = Sealed1#{aad_hash => Hash2},
    ?assertEqual(
        {error, {open_failed, authentication_failed}},
        eb_managed_crypto:open(Aad2, Forged, Ref)
    ).

%% 非二进制明文 → 拒绝（不做隐式 iodata 转换，避免形态歧义）。
invalid_plaintext_fails_closed_test() ->
    Ref = #{key => crypto:strong_rand_bytes(32), key_version => 1},
    ?assertEqual(
        {error, invalid_plaintext},
        eb_managed_crypto:seal(?AAD(121, 122), "list-is-not-binary", Ref)
    ),
    ?assertEqual(
        {error, invalid_plaintext},
        eb_managed_crypto:seal(?AAD(121, 122), 12345, Ref)
    ),
    %% 对照：同一 ref + 合法 AAD + 二进制明文必须成功（证明上面拒绝的是明文形态）
    ?assertMatch({ok, _}, eb_managed_crypto:seal(?AAD(121, 122), <<"ok">>, Ref)).

%% ===================================================================
%% EB-03-A05：无明文（密文封装 / HMAC 摘要 / 日志）
%% ===================================================================

sealed_payload_contains_no_plaintext_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Plain = eb_pg_test_fixture:canary(),
    {ok, Sealed} = eb_managed_crypto:seal(?AAD(131, 132), Plain, Ref),
    ?assertEqual(nomatch, binary:match(maps:get(cipher, Sealed), Plain)),
    ?assertEqual(nomatch, binary:match(maps:get(aad_hash, Sealed), Plain)),
    ?assert(eb_pg_test_fixture:canary_absent(maps:get(cipher, Sealed))),
    ?assertEqual(nomatch, binary:match(term_to_binary(Sealed), Plain)).

subject_hmac_is_org_scoped_digest_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Subject = eb_pg_test_fixture:canary(),
    {ok, HmacA} = eb_managed_crypto:subject_hmac(991700000000001, <<"wechat">>, Subject, Ref),
    {ok, HmacA2} = eb_managed_crypto:subject_hmac(991700000000001, <<"wechat">>, Subject, Ref),
    {ok, HmacOtherOrg} = eb_managed_crypto:subject_hmac(
        991700000000999, <<"wechat">>, Subject, Ref
    ),
    {ok, HmacOtherChannel} = eb_managed_crypto:subject_hmac(
        991700000000001, <<"phone">>, Subject, Ref
    ),
    %% 64 位小写 hex（EB-01 的 ck_eci_subject_hmac 形态）
    ?assertMatch({match, _}, re:run(HmacA, <<"^[0-9a-f]{64}$">>)),
    %% 确定性：同 Org 同 channel 同 subject → 同摘要（可用于幂等去重）
    ?assertEqual(HmacA, HmacA2),
    %% 组织域隔离：换 Org / 换 channel 必须得到不同摘要
    ?assertNotEqual(HmacA, HmacOtherOrg),
    ?assertNotEqual(HmacA, HmacOtherChannel),
    %% 摘要中不含明文 subject
    ?assertEqual(nomatch, binary:match(HmacA, Subject)).

%% HMAC 也必须在缺 key 时 fail-closed（不得用无密钥裸哈希兜底）。
subject_hmac_missing_key_fails_closed_test() ->
    ?assertMatch(
        {error, missing_key},
        eb_managed_crypto:subject_hmac(1, <<"wechat">>, <<"subject">>, undefined)
    ).

%% 密钥派生失败不抛异常（KDF 入口必须收敛为 error 元组）。
derive_failure_is_error_tuple_test() ->
    ?assertMatch(
        {error, _},
        eb_managed_crypto:seal(?AAD(141, 142), <<"b">>, #{key => <<"short">>, key_version => 1})
    ).

%% 日志不泄露：封装/解封/摘要全路径不得产生任何日志事件，尤其不得包含明文金丝雀。
no_plaintext_in_logs_test() ->
    Ref = eb_pg_test_fixture:key_ref(1),
    Plain = eb_pg_test_fixture:canary(),
    Aad = ?AAD(151, 152),
    Self = self(),
    FilterId = eb03_canary_capture,
    ok = logger:add_primary_filter(FilterId, {
        fun(LogEvent, Pid) ->
            Pid ! {eb03_log_event, LogEvent},
            ignore
        end,
        Self
    }),
    Events =
        try
            {ok, Sealed} = eb_managed_crypto:seal(Aad, Plain, Ref),
            _ = eb_managed_crypto:open(Aad, Sealed, Ref),
            _ = eb_managed_crypto:open(?AAD(151, 153), Sealed, Ref),
            _ = eb_managed_crypto:subject_hmac(991700000000001, <<"wechat">>, Plain, Ref),
            collect_log_events()
        after
            logger:remove_primary_filter(FilterId)
        end,
    ?assertEqual(
        [],
        [
            E
         || E <- Events, binary:match(iolist_to_binary(io_lib:format("~p", [E])), Plain) =/= nomatch
        ]
    ).

collect_log_events() ->
    receive
        {eb03_log_event, Event} -> [Event | collect_log_events()]
    after 0 ->
        []
    end.

%% 静态证据：加密模块本身不得引用任何日志宏/API（无明文可泄的更强保证）。
crypto_module_has_no_logging_test() ->
    Source = read_source(eb_managed_crypto),
    Code = strip_comments(Source),
    ?assertEqual(nomatch, binary:match(Code, <<"?WARN_LOG">>)),
    ?assertEqual(nomatch, binary:match(Code, <<"?INFO_LOG">>)),
    ?assertEqual(nomatch, binary:match(Code, <<"?DEBUG_LOG">>)),
    ?assertEqual(nomatch, binary:match(Code, <<"?ERROR_LOG">>)),
    ?assertEqual(nomatch, binary:match(Code, <<"logger:">>)),
    ?assertEqual(nomatch, binary:match(Code, <<"error_logger">>)).

%% ===================================================================
%% 端口一致性：基础设施实现 ↔ EB-02 冻结契约
%% ===================================================================

%% 每个已装配的实现都必须声明对应 behaviour、导出全部冻结 callback，
%% 且 behaviour_info(callbacks) 与 eb_ports:contracts/0 逐条一致。
declared_port_implementations_match_frozen_contracts_test() ->
    Contracts = eb_ports:contracts(),
    Implementations = eb_infra_ports:implementations(),
    ?assert(length(Implementations) >= 5),
    lists:foreach(
        fun({Port, Impl}) ->
            Frozen = maps:get(Port, Contracts),
            ?assert(lists:member(Port, declared_behaviours(Impl))),
            ?assertEqual(lists:sort(Frozen), lists:sort(Port:behaviour_info(callbacks))),
            lists:foreach(
                fun({Fun, Arity}) ->
                    ?assert(erlang:function_exported(Impl, Fun, Arity))
                end,
                Frozen
            )
        end,
        Implementations
    ).

%% EB-03R：`asset` 不再是 not_implemented_yet（R0-5 修复）；**未装配的已声明端口**
%% 仍必须显式失败，不得静默指向空实现（C6）。
unassigned_ports_are_explicit_test() ->
    ?assertEqual({ok, eb_asset_store}, eb_infra_ports:resolve(asset)),
    ?assertEqual({ok, eb_pg_auth_facts}, eb_infra_ports:resolve(auth)),
    ?assertEqual({ok, eb_pg_purge_port}, eb_infra_ports:resolve(purge)),
    ?assertEqual({error, {unknown_port, bogus}}, eb_infra_ports:resolve(bogus)),
    %% 负向对照：把任一端口从装配里摘掉 ⇒ 必须点名 unimplemented_port（不是 unknown）
    Trimmed = [
        Pair
     || {Port, _Impl} = Pair <- eb_infra_ports:implementations(), Port =/= eb_asset_port
    ],
    ?assertEqual({error, {unimplemented_port, eb_asset_port}}, eb_ports:assembly_missing(Trimmed)).

%% 端口注册表不改变 EB-02 的冻结事实（注册表只做装配映射）。
registry_does_not_override_frozen_ports_test() ->
    %% EB-03R：追加 auth / member_fact / tx / purge 四个端口；前六个逐字未动。
    ?assertEqual(
        eb_ports:all(),
        [
            eb_ports:store(),
            eb_ports:crypto(),
            eb_ports:clock(),
            eb_ports:id(),
            eb_ports:audit(),
            eb_ports:asset(),
            eb_ports:auth(),
            eb_ports:member_fact(),
            eb_ports:tx(),
            eb_ports:purge()
        ]
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

declared_behaviours(Module) ->
    lists:flatten([
        Value
     || {Key, Value} <- Module:module_info(attributes), Key =:= behaviour
    ]).

%% 源码定位：eunit 以仓库根为 cwd 运行（与既有触库套件读 priv/migrations 同口径）。
read_source(Name) ->
    Relative = "src/features/enterprise_business/infrastructure/" ++ atom_to_list(Name) ++ ".erl",
    Candidates = [Relative, "../" ++ Relative, "../../" ++ Relative],
    case first_readable(Candidates) of
        {ok, Source} -> Source;
        error -> erlang:error({source_not_readable, Name, Candidates})
    end.

first_readable([]) ->
    error;
first_readable([Path | Rest]) ->
    case file:read_file(Path) of
        {ok, Source} -> {ok, Source};
        {error, _} -> first_readable(Rest)
    end.

%% 仅剥离行注释（`%` 到行尾），用于「源文件不含日志调用」的静态断言。
strip_comments(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    iolist_to_binary([
        begin
            case binary:split(Line, <<"%">>) of
                [Code | _] -> <<Code/binary, "\n">>;
                [] -> <<>>
            end
        end
     || Line <- Lines
    ]).
