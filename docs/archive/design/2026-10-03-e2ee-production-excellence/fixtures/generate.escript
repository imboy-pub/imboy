#!/usr/bin/env escript
%%! -noshell
%%--------------------------------------------------------------------
%% C09（run-20261003-094804）golden vectors 生成器（一次性、可复现）。
%%
%% 生成 identity-transparency-profile.md §5 定义的 4 场景×5 类 vectors
%% （+ 全局不变量组 + S5 交叉签名组），全部密码学计算来自**主仓真实实现**：
%%   - canonical/MTH/proof：imboy/ebin/e2ee_kt_merkle.beam（只读加载）
%%   - Ed25519：deps/jose 纯 Erlang jose_jwa_ed25519（主仓既有依赖；
%%     规避本机 OTP29/OpenSSL3.6 crypto:sign(eddsa,…) 损坏路径，见证据卡）
%%
%% fix-round1（W1 评审 M1/M2）：新增 S5 交叉签名 vectors（R1/R2/R3 正例 +
%% 伪造/quorum 不足/域混淆/换叶重放负例），cross-sign 输入带显式域分离前缀
%%（profile §3.6：0x03 ‖ domain ‖ 0x00 ‖ leaf_bytes）。
%%
%% 用法（在只读 checkout 上跑，不写主仓）：
%%   escript generate.escript > vectors.json
%%
%% 输出确定性：固定事件、固定 timestamp、固定 key seed → 逐字节可复现。
%%--------------------------------------------------------------------
-module(c09_generate).

%% 主仓路径（只读）。相对本文件 .. 的 ../../../../.. 即 imboy 仓根。
-define(IMBOY_EBIN, "/Users/leeyi/project/imboy.pub/imboy/ebin").
-define(JOSE_EBIN, "/Users/leeyi/project/imboy.pub/imboy/deps/jose/ebin").

-define(HEX(B), binary:encode_hex(B, lowercase)).

%% profile §3.6 cross-sign 域常量（0x03 前缀体系的域分配）
-define(CS_DOMAIN_DEVICE, <<"imboy.kt.v2.crosssign.device.v1">>).
-define(CS_DOMAIN_AGENT, <<"imboy.kt.v2.crosssign.agent.v1">>).
-define(CS_DOMAIN_RECOVERY, <<"imboy.kt.v2.crosssign.recovery.v1">>).

main(_) ->
    _ = code:add_path(?IMBOY_EBIN),
    _ = code:add_path(?JOSE_EBIN),
    {module, _} = code:ensure_loaded(e2ee_kt_merkle),
    {module, _} = code:ensure_loaded(jose_jwa_ed25519),
    Md5 = ?HEX(erlang:get_module_info(e2ee_kt_merkle, md5)),

    %% ---- 确定性签名 key（与文档/fixtures 一致的 seed 派生）----
    {Pk1, Sk1} = key(<<"imboy-c09-kt-vector-key-1">>),
    {Pk2, Sk2} = key(<<"imboy-c09-kt-vector-key-2">>),

    %% ---- S5 交叉签名确定性 key（profile §3.6 / W1 评审 M1）----
    RootK = key(<<"imboy-c09-crosssign-root-key-1">>),
    DepK = key(<<"imboy-c09-crosssign-deploy-key-1">>),
    Rec1 = key(<<"imboy-c09-crosssign-recovery-key-1">>),
    Rec2 = key(<<"imboy-c09-crosssign-recovery-key-2">>),
    Rec3 = key(<<"imboy-c09-crosssign-recovery-key-3">>),
    AtkK = key(<<"imboy-c09-crosssign-attacker-key-1">>),

    %% ---- C06 anchor（canonical 与 Dart ai_identity_anchor.dart 逐字节同构）----
    AnchorDigest = anchor_digest(),

    %% ---- 三棵场景树 ----
    S1Events = s1_events(),
    S2Events = s2_events(AnchorDigest),
    S3Events = s3_events(),

    {J1, Ok1} = scenario_json(<<"S1">>, <<"human 多设备账号：publish×2 → rotate → revoke">>,
                              S1Events, {Pk1, Sk1}, 1753747200000),
    {J2, Ok2} = scenario_json(<<"S2">>, <<"AI agent 锚发布 + human peer 混合树">>,
                              S2Events, {Pk2, Sk2}, 1753747260000),
    {J3, Ok3} = scenario_json(<<"S3">>, <<"账号信任根：root_publish → root_revoke">>,
                              S3Events, {Pk1, Sk1}, 1753747320000),
    {J4, Ok4} = s4_json(S3Events, S1Events),
    {J5, Ok5} = s5_json(S1Events, S2Events, S3Events,
                        RootK, DepK, [Rec1, Rec2, Rec3], AtkK),

    AllOk = Ok1 and Ok2 and Ok3 and Ok4 and Ok5,
    Doc = #{
        <<"meta">> => #{
            <<"version">> => 2,
            <<"profile">> => <<"identity-transparency-profile.md (C09, run-20261003-094804, fix-round1)">>,
            <<"kt_module">> => <<"e2ee_kt_merkle">>,
            <<"kt_module_md5">> => Md5,
            <<"ed25519_impl">> => <<"jose_jwa_ed25519 (deps/jose, pure Erlang)">>,
            <<"domain_leaf_prefix_hex">> => <<"00">>,
            <<"domain_node_prefix_hex">> => <<"01">>,
            <<"domain_head_prefix_hex">> => <<"02">>,
            <<"crosssign_prefix_hex">> => <<"03">>,
            <<"crosssign_domains">> => #{
                <<"device">> => ?CS_DOMAIN_DEVICE,
                <<"agent">> => ?CS_DOMAIN_AGENT,
                <<"recovery">> => ?CS_DOMAIN_RECOVERY},
            <<"anchor_digest_sha256_hex">> => AnchorDigest,
            <<"self_check">> => AllOk,
            <<"note">> => <<"vectors 由生产 beam 实算生成；verify_python.py 按文档规范独立重算做第二实现交叉验证（AC-19）。version=2：新增 S5 交叉签名域分离 vectors（§3.6，W1 评审 M1/M2 修复）。">>
        },
        <<"scenarios">> => [J1, J2, J3, J4, J5]
    },
    try
        Out = iolist_to_binary([json_enc(Doc), $\n]),
        file:write(standard_io, Out)
    catch
        C:R:ST ->
            io:format(standard_error, "json encode failed: ~p:~p~n~p~n", [C, R, ST]),
            halt(2)
    end,
    case AllOk of
        true -> halt(0);
        false -> halt(1)
    end.

%%%===================================================================
%%% key / anchor
%%%===================================================================

key(Tag) ->
    Seed = binary:part(crypto:hash(sha256, Tag), 0, 32),
    jose_jwa_ed25519:keypair(Seed).

%% 与 C06 ai_identity_anchor.dart canonicalBytes() 逐字节同构（字段序硬编码）。
%% fixture 假材料为可读假 base64；真实锚材料不进 fixtures。
anchor_digest() ->
    Payload = <<
        "agent_ed25519=QUdFTlRfRFlfQjMyX0ZJWFRVUkUwMQ==\n"
        "agent_uid=2002\n"
        "anchor_version=1\n"
        "deployment_id=deploy-imboy-demo\n"
        "domain=imboy.ai-identity-anchor.v1\n"
        "identity_version=1\n"
        "issued_at=1760000000000\n"
        "server_ed25519=U0VSVkVSX0VENTI1NTE5X0RFTU9fUDBVQkxJQ0swMQ=="
    >>,
    ?HEX(crypto:hash(sha256, Payload)).

%%%===================================================================
%%% 场景事件
%%%===================================================================

s1_events() ->
    [#{<<"curve25519_key">> => <<"Y3VydmUx">>, <<"deployment_id">> => <<"deploy-imboy-demo">>,
       <<"device_id">> => <<"dev-A">>, <<"ed25519_key">> => <<"ZWQyNTUxOTE=">>,
       <<"event_type">> => <<"device_publish">>, <<"identity_version">> => 1,
       <<"subject_type">> => <<"device">>, <<"user_id">> => 1001},
     #{<<"curve25519_key">> => <<"Y3VydmUy">>, <<"deployment_id">> => <<"deploy-imboy-demo">>,
       <<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<"ZWQyNTUxOTI=">>,
       <<"event_type">> => <<"device_publish">>, <<"identity_version">> => 1,
       <<"subject_type">> => <<"device">>, <<"user_id">> => 1001},
     #{<<"curve25519_key">> => <<"TkVXX0NVUlZFMQ==">>, <<"deployment_id">> => <<"deploy-imboy-demo">>,
       <<"device_id">> => <<"dev-A">>, <<"ed25519_key">> => <<"TkVXX0VEMjU1MTlfMQ==">>,
       <<"event_type">> => <<"device_rotate">>, <<"identity_version">> => 2,
       <<"subject_type">> => <<"device">>, <<"user_id">> => 1001},
     #{<<"curve25519_key">> => <<>>, <<"deployment_id">> => <<"deploy-imboy-demo">>,
       <<"device_id">> => <<"dev-B">>, <<"ed25519_key">> => <<>>,
       <<"event_type">> => <<"device_revoke">>, <<"identity_version">> => 2,
       <<"subject_type">> => <<"device">>, <<"user_id">> => 1001}].

s2_events(AnchorDigest) ->
    [#{<<"anchor_digest">> => AnchorDigest, <<"deployment_id">> => <<"deploy-imboy-demo">>,
       <<"event_type">> => <<"agent_publish">>, <<"identity_version">> => 1,
       <<"subject_type">> => <<"agent">>, <<"user_id">> => 2002},
     #{<<"curve25519_key">> => <<"MzAwM19kZXYtQ19jdXJ2ZQ==">>, <<"deployment_id">> => <<"deploy-imboy-demo">>,
       <<"device_id">> => <<"dev-C">>, <<"ed25519_key">> => <<"MzAwM19kZXYtQ19lZA==">>,
       <<"event_type">> => <<"device_publish">>, <<"identity_version">> => 1,
       <<"subject_type">> => <<"device">>, <<"user_id">> => 3003}].

s3_events() ->
    [#{<<"deployment_id">> => <<"deploy-imboy-demo">>,
       <<"event_type">> => <<"root_publish">>, <<"key_id">> =>
       <<"a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f90">>,
       <<"quorum_threshold">> => 2, <<"root_ed25519">> => <<"Um9vdEVkMjU1MTlfREVPXzAx">>,
       <<"subject_type">> => <<"root">>, <<"user_id">> => 1001},
     #{<<"deployment_id">> => <<"deploy-imboy-demo">>,
       <<"event_type">> => <<"root_revoke">>, <<"key_id">> =>
       <<"a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f90">>,
       <<"quorum_threshold">> => 0, <<"root_ed25519">> => <<>>,
       <<"subject_type">> => <<"root">>, <<"user_id">> => 1001}].

%%%===================================================================
%%% 场景组装（5 类 vector + 自验）
%%%===================================================================

scenario_json(Id, Desc, Events, {Pk, Sk}, TsMs) ->
    Bytes = [canonical(E) || E <- Events],
    N = length(Events),
    Root = e2ee_kt_merkle:mth(Bytes),

    %% head（v2 字段集，§3.3）
    HeadMap = head_map(Root, N, TsMs),
    HeadBytes = canonical(HeadMap),
    SigningInput = e2ee_kt_merkle:tree_head_signing_input(HeadBytes),
    Sig = jose_jwa_ed25519:ed25519_sign(SigningInput, Sk),

    InclIdx = incl_index(Id),
    {MIdx, MN} = consistency_pair(Id),

    %% V1 inclusion_ok
    InclLeaf = lists:nth(InclIdx + 1, Bytes),
    InclLH = e2ee_kt_merkle:leaf_hash(InclLeaf),
    InclPath = e2ee_kt_merkle:inclusion_path(InclIdx, Bytes),
    OkV1 = e2ee_kt_merkle:verify_inclusion(InclLH, InclIdx, N, InclPath, Root),

    %% V2 consistency_ok
    RootM = e2ee_kt_merkle:mth(lists:sublist(Bytes, MIdx)),
    CPath = e2ee_kt_merkle:consistency_path(MIdx, Bytes),
    OkV2 = e2ee_kt_merkle:verify_consistency(MIdx, MN, CPath, RootM, Root),

    %% V3 bad_leaf：错误叶子（用 index 0 的 hash 冒充 index MIdx 的叶子）+ 编码拒绝组
    WrongLeaf = e2ee_kt_merkle:leaf_hash(lists:nth(1, Bytes)), %% index 0 ≠ MIdx（三场景均成立）
    BadPath = e2ee_kt_merkle:inclusion_path(MIdx, Bytes),
    OkV3 = not e2ee_kt_merkle:verify_inclusion(WrongLeaf, MIdx, N, BadPath, Root),
    RejNl = is_error(e2ee_kt_merkle:canonical_event_bytes(#{<<"a">> => <<"x\ny">>})),
    RejEq = is_error(e2ee_kt_merkle:canonical_event_bytes(#{<<"a=b">> => <<"v">>})),
    RejEmpty = is_error(e2ee_kt_merkle:canonical_event_bytes(#{})),

    %% V4 tampered_head：按场景篡改 head 字段（S1 timestamp / S2 tree_size / S3 log_version）
    TamperedMap = tamper_head(Id, HeadMap),
    TamperedBytes = canonical(TamperedMap),
    TamperedInput = e2ee_kt_merkle:tree_head_signing_input(TamperedBytes),
    OkV4a = TamperedInput =/= SigningInput,
    OkV4b = not jose_jwa_ed25519:ed25519_verify(Sig, TamperedInput, Pk),
    TamperedSig = jose_jwa_ed25519:ed25519_sign(TamperedInput, Sk),
    OkV4c = jose_jwa_ed25519:ed25519_verify(TamperedSig, TamperedInput, Pk),

    %% V5 fork_head：同 tree_size 叶子重排 → root 分叉
    ForkBytes = fork_order(Id, Bytes),
    ForkRoot = e2ee_kt_merkle:mth(ForkBytes),
    OkV5a = ForkRoot =/= Root,
    OkV5b = not e2ee_kt_merkle:verify_consistency(N, N, [], Root, ForkRoot),
    OkV5c = e2ee_kt_merkle:verify_consistency(N, N, [], Root, Root),

    AllOk = OkV1 and OkV2 and OkV3 and RejNl and RejEq and RejEmpty
            and OkV4a and OkV4b and OkV4c and OkV5a and OkV5b and OkV5c,

    J = #{
        <<"id">> => Id,
        <<"description">> => Desc,
        <<"self_checks">> => #{
            <<"v1_verify_inclusion">> => OkV1,
            <<"v2_verify_consistency">> => OkV2,
            <<"v3_wrong_leaf_rejected">> => OkV3,
            <<"v3_reject_newline">> => RejNl,
            <<"v3_reject_eq_key">> => RejEq,
            <<"v3_reject_empty">> => RejEmpty,
            <<"v4_signing_input_changed">> => OkV4a,
            <<"v4_orig_sig_rejected">> => OkV4b,
            <<"v4_attacker_resign">> => OkV4c,
            <<"v5_roots_differ">> => OkV5a,
            <<"v5_consistency_fork_rejected">> => OkV5b,
            <<"v5_consistency_self_root">> => OkV5c
        },
        <<"events">> => Events,
        <<"tree">> => #{
            <<"canonical_bytes_hex">> => [?HEX(B) || B <- Bytes],
            <<"leaf_hashes_hex">> => [?HEX(e2ee_kt_merkle:leaf_hash(B)) || B <- Bytes],
            <<"root_hash_hex">> => ?HEX(Root)
        },
        <<"signing_key">> => #{
            <<"tag">> => sign_key_tag(Id),
            <<"seed_sha256_first32_hex">> => ?HEX(binary:part(crypto:hash(sha256, sign_key_tag(Id)), 0, 32)),
            <<"public_key_hex">> => ?HEX(Pk)
        },
        <<"tree_head">> => #{
            <<"input_fields">> => HeadMap,
            <<"canonical_hex">> => ?HEX(HeadBytes),
            <<"signing_input_hex">> => ?HEX(SigningInput),
            <<"signature_hex">> => ?HEX(Sig),
            <<"key_id_wire">> => ?HEX(crypto:hash(sha256, Pk))
        },
        <<"vectors">> => [
            #{
                <<"id">> => <<Id/binary, "-V1">>, <<"kind">> => <<"inclusion_ok">>,
                <<"input">> => #{<<"leaf_index">> => InclIdx, <<"tree_size">> => N},
                <<"expected">> => #{
                    <<"leaf_canonical_hex">> => ?HEX(InclLeaf),
                    <<"leaf_hash_hex">> => ?HEX(InclLH),
                    <<"root_hash_hex">> => ?HEX(Root),
                    <<"audit_path_hex">> => [?HEX(P) || P <- InclPath],
                    <<"verify_inclusion">> => OkV1}
            },
            #{
                <<"id">> => <<Id/binary, "-V2">>, <<"kind">> => <<"consistency_ok">>,
                <<"input">> => #{<<"first_size">> => MIdx, <<"second_size">> => MN},
                <<"expected">> => #{
                    <<"first_root_hex">> => ?HEX(RootM),
                    <<"second_root_hex">> => ?HEX(Root),
                    <<"consistency_path_hex">> => [?HEX(P) || P <- CPath],
                    <<"verify_consistency">> => OkV2}
            },
            #{
                <<"id">> => <<Id/binary, "-V3">>, <<"kind">> => <<"bad_leaf">>,
                <<"input">> => #{
                    <<"wrong_leaf_hash_from_index">> => 0,
                    <<"claimed_leaf_index">> => MIdx,
                    <<"reject_encode_cases">> => [<<"value_contains_newline">>,
                                                   <<"key_contains_equals">>,
                                                   <<"empty_field_set">>]},
                <<"expected">> => #{
                    <<"wrong_leaf_hash_hex">> => ?HEX(WrongLeaf),
                    <<"audit_path_hex">> => [?HEX(P) || P <- BadPath],
                    <<"root_hash_hex">> => ?HEX(Root),
                    <<"verify_inclusion_wrong_leaf">> => false,
                    <<"self_check_wrong_leaf_rejected">> => OkV3,
                    <<"reject_encode_newline">> => RejNl,
                    <<"reject_encode_eq_in_key">> => RejEq,
                    <<"reject_empty_field_set">> => RejEmpty}
            },
            #{
                <<"id">> => <<Id/binary, "-V4">>, <<"kind">> => <<"tampered_head">>,
                <<"input">> => #{<<"mutation">> => tamper_desc(Id)},
                <<"expected">> => #{
                    <<"tampered_head_canonical_hex">> => ?HEX(TamperedBytes),
                    <<"tampered_signing_input_hex">> => ?HEX(TamperedInput),
                    <<"tampered_signature_hex">> => ?HEX(TamperedSig),
                    <<"signing_input_changed">> => OkV4a,
                    <<"original_sig_verifies_tampered">> => false,
                    <<"self_check_orig_sig_rejected">> => OkV4b,
                    <<"attacker_resign_succeeds_but_is_fork_surface">> => OkV4c}
            },
            #{
                <<"id">> => <<Id/binary, "-V5">>, <<"kind">> => <<"fork_head">>,
                <<"input">> => #{<<"fork_permutation">> => fork_perm(Id)},
                <<"expected">> => #{
                    <<"fork_root_hash_hex">> => ?HEX(ForkRoot),
                    <<"root_hash_hex">> => ?HEX(Root),
                    <<"tree_size">> => N,
                    <<"roots_differ">> => OkV5a,
                    <<"verify_consistency_same_size_fork">> => false,
                    <<"self_check_consistency_fork_rejected">> => OkV5b,
                    <<"self_check_consistency_self_root">> => OkV5c,
                    <<"split_view_detected">> => OkV5a and OkV5b}
            }
        ]
    },
    {J, AllOk}.

%% 各场景 inclusion 目标叶 & consistency 对（覆盖不同树形：偶/奇 split、右子叶）
incl_index(<<"S1">>) -> 2;   %% rotate 叶（奇树 k=2 右子叶，非平凡 path）
incl_index(<<"S2">>) -> 0;   %% agent 叶（左子叶）
incl_index(<<"S3">>) -> 0.   %% root_publish 叶

consistency_pair(<<"S1">>) -> {2, 4};  %% 非平衡：左 2 右 2，m 为 2 的幂
consistency_pair(<<"S2">>) -> {1, 2};  %% m=1（最小前缀）
consistency_pair(<<"S3">>) -> {1, 2}.

%% 按场景分派的 head 篡改：S1 时间戳 +1ms；S2 tree_size +1（root 不变）；
%% S3 log_version 降级 2→1（schema 降级攻击面）。
tamper_head(<<"S1">>, M) -> M#{<<"timestamp_ms">> := maps:get(<<"timestamp_ms">>, M) + 1};
tamper_head(<<"S2">>, M) -> M#{<<"tree_size">> := maps:get(<<"tree_size">>, M) + 1};
tamper_head(<<"S3">>, M) -> M#{<<"log_version">> := maps:get(<<"log_version">>, M) - 1}.

tamper_desc(<<"S1">>) -> <<"timestamp_ms 1753747200000 -> 1753747200001">>;
tamper_desc(<<"S2">>) -> <<"tree_size 2 -> 3 (root 不变)">>;
tamper_desc(<<"S3">>) -> <<"log_version 2 -> 1 (schema 降级)">>.

fork_perm(<<"S1">>) -> [1, 0, 2, 3];
fork_perm(<<"S2">>) -> [1, 0];
fork_perm(<<"S3">>) -> [1, 0].

%% 真正的叶子重排：交换前两叶（同 tree_size、不同叶序 ⇒ 不同 root ⇒ 分叉）。
fork_order(<<"S1">>, [B0, B1, B2, B3]) -> [B1, B0, B2, B3];
fork_order(_, [B0, B1]) -> [B1, B0].

sign_key_tag(<<"S2">>) -> <<"imboy-c09-kt-vector-key-2">>;
sign_key_tag(_) -> <<"imboy-c09-kt-vector-key-1">>.

%%%===================================================================
%%% S4 全局不变量
%%%===================================================================

s4_json(S3Events, S1Events) ->
    EmptyRoot = e2ee_kt_merkle:mth([]),
    OkEmpty = EmptyRoot =:= crypto:hash(sha256, <<>>),
    X = canonical(hd(S3Events)),
    Single = e2ee_kt_merkle:mth([X]),
    OkSingle = Single =:= e2ee_kt_merkle:leaf_hash(X),
    RejNl = is_error(e2ee_kt_merkle:canonical_event_bytes(#{<<"k">> => <<"a\nb">>})),
    RejEq = is_error(e2ee_kt_merkle:canonical_event_bytes(#{<<"x=y">> => <<"v">>})),
    RejCr = is_error(e2ee_kt_merkle:canonical_event_bytes(#{<<"k">> => <<"a\rb">>})),
    RejEmpty = is_error(e2ee_kt_merkle:canonical_event_bytes(#{})),
    %% int 2 与 binary "2" 渲染一致（跨实现类型映射契约）
    BA = canonical(#{<<"a">> => 2}),
    BB = canonical(#{<<"a">> => <<"2">>}),
    OkTyping = BA =:= BB,
    %% consistency 同大小自证（S1 树，n=4）
    S1Bytes = [canonical(E) || E <- S1Events],
    R4 = e2ee_kt_merkle:mth(S1Bytes),
    OkSelf = e2ee_kt_merkle:verify_consistency(4, 4, [], R4, R4),
    AllOk = OkEmpty and OkSingle and RejNl and RejEq and RejCr and RejEmpty
            and OkTyping and OkSelf,
    J = #{
        <<"id">> => <<"S4">>,
        <<"description">> => <<"全局不变量：空树根 / 单叶恒等 / 编码拒绝 / 类型映射 / 同大小一致性">>,
        <<"vectors">> => [
            #{<<"id">> => <<"S4-V1">>, <<"kind">> => <<"empty_tree">>,
              <<"input">> => #{<<"leaves">> => 0},
              <<"expected">> => #{
                  <<"root_hash_hex">> => ?HEX(EmptyRoot),
                  <<"must_equal_sha256_of_empty_string">> => true,
                  <<"self_check">> => OkEmpty}},
            #{<<"id">> => <<"S4-V2">>, <<"kind">> => <<"single_leaf_identity">>,
              <<"input">> => #{<<"source">> => <<"S3-L0">>},
              <<"expected">> => #{
                  <<"leaf_hash_hex">> => ?HEX(e2ee_kt_merkle:leaf_hash(X)),
                  <<"mth_single_hex">> => ?HEX(Single),
                  <<"equal">> => true, <<"self_check">> => OkSingle}},
            #{<<"id">> => <<"S4-V3">>, <<"kind">> => <<"canonical_reject">>,
              <<"input">> => #{<<"cases">> => [<<"value_0x0a">>, <<"value_0x0d">>,
                                                <<"key_contains_eq">>, <<"empty_field_set">>]},
              <<"expected">> => #{
                  <<"reject_value_lf">> => RejNl, <<"reject_value_cr">> => RejCr,
                  <<"reject_key_eq">> => RejEq, <<"reject_empty">> => RejEmpty}},
            #{<<"id">> => <<"S4-V4">>, <<"kind">> => <<"type_mapping">>,
              <<"input">> => #{<<"int_value">> => 2, <<"binary_value">> => <<"2">>},
              <<"expected">> => #{
                  <<"int_canonical_hex">> => ?HEX(BA),
                  <<"binary_canonical_hex">> => ?HEX(BB),
                  <<"equal">> => OkTyping}},
            #{<<"id">> => <<"S4-V5">>, <<"kind">> => <<"consistency_same_size">>,
              <<"input">> => #{<<"m">> => 4, <<"n">> => 4},
              <<"expected">> => #{
                  <<"root_hash_hex">> => ?HEX(R4),
                  <<"verify_consistency_self">> => OkSelf,
                  <<"verify_consistency_fork_same_size">> => false}}
        ]
    },
    {J, AllOk}.

%%%===================================================================
%%% S5 交叉签名（profile §3.6 域分离；W1 评审 M1/M2）
%%%===================================================================

%% cross_sign_signing_input(Domain, LeafBytes) = 0x03 ‖ domain ‖ 0x00 ‖ leaf_bytes
crosssign_input(Domain, LeafBytes) ->
    <<16#03, Domain/binary, 16#00, LeafBytes/binary>>.

key_json(Tag, {Pk, _Sk}) ->
    #{<<"tag">> => Tag,
      <<"seed_sha256_first32_hex">> => ?HEX(binary:part(crypto:hash(sha256, Tag), 0, 32)),
      <<"public_key_hex">> => ?HEX(Pk)}.

%% quorum 判定（profile §3.2/§3.6）：呈交的每份签名，对**全部登记** recovery
%% 公钥逐一试验签；在任一登记公钥下验签通过即计 1 份有效。有效份数 ≥ M 即达标。
%% ——错钥伪造签名对全部登记公钥都验败，不凑数（凑数攻击防线）。
quorum_valid_count(LeafBytes, Domain, EnrolledPks, PresentedSigs) ->
    Input = crosssign_input(Domain, LeafBytes),
    length([ok || Sig <- PresentedSigs,
                  lists:any(fun(Pk) ->
                                jose_jwa_ed25519:ed25519_verify(Sig, Input, Pk)
                            end, EnrolledPks)]).

s5_json(S1Events, S2Events, S3Events, {KRp, KRs}, {KDp, KDs},
        [{R1p, R1s}, {R2p, R2s}, {R3p, _R3s}], {KAp, KAs}) ->
    %% 被签对象：复用既有场景 leaf（canonical 由生产 beam 计算）
    LeafR1 = canonical(lists:nth(3, S1Events)),   %% S1-L2 device_rotate
    LeafR2 = canonical(lists:nth(1, S2Events)),   %% S2-L0 agent_publish
    LeafR3 = canonical(lists:nth(1, S3Events)),   %% S3-L0 root_publish (M=2)
    Enrolled = [R1p, R2p, R3p],                   %% N=3 登记 recovery 公钥

    %% ---- V1: R1 合法 device cross-sign（root 签 device leaf）----
    InR1 = crosssign_input(?CS_DOMAIN_DEVICE, LeafR1),
    SigR1 = jose_jwa_ed25519:ed25519_sign(InR1, KRs),
    OkV1 = jose_jwa_ed25519:ed25519_verify(SigR1, InR1, KRp),

    %% ---- V2: R2 agent 双背书（root + deploy 各签同一输入）----
    InR2 = crosssign_input(?CS_DOMAIN_AGENT, LeafR2),
    SigR2Root = jose_jwa_ed25519:ed25519_sign(InR2, KRs),
    SigR2Deploy = jose_jwa_ed25519:ed25519_sign(InR2, KDs),
    SigR2Atk = jose_jwa_ed25519:ed25519_sign(InR2, KAs),
    OkV2a = jose_jwa_ed25519:ed25519_verify(SigR2Root, InR2, KRp)
             andalso jose_jwa_ed25519:ed25519_verify(SigR2Deploy, InR2, KDp),
    OkV2b = not jose_jwa_ed25519:ed25519_verify(SigR2Atk, InR2, KRp),  %% 伪造 root 侧拒
    OkV2c = SigR2Root =/= SigR2Deploy,                                  %% 异钥异签

    %% ---- V3: R3 recovery quorum 2-of-3 达标 ----
    InR3 = crosssign_input(?CS_DOMAIN_RECOVERY, LeafR3),
    SigR3a = jose_jwa_ed25519:ed25519_sign(InR3, R1s),
    SigR3b = jose_jwa_ed25519:ed25519_sign(InR3, R2s),
    OkV3a = jose_jwa_ed25519:ed25519_verify(SigR3a, InR3, R1p)
             andalso jose_jwa_ed25519:ed25519_verify(SigR3b, InR3, R2p),
    OkV3b = quorum_valid_count(LeafR3, ?CS_DOMAIN_RECOVERY,
                               Enrolled, [SigR3a, SigR3b]) =:= 2,

    %% ---- V4: 伪造 root 签名（错钥）拒绝 ----
    SigAtkR1 = jose_jwa_ed25519:ed25519_sign(InR1, KAs),  %% 对与 V1 完全相同的输入签
    OkV4a = jose_jwa_ed25519:ed25519_verify(SigAtkR1, InR1, KAp),    %% 攻击者自证成立（分叉面，诚实记录，同 C06 模式）
    OkV4b = not jose_jwa_ed25519:ed25519_verify(SigAtkR1, InR1, KRp), %% root 公钥验必败
    OkV4c = SigAtkR1 =/= SigR1,

    %% ---- V5: quorum 不足拒绝 ----
    SigAtkR3 = jose_jwa_ed25519:ed25519_sign(InR3, KAs),  %% attacker 对 recovery 域输入合法签名（凑数攻击材料）
    Cnt1 = quorum_valid_count(LeafR3, ?CS_DOMAIN_RECOVERY, Enrolled, [SigR3a]),
    Cnt2 = quorum_valid_count(LeafR3, ?CS_DOMAIN_RECOVERY, Enrolled, [SigR3a, SigAtkR3]),
    Cnt0 = quorum_valid_count(LeafR3, ?CS_DOMAIN_RECOVERY, Enrolled, []),
    OkV5a = (Cnt1 =:= 1) andalso (Cnt1 < 2),  %% 只交 1 份有效 → 1 < 2 拒
    OkV5b = (Cnt2 =:= 1) andalso (Cnt2 < 2),  %% 凑数：attacker 份对登记公钥全验败不计入 → 仍 1 < 2 拒
    OkV5c = (Cnt0 =:= 0) andalso (Cnt0 < 2),  %% 无签 → 0 < 2 拒

    %% ---- V6: 域混淆拒绝（device 域签名当 recovery/agent 域用）----
    %% 取 V1 的合法 root 签名（device 域 ‖ S1-L2），在其它域常量重组的输入下验：
    ConfR = crosssign_input(?CS_DOMAIN_RECOVERY, LeafR1),
    ConfA = crosssign_input(?CS_DOMAIN_AGENT, LeafR1),
    OkV6a = jose_jwa_ed25519:ed25519_verify(SigR1, InR1, KRp),          %% 原域（device）验签通过——签名本身完好
    OkV6b = not jose_jwa_ed25519:ed25519_verify(SigR1, ConfR, KRp),     %% recovery 域必败
    OkV6c = not jose_jwa_ed25519:ed25519_verify(SigR1, ConfA, KRp),     %% agent 域必败
    OkV6d = (ConfR =/= InR1) andalso (ConfA =/= InR1),                  %% 三域输入互异（结构前提）

    %% ---- V7: 换叶重放拒绝（S1-L2 的签名重放到 S1-L0，同域）----
    LeafR1Alt = canonical(lists:nth(1, S1Events)),  %% S1-L0 device_publish(dev-B)
    ReplayIn = crosssign_input(?CS_DOMAIN_DEVICE, LeafR1Alt),
    OkV7a = not jose_jwa_ed25519:ed25519_verify(SigR1, ReplayIn, KRp),  %% 重放必败（签名绑定 leaf canonical bytes）
    OkV7b = jose_jwa_ed25519:ed25519_verify(SigR1, InR1, KRp),          %% 原叶验签仍通过
    OkV7c = LeafR1Alt =/= LeafR1,

    AllOk = OkV1 and OkV2a and OkV2b and OkV2c and OkV3a and OkV3b
            and OkV4a and OkV4b and OkV4c and OkV5a and OkV5b and OkV5c
            and OkV6a and OkV6b and OkV6c and OkV6d and OkV7a and OkV7b and OkV7c,

    J = #{
        <<"id">> => <<"S5">>,
        <<"description">> => <<"交叉签名（R1/R2/R3）：§3.6 域分离输入 0x03‖domain‖0x00‖leaf；合法/伪造错钥/quorum 不足/凑数/域混淆/换叶重放">>,
        <<"keys">> => #{
            <<"root">> => key_json(<<"imboy-c09-crosssign-root-key-1">>, {KRp, KRs}),
            <<"deploy">> => key_json(<<"imboy-c09-crosssign-deploy-key-1">>, {KDp, KDs}),
            <<"recovery_1">> => key_json(<<"imboy-c09-crosssign-recovery-key-1">>, {R1p, R1s}),
            <<"recovery_2">> => key_json(<<"imboy-c09-crosssign-recovery-key-2">>, {R2p, R2s}),
            <<"recovery_3">> => key_json(<<"imboy-c09-crosssign-recovery-key-3">>, {R3p, _R3s}),
            <<"attacker">> => key_json(<<"imboy-c09-crosssign-attacker-key-1">>, {KAp, KAs})
        },
        <<"domains">> => #{
            <<"device">> => ?CS_DOMAIN_DEVICE,
            <<"agent">> => ?CS_DOMAIN_AGENT,
            <<"recovery">> => ?CS_DOMAIN_RECOVERY
        },
        <<"self_checks">> => #{
            <<"v1_legit_device_crosssign">> => OkV1,
            <<"v2_double_endorse_both_verify">> => OkV2a,
            <<"v2_attacker_rejected_by_root">> => OkV2b,
            <<"v2_signatures_differ">> => OkV2c,
            <<"v3_each_signature_verifies">> => OkV3a,
            <<"v3_quorum_2of3_satisfied">> => OkV3b,
            <<"v4_attacker_selfverify_succeeds">> => OkV4a,
            <<"v4_forged_rejected_by_root">> => OkV4b,
            <<"v4_forged_differs_from_legit">> => OkV4c,
            <<"v5_one_valid_insufficient">> => OkV5a,
            <<"v5_forged_not_counted">> => OkV5b,
            <<"v5_empty_insufficient">> => OkV5c,
            <<"v6_device_domain_accepts">> => OkV6a,
            <<"v6_recovery_domain_rejects">> => OkV6b,
            <<"v6_agent_domain_rejects">> => OkV6c,
            <<"v6_inputs_differ">> => OkV6d,
            <<"v7_leaf_swap_replay_rejected">> => OkV7a,
            <<"v7_original_still_verifies">> => OkV7b,
            <<"v7_leaves_differ">> => OkV7c
        },
        <<"vectors">> => [
            #{<<"id">> => <<"S5-V1">>, <<"kind">> => <<"r1_device_crosssign_ok">>,
              <<"input">> => #{<<"rule">> => <<"R1">>, <<"leaf_source">> => <<"S1-L2">>,
                               <<"domain">> => ?CS_DOMAIN_DEVICE, <<"signer">> => <<"root">>},
              <<"expected">> => #{
                  <<"leaf_canonical_hex">> => ?HEX(LeafR1),
                  <<"crosssign_input_hex">> => ?HEX(InR1),
                  <<"signature_hex">> => ?HEX(SigR1),
                  <<"verify_with_root_pk">> => OkV1}},
            #{<<"id">> => <<"S5-V2">>, <<"kind">> => <<"r2_agent_double_endorse_ok">>,
              <<"input">> => #{<<"rule">> => <<"R2">>, <<"leaf_source">> => <<"S2-L0">>,
                               <<"domain">> => ?CS_DOMAIN_AGENT,
                               <<"signers">> => [<<"root">>, <<"deploy">>]},
              <<"expected">> => #{
                  <<"leaf_canonical_hex">> => ?HEX(LeafR2),
                  <<"crosssign_input_hex">> => ?HEX(InR2),
                  <<"root_signature_hex">> => ?HEX(SigR2Root),
                  <<"deploy_signature_hex">> => ?HEX(SigR2Deploy),
                  <<"attacker_signature_hex">> => ?HEX(SigR2Atk),
                  <<"verify_root_sig_with_root_pk">> => true,
                  <<"verify_deploy_sig_with_deploy_pk">> => true,
                  <<"verify_attacker_sig_with_root_pk">> => false,
                  <<"signatures_differ">> => OkV2c}},
            #{<<"id">> => <<"S5-V3">>, <<"kind">> => <<"r3_recovery_quorum_ok">>,
              <<"input">> => #{<<"rule">> => <<"R3">>, <<"leaf_source">> => <<"S3-L0">>,
                               <<"domain">> => ?CS_DOMAIN_RECOVERY,
                               <<"quorum_threshold">> => 2, <<"enrolled_keys">> => 3,
                               <<"presented">> => [<<"recovery_1">>, <<"recovery_2">>]},
              <<"expected">> => #{
                  <<"leaf_canonical_hex">> => ?HEX(LeafR3),
                  <<"crosssign_input_hex">> => ?HEX(InR3),
                  <<"signatures_hex">> => [?HEX(SigR3a), ?HEX(SigR3b)],
                  <<"verify_each">> => [true, true],
                  <<"valid_count">> => 2,
                  <<"quorum_satisfied">> => OkV3b}},
            #{<<"id">> => <<"S5-V4">>, <<"kind">> => <<"forged_root_signature_rejected">>,
              <<"input">> => #{<<"rule">> => <<"R1">>, <<"leaf_source">> => <<"S1-L2">>,
                               <<"domain">> => ?CS_DOMAIN_DEVICE, <<"signer">> => <<"attacker">>},
              <<"expected">> => #{
                  <<"crosssign_input_hex">> => ?HEX(InR1),
                  <<"attacker_signature_hex">> => ?HEX(SigAtkR1),
                  <<"legit_signature_hex">> => ?HEX(SigR1),
                  <<"verify_with_attacker_pk">> => OkV4a,
                  <<"verify_with_root_pk">> => false,
                  <<"signatures_differ">> => OkV4c}},
            #{<<"id">> => <<"S5-V5">>, <<"kind">> => <<"quorum_insufficient_rejected">>,
              <<"input">> => #{<<"rule">> => <<"R3">>, <<"leaf_source">> => <<"S3-L0">>,
                               <<"quorum_threshold">> => 2, <<"enrolled_keys">> => 3,
                               <<"cases">> => [<<"one_valid_sig">>,
                                               <<"one_valid_plus_forged">>,
                                               <<"no_signatures">>]},
              <<"expected">> => #{
                  <<"case_one_valid_sig">> => #{<<"valid_count">> => Cnt1, <<"quorum_satisfied">> => false},
                  <<"case_one_valid_plus_forged">> => #{<<"valid_count">> => Cnt2, <<"quorum_satisfied">> => false},
                  <<"case_no_signatures">> => #{<<"valid_count">> => Cnt0, <<"quorum_satisfied">> => false},
                  <<"forged_sig_hex">> => ?HEX(SigAtkR3),
                  <<"first_valid_sig_hex">> => ?HEX(SigR3a)}},
            #{<<"id">> => <<"S5-V6">>, <<"kind">> => <<"crosssign_domain_confusion_rejected">>,
              <<"input">> => #{<<"signature_of">> => <<"S5-V1">>,
                               <<"signed_domain">> => ?CS_DOMAIN_DEVICE,
                               <<"leaf_source">> => <<"S1-L2">>,
                               <<"confused_domains">> => [?CS_DOMAIN_RECOVERY, ?CS_DOMAIN_AGENT]},
              <<"expected">> => #{
                  <<"device_domain_input_hex">> => ?HEX(InR1),
                  <<"recovery_domain_input_hex">> => ?HEX(ConfR),
                  <<"agent_domain_input_hex">> => ?HEX(ConfA),
                  <<"verify_device_domain">> => OkV6a,
                  <<"verify_recovery_domain">> => false,
                  <<"verify_agent_domain">> => false,
                  <<"inputs_mutually_distinct">> => OkV6d}},
            #{<<"id">> => <<"S5-V7">>, <<"kind">> => <<"leaf_swap_replay_rejected">>,
              <<"input">> => #{<<"signature_of">> => <<"S5-V1">>,
                               <<"replay_onto">> => <<"S1-L0">>,
                               <<"domain">> => ?CS_DOMAIN_DEVICE},
              <<"expected">> => #{
                  <<"original_leaf_canonical_hex">> => ?HEX(LeafR1),
                  <<"replay_leaf_canonical_hex">> => ?HEX(LeafR1Alt),
                  <<"replay_input_hex">> => ?HEX(ReplayIn),
                  <<"verify_replay_with_root_pk">> => false,
                  <<"verify_original_with_root_pk">> => OkV7b,
                  <<"leaves_differ">> => OkV7c}}
        ]
    },
    {J, AllOk}.

%%%===================================================================
%%% 工具
%%%===================================================================

canonical(Map) ->
    {ok, B} = e2ee_kt_merkle:canonical_event_bytes(Map),
    B.

is_error({error, _}) -> true;
is_error(_) -> false.

head_map(Root, N, TsMs) ->
    #{<<"deployment_id">> => <<"deploy-imboy-demo">>,
      <<"domain">> => <<"imboy.kt.v2.tree_head">>,
      <<"log_id">> => <<"imboy-identity-log">>,
      <<"log_version">> => 2,
      <<"root_hash">> => ?HEX(Root),
      <<"timestamp_ms">> => TsMs,
      <<"tree_size">> => N}.

%%%===================================================================
%%% 极简 JSON 编码（binary/integer/boolean/list/map；字符串做 JSON 转义）
%%%===================================================================

json_enc(true) -> <<"true">>;
json_enc(false) -> <<"false">>;
json_enc(null) -> <<"null">>;
json_enc(I) when is_integer(I) -> integer_to_binary(I);
json_enc(B) when is_binary(B) -> [$", json_str(B), $"];
json_enc(L) when is_list(L) ->
    [$[, lists:join($,, [json_enc(E) || E <- L]), $]];
json_enc(M) when is_map(M) ->
    Pairs = lists:keysort(1, [{K, json_enc(V)} || K := V <- M]),
    [${, lists:join($,, [[json_enc(K), $:, V] || {K, V} <- Pairs]), $}].

%% 字节级处理：UTF-8 字节流（≥0x80）原样透传即合法 JSON；仅转义
%% "、\ 与 <0x20 控制字节。不做 UTF-8 解码（解码对非字符输入不稳）。
json_str(B) -> json_str(B, <<>>).
json_str(<<>>, Acc) -> Acc;
json_str(<<C, Rest/binary>>, Acc) ->
    E = case C of
        $" -> <<"\\\"">>;
        $\\ -> <<"\\\\">>;
        $\n -> <<"\\n">>;
        $\r -> <<"\\r">>;
        $\t -> <<"\\t">>;
        _ when C < 16#20 -> <<"\\u00", (hex2(C))/binary>>;
        _ -> <<C>>
    end,
    json_str(Rest, <<Acc/binary, E/binary>>).

hex2(C) ->
    S0 = string:lowercase(integer_to_binary(C, 16)),
    case byte_size(S0) of
        1 -> <<"0", S0/binary>>;
        2 -> S0
    end.
