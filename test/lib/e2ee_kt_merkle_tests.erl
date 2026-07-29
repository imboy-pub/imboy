%%%-------------------------------------------------------------------
%%% E2EE-065 Slice 4 验收 —— Merkle 树与 proof
%%%
%%% 两条主线：
%%% 1. **把 profile 的 golden vector 钉死**。`29-...profile-v1.md` §8 的向量此前
%%%    **只存在于文档里**（§10 残留 1）；文档里的常量不会因为实现漂移而变红。
%%% 2. **穷举交叉核验**。proof 生成照 RFC 6962 递归定义直译（最贴规范原文），
%%%    验证走标准迭代算法（验证方拿不到整棵树）。两者互为对照：
%%%    穷举 n≤16 的**全部** (index,size) 与 (m,n)，迭代算法任何细微写错都会被照出来。
%%%
%%% ⚠️ 只验"篡改能否拒收"无效——恒 false 的 verify 在负向矩阵上满分。
%%% 故每组都配正向可用性锚点（合法 proof **必须**被接受）。
%%%-------------------------------------------------------------------
-module(e2ee_kt_merkle_tests).

-include_lib("eunit/include/eunit.hrl").

%% profile §8.2 的三条测试事件（canonical bytes）
-define(E1, <<
    "curve25519_key=Y3VydmUx\ndevice_id=dev-A\ned25519_key=ZWQyNTUxOTE=\n"
    "event_type=publish\nuser_id=1001"
>>).
-define(E2, <<
    "curve25519_key=Y3VydmUy\ndevice_id=dev-B\ned25519_key=ZWQyNTUxOTI=\n"
    "event_type=publish\nuser_id=1001"
>>).
-define(E3, <<"curve25519_key=\ndevice_id=dev-A\ned25519_key=\nevent_type=revoke\nuser_id=1001">>).

hex(B) -> string:lowercase(binary_to_list(binary:encode_hex(B))).

%%%===================================================================
%%% 1. 对照组 —— 两重外部自校验
%%%===================================================================

%% profile §8.1 明写：空树根等于公认的 SHA-256("")。若这条红，
%% 该实现连标准 SHA-256 都不对，后面所有向量都不必看。
empty_tree_is_sha256_of_empty_test() ->
    ?assertEqual(
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        hex(e2ee_kt_merkle:mth([]))
    ),
    ?assertEqual(hex(crypto:hash(sha256, <<>>)), hex(e2ee_kt_merkle:mth([]))).

%% RFC 6962 的定义要求：单叶树的根就是那片叶子的 hash（第二重自校验）
single_leaf_root_equals_leaf_hash_test() ->
    ?assertEqual(hex(e2ee_kt_merkle:leaf_hash(?E1)), hex(e2ee_kt_merkle:mth([?E1]))).

%%%===================================================================
%%% 2. profile §8 golden vectors 钉死
%%%===================================================================

%% 长度也是向量的一部分：长度对不上说明编码规则理解错了，不必再比 hash
e1_canonical_length_test() ->
    ?assertEqual(96, byte_size(?E1)).

golden_leaf_hashes_test() ->
    ?assertEqual(
        "de22f9f514db9c0faa1c57e53668678cfbf93f3b166eafd1212e254368e602e1",
        hex(e2ee_kt_merkle:leaf_hash(?E1))
    ),
    ?assertEqual(
        "d067ead2437f484a7413470f3a1b68c2cc4be8b5f2ec42ae7e7943a0ae33d012",
        hex(e2ee_kt_merkle:leaf_hash(?E2))
    ),
    ?assertEqual(
        "f571dce4078d4163a60dfeb441c23b09e1fefcdb862efecf4c8ebc7ccc2c6cab",
        hex(e2ee_kt_merkle:leaf_hash(?E3))
    ).

%% n=3 是刻意选的最小非平衡树（k=2：左 2 叶、右 1 叶）——
%% 只用 n=1/2/4 的向量无法区分「RFC 6962 的分裂规则」与「朴素两两配对」。
golden_tree_roots_test() ->
    ?assertEqual(
        "de22f9f514db9c0faa1c57e53668678cfbf93f3b166eafd1212e254368e602e1",
        hex(e2ee_kt_merkle:mth([?E1]))
    ),
    ?assertEqual(
        "bbd5b8a61334085b836b15c8aa421104b42d906b6bc8bd91da2b320a13a77ae0",
        hex(e2ee_kt_merkle:mth([?E1, ?E2]))
    ),
    ?assertEqual(
        "6beeef5d57749b14c1f9d4b090ebcc0eaa35422a7b19bdde36863dc8e3acb962",
        hex(e2ee_kt_merkle:mth([?E1, ?E2, ?E3]))
    ).

%% profile §8.3
golden_tree_head_signing_input_test() ->
    Root = hex(e2ee_kt_merkle:mth([?E1, ?E2, ?E3])),
    {ok, HeadBytes} = e2ee_kt_merkle:canonical_head_bytes(#{
        <<"domain">> => <<"imboy.kt.v1.tree_head">>,
        <<"log_id">> => <<"imboy-identity-log">>,
        <<"root_hash">> => list_to_binary(Root),
        <<"timestamp_ms">> => 1753747200000,
        <<"tree_size">> => 3
    }),
    ?assertEqual(168, byte_size(HeadBytes)),
    ?assertEqual(
        "34760542818964fc8f23ad1a09dca6c5a9d4388561cfa87ae8110e9c04cd1f3d",
        hex(e2ee_kt_merkle:tree_head_signing_input(HeadBytes))
    ).

%%%===================================================================
%%% 3. Canonical 编码（profile §3）
%%%===================================================================

canonical_event_bytes_matches_golden_test() ->
    {ok, B} = e2ee_kt_merkle:canonical_event_bytes(#{
        <<"user_id">> => 1001,
        <<"event_type">> => <<"publish">>,
        <<"ed25519_key">> => <<"ZWQyNTUxOTE=">>,
        <<"device_id">> => <<"dev-A">>,
        <<"curve25519_key">> => <<"Y3VydmUx">>
    }),
    %% 入参 map 的书写顺序刻意打乱：编码必须按 key 的 ASCII 字典序，与书写无关
    ?assertEqual(?E1, B).

canonical_event_bytes_no_trailing_newline_test() ->
    {ok, B} = e2ee_kt_merkle:canonical_event_bytes(#{<<"a">> => <<"1">>, <<"b">> => <<"2">>}),
    ?assertEqual(<<"a=1\nb=2">>, B).

%% fail-closed：value 内含换行会让编码非单射（同一串字节对应多组字段拆分）
canonical_rejects_newline_in_value_test() ->
    ?assertMatch(
        {error, {unsafe_field, _}},
        e2ee_kt_merkle:canonical_event_bytes(#{<<"device_id">> => <<"a\nb=c">>})
    ),
    ?assertMatch(
        {error, {unsafe_field, _}},
        e2ee_kt_merkle:canonical_event_bytes(#{<<"device_id">> => <<"a\rb">>})
    ).

canonical_rejects_newline_or_eq_in_key_test() ->
    ?assertMatch(
        {error, {unsafe_field, _}},
        e2ee_kt_merkle:canonical_event_bytes(#{<<"a\nb">> => <<"1">>})
    ),
    ?assertMatch(
        {error, {unsafe_field, _}},
        e2ee_kt_merkle:canonical_event_bytes(#{<<"a=b">> => <<"1">>})
    ).

%% 具体的注入形态：不带守卫时下面两组会编成同一串字节
canonical_injectivity_test() ->
    A = e2ee_kt_merkle:canonical_event_bytes(#{<<"a">> => <<"1\nb=2">>}),
    ?assertMatch({error, _}, A),
    {ok, B} = e2ee_kt_merkle:canonical_event_bytes(#{<<"a">> => <<"1">>, <<"b">> => <<"2">>}),
    ?assertEqual(<<"a=1\nb=2">>, B).

canonical_rejects_empty_and_non_map_test() ->
    ?assertEqual({error, empty_field_set}, e2ee_kt_merkle:canonical_event_bytes(#{})),
    ?assertEqual({error, not_a_map}, e2ee_kt_merkle:canonical_event_bytes(<<"x">>)).

%%%===================================================================
%%% 4. Domain separation（profile §2 的安全理由）
%%%===================================================================

%% 若 leaf 与 node 共用前缀，攻击者可把一个内部节点的两个子哈希拼成一条
%% 「事件」，使 leaf(x) == node(a,b)，对同一 root 构造出两棵不同的树。
leaf_and_node_are_domain_separated_test() ->
    L = crypto:hash(sha256, <<"l">>),
    R = crypto:hash(sha256, <<"r">>),
    Node = e2ee_kt_merkle:node_hash(L, R),
    %% 把两个子哈希原样拼成一条 64 字节的「事件」
    Forged = e2ee_kt_merkle:leaf_hash(<<L/binary, R/binary>>),
    ?assertNotEqual(Node, Forged).

head_prefix_differs_from_leaf_test() ->
    B = <<"whatever">>,
    ?assertNotEqual(e2ee_kt_merkle:tree_head_signing_input(B), e2ee_kt_merkle:leaf_hash(B)).

%%%===================================================================
%%% 5. Inclusion proof —— 穷举交叉核验
%%%===================================================================

ev(I) -> <<"curve25519_key=k", (integer_to_binary(I))/binary, "\nuser_id=1001">>.

events(N) -> [ev(I) || I <- lists:seq(1, N)].

%% 正向可用性：n=1..16 的**每一片叶子**都必须能被验通过。
%% 恒 false 的 verify 在这条上必红。
inclusion_exhaustive_accepts_test() ->
    [
        begin
            Ds = events(N),
            Root = e2ee_kt_merkle:mth(Ds),
            Path = e2ee_kt_merkle:inclusion_path(M, Ds),
            Leaf = e2ee_kt_merkle:leaf_hash(lists:nth(M + 1, Ds)),
            ?assert(e2ee_kt_merkle:verify_inclusion(Leaf, M, N, Path, Root))
        end
     || N <- lists:seq(1, 16), M <- lists:seq(0, N - 1)
    ].

%% 负向：同一 (N,M) 上换叶子 / 换 index / 换 root / 动 path 一律拒收
inclusion_exhaustive_rejects_test() ->
    [
        begin
            Ds = events(N),
            Root = e2ee_kt_merkle:mth(Ds),
            Path = e2ee_kt_merkle:inclusion_path(M, Ds),
            Leaf = e2ee_kt_merkle:leaf_hash(lists:nth(M + 1, Ds)),
            Other = e2ee_kt_merkle:leaf_hash(<<"not-in-tree">>),
            BadRoot = crypto:hash(sha256, <<"bad">>),
            %% 叶子不在树里
            ?assertNot(e2ee_kt_merkle:verify_inclusion(Other, M, N, Path, Root)),
            %% root 不对
            ?assertNot(e2ee_kt_merkle:verify_inclusion(Leaf, M, N, Path, BadRoot)),
            %% path 多一段 / 少一段
            ?assertNot(
                e2ee_kt_merkle:verify_inclusion(Leaf, M, N, Path ++ [BadRoot], Root)
            ),
            Path =/= [] andalso
                ?assertNot(
                    e2ee_kt_merkle:verify_inclusion(Leaf, M, N, tl(Path), Root)
                ),
            %% index 挪一位（N>1 时必然指向别的叶子）
            N > 1 andalso
                ?assertNot(
                    e2ee_kt_merkle:verify_inclusion(Leaf, (M + 1) rem N, N, Path, Root)
                )
        end
     || N <- lists:seq(1, 16), M <- lists:seq(0, N - 1)
    ].

%% path 里任一段被换掉都必须拒收（逐段扫，不是只动第一段）
inclusion_rejects_each_tampered_path_element_test() ->
    N = 11,
    Ds = events(N),
    Root = e2ee_kt_merkle:mth(Ds),
    Bad = crypto:hash(sha256, <<"tampered">>),
    [
        begin
            Path = e2ee_kt_merkle:inclusion_path(M, Ds),
            Leaf = e2ee_kt_merkle:leaf_hash(lists:nth(M + 1, Ds)),
            [
                ?assertNot(
                    e2ee_kt_merkle:verify_inclusion(
                        Leaf, M, N, replace_at(Path, I, Bad), Root
                    )
                )
             || I <- lists:seq(1, length(Path))
            ]
        end
     || M <- lists:seq(0, N - 1)
    ].

replace_at(L, I, V) ->
    lists:sublist(L, I - 1) ++ [V] ++ lists:nthtail(I, L).

inclusion_rejects_malformed_args_test() ->
    Ds = events(3),
    Root = e2ee_kt_merkle:mth(Ds),
    Leaf = e2ee_kt_merkle:leaf_hash(hd(Ds)),
    Path = e2ee_kt_merkle:inclusion_path(0, Ds),
    %% index 越界
    ?assertNot(e2ee_kt_merkle:verify_inclusion(Leaf, 3, 3, Path, Root)),
    %% 空树
    ?assertNot(e2ee_kt_merkle:verify_inclusion(Leaf, 0, 0, Path, Root)),
    %% 哈希长度不对
    ?assertNot(e2ee_kt_merkle:verify_inclusion(<<1, 2, 3>>, 0, 3, Path, Root)),
    ?assertNot(e2ee_kt_merkle:verify_inclusion(Leaf, 0, 3, Path, <<1, 2, 3>>)),
    ?assertNot(e2ee_kt_merkle:verify_inclusion(Leaf, 0, 3, [<<1, 2, 3>>], Root)).

%%%===================================================================
%%% 6. Consistency proof —— 穷举交叉核验
%%%===================================================================

%% 正向可用性：全部 0<m<=n<=16 的 (m,n) 组合都必须验通过
consistency_exhaustive_accepts_test() ->
    [
        begin
            Ds = events(N),
            Old = lists:sublist(Ds, M),
            R1 = e2ee_kt_merkle:mth(Old),
            R2 = e2ee_kt_merkle:mth(Ds),
            Path = e2ee_kt_merkle:consistency_path(M, Ds),
            ?assert(e2ee_kt_merkle:verify_consistency(M, N, Path, R1, R2))
        end
     || N <- lists:seq(1, 16), M <- lists:seq(1, N)
    ].

%% ⚠️ 这才是 KT 真正要防的：split view / 回滚。
%% 「旧根不是新树的前缀」必须拒收——否则日志可以悄悄改写历史。
consistency_rejects_forked_history_test() ->
    N = 9,
    Ds = events(N),
    R2 = e2ee_kt_merkle:mth(Ds),
    [
        begin
            Path = e2ee_kt_merkle:consistency_path(M, Ds),
            Old = lists:sublist(Ds, M),
            R1 = e2ee_kt_merkle:mth(Old),
            %% 分叉：把旧树的最后一片叶子换掉，再拿原 proof 去验
            Forked = lists:sublist(Old, M - 1) ++ [<<"forged-event">>],
            RForked = e2ee_kt_merkle:mth(Forked),
            ?assertNot(e2ee_kt_merkle:verify_consistency(M, N, Path, RForked, R2)),
            %% 新根被换掉
            ?assertNot(
                e2ee_kt_merkle:verify_consistency(
                    M, N, Path, R1, crypto:hash(sha256, <<"other-root">>)
                )
            )
        end
     || M <- lists:seq(1, N - 1)
    ].

consistency_rejects_each_tampered_path_element_test() ->
    N = 13,
    Ds = events(N),
    R2 = e2ee_kt_merkle:mth(Ds),
    Bad = crypto:hash(sha256, <<"tampered">>),
    [
        begin
            Path = e2ee_kt_merkle:consistency_path(M, Ds),
            R1 = e2ee_kt_merkle:mth(lists:sublist(Ds, M)),
            [
                ?assertNot(
                    e2ee_kt_merkle:verify_consistency(
                        M, N, replace_at(Path, I, Bad), R1, R2
                    )
                )
             || I <- lists:seq(1, length(Path))
            ]
        end
     || M <- lists:seq(1, N - 1)
    ].

%% 同一棵树：proof 必须为空且两根相同——空 proof 不得成为万能钥匙
consistency_same_size_requires_empty_path_and_equal_roots_test() ->
    Ds = events(5),
    R = e2ee_kt_merkle:mth(Ds),
    Other = crypto:hash(sha256, <<"other">>),
    ?assert(e2ee_kt_merkle:verify_consistency(5, 5, [], R, R)),
    ?assertNot(e2ee_kt_merkle:verify_consistency(5, 5, [], R, Other)),
    ?assertNot(e2ee_kt_merkle:verify_consistency(5, 5, [Other], R, R)).

%% 空 proof 不能在 m<n 时蒙混过关
consistency_rejects_empty_path_when_growing_test() ->
    Ds = events(7),
    R1 = e2ee_kt_merkle:mth(lists:sublist(Ds, 3)),
    R2 = e2ee_kt_merkle:mth(Ds),
    ?assertNot(e2ee_kt_merkle:verify_consistency(3, 7, [], R1, R2)).

consistency_rejects_malformed_args_test() ->
    Ds = events(4),
    R1 = e2ee_kt_merkle:mth(lists:sublist(Ds, 2)),
    R2 = e2ee_kt_merkle:mth(Ds),
    Path = e2ee_kt_merkle:consistency_path(2, Ds),
    %% 旧树比新树大（回滚）
    ?assertNot(e2ee_kt_merkle:verify_consistency(4, 2, Path, R2, R1)),
    %% m = 0
    ?assertNot(e2ee_kt_merkle:verify_consistency(0, 4, Path, R1, R2)),
    %% 哈希长度不对
    ?assertNot(e2ee_kt_merkle:verify_consistency(2, 4, Path, <<1, 2>>, R2)),
    ?assertNot(e2ee_kt_merkle:verify_consistency(2, 4, [<<1, 2>>], R1, R2)).
