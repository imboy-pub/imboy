%%%-------------------------------------------------------------------
%%% E2EE-065 Slice 4 —— Key Transparency 的 Merkle 树与 proof（纯函数）
%%%
%%% 实现 `29-e2ee-065-transparency-profile-v1.md` 冻结的 RFC 6962 profile：
%%% canonical event bytes、leaf/node domain separation、MTH、
%%% inclusion / consistency proof 的**生成与验证**、tree head 签名输入。
%%%
%%% ⚠️ **实现 profile 不等于接受 profile**。`29-...` 仍是**未签字的冻结草案**，
%%% playbook 明写 profile 须由安全 reviewer 人工接受，loop 不得自我接受。
%%% 本模块的存在不构成对该 profile 的任何形式的批准。
%%%
%%% ⚠️ **纯函数，未接线**：不碰 DB、不碰 HTTP、不产生也不持有签名私钥。
%%% 日志写入、sequencer（Slice 1 已定案：leaf index 必须与 bigserial 解耦）、
%%% proof API 与 monitor 分别属其后各刀。
%%%
%%% 设计要点：
%%% - proof **生成**照 RFC 6962 §2.1.1/§2.1.2 的递归定义直译（PATH / SUBPROOF），
%%%   它是本模块里最接近规范原文、因而最可核对的一段；
%%% - proof **验证**用标准迭代算法（验证方拿不到整棵树）。两者的一致性
%%%   由测试**穷举** n≤16 的全部 (index, size) 与 (m, n) 组合交叉核验——
%%%   迭代算法的任何细微写错都会被生成侧照出来。
%%%-------------------------------------------------------------------
-module(e2ee_kt_merkle).

-export([
    canonical_event_bytes/1,
    canonical_head_bytes/1,
    leaf_hash/1,
    node_hash/2,
    mth/1,
    inclusion_path/2,
    verify_inclusion/5,
    consistency_path/2,
    verify_consistency/5,
    tree_head_signing_input/1
]).

%% profile §2/§4：domain separation 前缀
-define(LEAF_PREFIX, 16#00).
-define(NODE_PREFIX, 16#01).
-define(HEAD_PREFIX, 16#02).

%%%===================================================================
%%% Canonical bytes（profile §3，复用既有 trust event 方案）
%%%===================================================================

%% @doc 把字段 map 编成 canonical bytes：`key=value` 每字段一行，`\n` 分隔，
%% key 按 ASCII 字典序，**末字段无尾随换行**。
%%
%% fail-closed：任一 key/value 含 `\n` 或 `\r` 即拒绝编码——`key=value\n` 的
%% 分隔符唯一，value 内含换行会让编码**非单射**（同一串字节对应多组字段拆分），
%% 等价于签名伪造。key 含 `=` 同理拒绝。
-spec canonical_event_bytes(map()) -> {ok, binary()} | {error, term()}.
canonical_event_bytes(Fields) when is_map(Fields) ->
    encode_kv(Fields);
canonical_event_bytes(_) ->
    {error, not_a_map}.

%% @doc tree head 的 canonical bytes（profile §5）。与事件同一套编码规则。
-spec canonical_head_bytes(map()) -> {ok, binary()} | {error, term()}.
canonical_head_bytes(Fields) when is_map(Fields) ->
    encode_kv(Fields);
canonical_head_bytes(_) ->
    {error, not_a_map}.

encode_kv(Fields) when map_size(Fields) =:= 0 ->
    {error, empty_field_set};
encode_kv(Fields) ->
    Pairs = lists:keysort(1, [{to_bin(K), to_bin(V)} || {K, V} <- maps:to_list(Fields)]),
    case lists:filter(fun({K, V}) -> unsafe(K) orelse unsafe(V) orelse has_eq(K) end, Pairs) of
        [] ->
            Lines = [<<K/binary, "=", V/binary>> || {K, V} <- Pairs],
            {ok, iolist_to_binary(lists:join(<<"\n">>, Lines))};
        [{BadK, _} | _] ->
            {error, {unsafe_field, BadK}}
    end.

%% 整数按十进制渲染（对齐 e2ee_trust_logic:i2b/1）
to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_integer(V) -> integer_to_binary(V);
to_bin(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_bin(V) when is_list(V) -> unicode:characters_to_binary(V).

unsafe(B) ->
    binary:match(B, [<<"\n">>, <<"\r">>]) =/= nomatch.

has_eq(K) ->
    binary:match(K, [<<"=">>]) =/= nomatch.

%%%===================================================================
%%% Leaf / Node / MTH（profile §2、§4）
%%%===================================================================

-spec leaf_hash(binary()) -> binary().
leaf_hash(EventBytes) when is_binary(EventBytes) ->
    crypto:hash(sha256, <<?LEAF_PREFIX, EventBytes/binary>>).

-spec node_hash(binary(), binary()) -> binary().
node_hash(<<L:32/binary>>, <<R:32/binary>>) ->
    crypto:hash(sha256, <<?NODE_PREFIX, L/binary, R/binary>>).

%% @doc Merkle Tree Hash。入参是**事件 canonical bytes 的列表**（不是已 hash 的叶子）。
%%
%% MTH({})   = SHA-256(<<>>)
%% MTH({d0}) = leaf_hash(d0)
%% MTH(D[n]) = node_hash(MTH(D[0:k]), MTH(D[k:n]))，k = 小于 n 的最大 2 的幂
-spec mth([binary()]) -> binary().
mth([]) ->
    crypto:hash(sha256, <<>>);
mth([D0]) ->
    leaf_hash(D0);
mth(Ds) when is_list(Ds) ->
    K = largest_pow2_below(length(Ds)),
    {Left, Right} = lists:split(K, Ds),
    node_hash(mth(Left), mth(Right)).

%% 小于 N 的最大 2 的幂（N >= 2）
largest_pow2_below(N) when N >= 2 ->
    lp2(1, N).

lp2(K, N) when K * 2 < N -> lp2(K * 2, N);
lp2(K, _) -> K.

%%%===================================================================
%%% Inclusion proof（RFC 6962 §2.1.1）
%%%===================================================================

%% @doc PATH(m, D[n]) —— 第 M 片叶子（0-based）在 Ds 中的 audit path。
%%
%% PATH(0, {d0}) = {}
%% PATH(m, D[n]) = PATH(m, D[0:k])   ++ [MTH(D[k:n])]   若 m < k
%%               | PATH(m-k, D[k:n]) ++ [MTH(D[0:k])]   否则
-spec inclusion_path(non_neg_integer(), [binary()]) -> [binary()].
inclusion_path(0, [_]) ->
    [];
inclusion_path(M, Ds) when is_integer(M), M >= 0, M < length(Ds) ->
    N = length(Ds),
    K = largest_pow2_below(N),
    {Left, Right} = lists:split(K, Ds),
    case M < K of
        true -> inclusion_path(M, Left) ++ [mth(Right)];
        false -> inclusion_path(M - K, Right) ++ [mth(Left)]
    end.

%% @doc 验证 inclusion proof。验证方**没有整棵树**，只有叶子哈希与 path。
%%
%% 返回 boolean()：任何长度不符、path 过长/过短、root 不等一律 false，
%% 不区分失败原因（失败原因不应成为 oracle）。
-spec verify_inclusion(binary(), non_neg_integer(), pos_integer(), [binary()], binary()) ->
    boolean().
verify_inclusion(LeafHash, Index, TreeSize, Path, Root) when
    is_binary(LeafHash),
    byte_size(LeafHash) =:= 32,
    is_integer(Index),
    is_integer(TreeSize),
    is_list(Path),
    is_binary(Root),
    byte_size(Root) =:= 32,
    Index >= 0,
    TreeSize > 0,
    Index < TreeSize
->
    case lists:all(fun(P) -> is_binary(P) andalso byte_size(P) =:= 32 end, Path) of
        false ->
            false;
        true ->
            case walk_inclusion(Index, TreeSize - 1, LeafHash, Path) of
                {ok, 0, Computed} -> Computed =:= Root;
                _ -> false
            end
    end;
verify_inclusion(_, _, _, _, _) ->
    false.

walk_inclusion(_Fn, Sn, R, []) ->
    {ok, Sn, R};
walk_inclusion(_Fn, 0, _R, [_ | _]) ->
    %% path 比树高还长
    error;
walk_inclusion(Fn, Sn, R, [P | Rest]) ->
    case (Fn band 1 =:= 1) orelse (Fn =:= Sn) of
        true ->
            R1 = node_hash(P, R),
            {Fn1, Sn1} = shift_while_even(Fn, Sn),
            walk_inclusion(Fn1 bsr 1, Sn1 bsr 1, R1, Rest);
        false ->
            R1 = node_hash(R, P),
            walk_inclusion(Fn bsr 1, Sn bsr 1, R1, Rest)
    end.

shift_while_even(Fn, Sn) when Fn =/= 0, Fn band 1 =:= 0 ->
    shift_while_even(Fn bsr 1, Sn bsr 1);
shift_while_even(Fn, Sn) ->
    {Fn, Sn}.

%%%===================================================================
%%% Consistency proof（RFC 6962 §2.1.2）
%%%===================================================================

%% @doc PROOF(m, D[n]) —— 从大小 M 的旧树到大小 N 的新树的 consistency proof。
-spec consistency_path(pos_integer(), [binary()]) -> [binary()].
consistency_path(M, Ds) when is_integer(M), M > 0, M =< length(Ds) ->
    subproof(M, Ds, true).

%% SUBPROOF(m, D[m], true)  = {}
%% SUBPROOF(m, D[m], false) = { MTH(D[m]) }
%% SUBPROOF(m, D[n], b)     = SUBPROOF(m, D[0:k], b)     ++ [MTH(D[k:n])]  若 m <= k
%%                          | SUBPROOF(m-k, D[k:n], false) ++ [MTH(D[0:k])] 否则
subproof(M, Ds, true) when M =:= length(Ds) ->
    [];
subproof(M, Ds, false) when M =:= length(Ds) ->
    [mth(Ds)];
subproof(M, Ds, B) ->
    N = length(Ds),
    K = largest_pow2_below(N),
    {Left, Right} = lists:split(K, Ds),
    case M =< K of
        true -> subproof(M, Left, B) ++ [mth(Right)];
        false -> subproof(M - K, Right, false) ++ [mth(Left)]
    end.

%% @doc 验证 consistency proof：旧树（大小 M，根 Root1）是新树（大小 N，根 Root2）的前缀。
-spec verify_consistency(pos_integer(), pos_integer(), [binary()], binary(), binary()) ->
    boolean().
verify_consistency(M, N, Path, Root1, Root2) when
    is_integer(M),
    is_integer(N),
    is_list(Path),
    is_binary(Root1),
    byte_size(Root1) =:= 32,
    is_binary(Root2),
    byte_size(Root2) =:= 32,
    M > 0,
    M =< N
->
    case lists:all(fun(P) -> is_binary(P) andalso byte_size(P) =:= 32 end, Path) of
        false ->
            false;
        true when M =:= N ->
            %% 同一棵树：path 必须为空，且两个根必须相同
            Path =:= [] andalso Root1 =:= Root2;
        true ->
            verify_consistency_1(M, N, Path, Root1, Root2)
    end;
verify_consistency(_, _, _, _, _) ->
    false.

verify_consistency_1(M, N, Path, Root1, Root2) ->
    {Node0, Last0} = shift_while_odd(M - 1, N - 1),
    case {Node0, Path} of
        {_, []} ->
            false;
        {0, _} ->
            %% M 是 2 的幂：旧根本身就是子树根，不占 path 的位置
            walk_consistency(Node0, Last0, Root1, Root1, Path, Root1, Root2);
        {_, [P0 | Rest]} ->
            walk_consistency(Node0, Last0, P0, P0, Rest, Root1, Root2)
    end.

shift_while_odd(Node, Last) when Node band 1 =:= 1 ->
    shift_while_odd(Node bsr 1, Last bsr 1);
shift_while_odd(Node, Last) ->
    {Node, Last}.

walk_consistency(_Node, Last, Fr, Sr, [], Root1, Root2) ->
    Last =:= 0 andalso Fr =:= Root1 andalso Sr =:= Root2;
walk_consistency(_Node, 0, _Fr, _Sr, [_ | _], _Root1, _Root2) ->
    false;
walk_consistency(Node, Last, Fr, Sr, [P | Rest], Root1, Root2) ->
    %% ⚠️ 判据是「Node 为奇 **或** Node =:= Last」——写成 `Node < Last`
    %% 会在 Node =:= Last 时把一个**左兄弟**当成右兄弟处理，
    %% 旧根那一路（Fr）就永远不再更新。该错法在平衡树上看不出来，
    %% 只在 m=5,n=6 / m=9..11,n=10..12 / m=13,n=14 这类非平衡情形失败——
    %% 本刀正是被穷举交叉核验（n≤16 全部 (m,n)）抓到的。
    %% 命中该分支后必须「沿右脊爬升到下一个奇节点」，与 inclusion 同理。
    case (Node band 1 =:= 1) orelse (Node =:= Last) of
        true ->
            {Node1, Last1} = shift_while_even(Node, Last),
            walk_consistency(
                Node1 bsr 1,
                Last1 bsr 1,
                node_hash(P, Fr),
                node_hash(P, Sr),
                Rest,
                Root1,
                Root2
            );
        false ->
            walk_consistency(
                Node bsr 1, Last bsr 1, Fr, node_hash(Sr, P), Rest, Root1, Root2
            )
    end.

%%%===================================================================
%%% Tree head（profile §5）
%%%===================================================================

%% @doc signing_input = SHA-256(0x02 ‖ canonical_head_bytes)。
%%
%% `0x02` 前缀防止一条 tree-head 签名被当作 leaf 数据复用。
%% **本函数不签名、不接触私钥**（profile §7：私钥不在 DB/repo/日志/API）。
-spec tree_head_signing_input(binary()) -> binary().
tree_head_signing_input(HeadBytes) when is_binary(HeadBytes) ->
    crypto:hash(sha256, <<?HEAD_PREFIX, HeadBytes/binary>>).
