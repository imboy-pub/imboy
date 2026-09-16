%%% @doc 离职交接用例层的**纯函数**部分（无 I/O、无进程、无隐式时间/随机源）。
%%%
%%% 依据：plan v4.1 EB-08（S1 suspend+freeze / S2 transfer+CAS / S3 verify+finalize）、
%%% `docs/architecture/feature-slice-rules.md` 铁律 4（domain 纯净）与铁律 7（CAS）。
%%%
%%% 为什么单独成模块：`eb_offboarding_app` 负责「参数收敛 → 端口读写 → 审计」；
%%% 本模块只放**可离线判定**的判定与派生：快照指纹、项计数、经办人裁决、失败原因
%%% 归一。它们被三个 Gate 共享，避免把判定散落进 I/O 分支里。
%%%
%%% 状态机的语义唯一真源仍是 domain `eb_offboarding`（transition/2、rebind/3、
%%% resume_after_failure/1）；本模块不重造状态机，只做「把库里的行整理成 domain
%%% 能消费的形状」与「从行集合里读出结论」。
-module(eb_offboarding_flow).

-export([
    idempotency_key/2,
    frozen_tuple/1,
    snapshot_hash/1,
    recomputed_key_matches/1,
    item_counts/1,
    pending_items/1,
    item_reasons/3,
    active_assignment/2,
    assignee_of/1,
    reason_binary/1
]).

-type item() :: map().
-type assignment() :: map().

%% ===================================================================
%% 快照：幂等键 / 冻结字段 / 指纹
%% ===================================================================

%% @doc 交接项的幂等键：由 (case_id, business_identity_id) **确定性**派生。
%%
%% 两个用途：
%%   1. `uq_eoi_org_idempotency_key` 让同 (Org, 键) 的重放不增行；
%%   2. S3 的防篡改判据——`business_identity_id` 一旦被改写，重算的键就对不上。
%% 键里不含 user：交接参与人不进幂等键（换人不产生第二条事实）。
-spec idempotency_key(integer(), integer()) -> binary().
idempotency_key(CaseId, IdentityId) ->
    <<"offboarding:", (integer_to_binary(CaseId))/binary, ":",
        (integer_to_binary(IdentityId))/binary>>.

%% @doc 冻结字段：S1 落库后**逐字不变**，S2/S3 只允许改 status/attempt/failure_reason。
-spec frozen_tuple(item()) -> tuple().
frozen_tuple(Item) ->
    {
        maps:get(case_id, Item, undefined),
        maps:get(business_identity_id, Item, undefined),
        maps:get(function_key, Item, undefined),
        maps:get(from_user_id, Item, undefined),
        maps:get(to_user_id, Item, undefined),
        maps:get(idempotency_key, Item, undefined)
    }.

%% @doc 快照指纹：对冻结字段集合排序后取 sha256（hex）。
%%
%% 同一集合 ⇒ 同一指纹（与行顺序无关）；任一冻结字段被改写 ⇒ 指纹必变。
-spec snapshot_hash([item()]) -> binary().
snapshot_hash(Items) ->
    Sorted = lists:sort([frozen_tuple(I) || I <- Items]),
    hex(crypto:hash(sha256, term_to_binary(Sorted))).

%% @doc 防篡改：重算幂等键并与落库值比对（返回不一致的项）。
-spec recomputed_key_matches([item()]) -> {ok, [item()]} | {mismatch, [item()]}.
recomputed_key_matches(Items) ->
    Bad = [
        I
     || I <- Items,
        idempotency_key(maps:get(case_id, I, 0), maps:get(business_identity_id, I, 0)) =/=
            maps:get(idempotency_key, I, undefined)
    ],
    case Bad of
        [] -> {ok, Items};
        _ -> {mismatch, Bad}
    end.

%% ===================================================================
%% 项计数
%% ===================================================================

%% @doc 项计数（case 表的 item_* 三列在冻结契约里**没有**写入口，故一律从行集合派生）。
-spec item_counts([item()]) -> map().
item_counts(Items) ->
    #{
        total => length(Items),
        success => length([I || I <- Items, status_of(I) =:= success]),
        failed => length([I || I <- Items, status_of(I) =:= failed]),
        pending => length([I || I <- Items, status_of(I) =:= pending])
    }.

%% @doc 仍需处理的项（pending / failed）——S2 的批量 rebind 输入。
-spec pending_items([item()]) -> [item()].
pending_items(Items) ->
    [I || I <- Items, status_of(I) =/= success].

status_of(Item) ->
    case maps:get(status, Item, undefined) of
        S when is_atom(S) -> S;
        _ -> undefined
    end.

%% ===================================================================
%% 项与 case 的一致性（S3 的校验输入）
%% ===================================================================

%% @doc 项的 from/to 必须与 case 的 leaver/successor 逐字一致。
%%
%% 判据来自两处**独立**事实（项行 vs case 行）：任一侧被改写都会让它们对不上；
%% 配合 `recomputed_key_matches/1`（identity_id ↔ 幂等键）与
%% `snapshot_hash/1`（整集合指纹），快照的防篡改是三重且各自独立的。
-spec item_reasons(item(), integer(), integer()) -> [{atom(), term()}].
item_reasons(Item, Leaver, Successor) ->
    From = maps:get(from_user_id, Item, undefined),
    To = maps:get(to_user_id, Item, undefined),
    FromReason =
        case From =:= Leaver of
            true -> [];
            false -> [{leaver_mismatch, Leaver, From}]
        end,
    ToReason =
        case To =:= Successor of
            true -> [];
            false -> [{successor_mismatch, Successor, To}]
        end,
    FromReason ++ ToReason.

%% ===================================================================
%% 经办人裁决
%% ===================================================================

%% @doc 某 identity 当前唯一的 active 经办行。
%%
%% 返回 `{ok, Row}` / `none` / `{error, {multiple_active, N}}`：DB 有
%% `uq_obia_active_identity` 保证唯一，出现多行属脏数据 ⇒ fail-closed（不猜第一条）。
-spec active_assignment(integer(), [assignment()]) -> {ok, assignment()} | none | {error, term()}.
active_assignment(IdentityId, Assignments) ->
    Active = [
        A
     || A <- Assignments,
        maps:get(business_identity_id, A, undefined) =:= IdentityId,
        maps:get(status, A, undefined) =:= active
    ],
    case Active of
        [] -> none;
        [Row] -> {ok, Row};
        Multiple -> {error, {multiple_active_assignment, IdentityId, length(Multiple)}}
    end.

%% @doc 经办行的 assignee（`undefined` 视为尚未绑定）。
-spec assignee_of(assignment()) -> integer() | undefined.
assignee_of(Assignment) ->
    maps:get(user_id, Assignment, undefined).

%% ===================================================================
%% 失败原因归一
%% ===================================================================

%% @doc 把任意错误项压成可落库的二进制（列 `failure_reason` 是 text）。
%%
%% 不落正文/密文/凭据：只落错误项的形状（它来自本用例自己的裁决，不含调用方数据）。
-spec reason_binary(term()) -> binary().
reason_binary(Reason) when is_binary(Reason) ->
    Reason;
reason_binary(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
reason_binary(Reason) when is_list(Reason) ->
    iolist_to_binary([safe_term_to_binary(Item) || Item <- Reason]);
reason_binary(Reason) ->
    safe_term_to_binary(Reason).

safe_term_to_binary(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true -> unicode:characters_to_binary(Term);
        false -> iolist_to_binary(io_lib:format("~0p", [Term]))
    end.

hex(Bin) ->
    <<<<(nibble(N))>> || <<N:4>> <= Bin>>.

nibble(N) when N < 10 -> $0 + N;
nibble(N) -> $a + N - 10.
