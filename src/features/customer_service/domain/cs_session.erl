%%% @doc 客服会话（customer_service_session）的领域纯函数。
%%%
%%% 依据：plan v4.1 §4.2（customer_service_session）、§5.2、EB-D03/EB-D05/EB-D10、
%%% CS-01-A02/A04/A05。
%%%
%%% 纯净性（铁律 4）：无 I/O、无进程、无隐式时间/随机源；时间（`at`/`Now`）、
%%% ID、actor 一律由调用方作为参数传入。所有函数可零 mock 单测
%%% （`test/features/customer_service/domain/cs_session_tests.erl`）。
%%%
%%% 冻结的不变量：
%%%   * 状态机 V1：queued → active（claim）、active → closed（close）、
%%%     queued → closed（排队取消）；closed 是终态，任何再迁移一律拒绝；
%%%   * 同一会话的并发 claim 恰好一个成功：由 DB 侧 (status, version) 条件
%%%     UPDATE 裁决，本模块的 `assert_cas_expectation/3` 是期望形状的唯一判定源；
%%%   * session 绑定 business_identity_id（不是 user_id）：transfer/rebind 只换
%%%     identity，org/workspace/contact/conversation/id 零迁移（A04 连续性）；
%%%   * rating 1..5、只允许写在 closed 会话、不可重复评；
%%%   * 访客域（A05）：visit token 只允许作用于其绑定的 (Org, contact)，
%%%     且吊销/过期即失效——token 单独不能换 member/seat 权限。
-module(cs_session).

-export([
    statuses/0,
    valid_transition/2,
    transition/3,
    assert_cas_expectation/3,
    rate/3,
    transfer/3,
    assert_rebind_continuity/2,
    assert_visitor_scope/3,
    assert_visitor_scope/4
]).

-type status() :: queued | active | closed.
-type session() :: map().
-type visit_token() :: #{
    organization_id := integer(),
    contact_id := integer(),
    expires_at := integer() | undefined,
    revoked_at := integer() | undefined
}.

-export_type([status/0, session/0]).

%% @doc V1 冻结的状态值域。
-spec statuses() -> [status()].
statuses() ->
    [queued, active, closed].

%% ===================================================================
%% 状态迁移判定（纯判定，不改数据）
%% ===================================================================

%% @doc 判定 (From, To) 是否是合法迁移。
%%
%%   queued -> active   ok（claim）
%%   queued -> closed   ok（排队取消）
%%   active -> closed   ok（close）
%%   active -> active   {error, {not_claimable, active}}（并发第二人）
%%   其余一律            {error, {invalid_transition, From, To}}
%%   closed -> 任意      {error, session_already_closed}（终态，重复 close 负例）
-spec valid_transition(status() | undefined, status()) -> ok | {error, term()}.
valid_transition(closed, _To) ->
    {error, session_already_closed};
valid_transition(queued, active) ->
    ok;
valid_transition(queued, closed) ->
    ok;
valid_transition(active, closed) ->
    ok;
valid_transition(active, active) ->
    {error, {not_claimable, active}};
valid_transition(From, To) ->
    {error, {invalid_transition, From, To}}.

%% ===================================================================
%% 迁移执行（纯函数：旧 map → 新 map）
%% ===================================================================

%% @doc 执行一次状态迁移，返回推进后的会话（version + 1）。
%%
%% Ctx：
%%   * 迁到 `active`：必含 `business_identity_id`（目标坐席）与 `at`（Unix 秒）；
%%   * 迁到 `closed`：必含 `at`，可选 `reason`（close_reason 审计文本）；
%%   * 迁到 `queued`：无额外键。
-spec transition(session(), status(), map()) -> {ok, session()} | {error, term()}.
transition(Session, To, Ctx) when is_map(Session), is_map(Ctx) ->
    From = maps:get(status, Session, undefined),
    case valid_transition(From, To) of
        ok -> apply_transition(Session, From, To, Ctx);
        {error, _} = Err -> Err
    end;
transition(_Session, _To, _Ctx) ->
    {error, {invalid_argument, transition}}.

apply_transition(Session, _From, active, Ctx) ->
    IdentityId = maps:get(business_identity_id, Ctx, undefined),
    At = maps:get(at, Ctx, undefined),
    case is_pos_int(IdentityId) andalso is_pos_int(At) of
        false ->
            {error, {invalid_argument, claim_context}};
        true ->
            {ok, Session#{
                status := active,
                business_identity_id := IdentityId,
                claimed_at := At,
                version := maps:get(version, Session, 1) + 1,
                updated_at := At
            }}
    end;
apply_transition(Session, _From, closed, Ctx) ->
    At = maps:get(at, Ctx, undefined),
    case is_pos_int(At) of
        false ->
            {error, {invalid_argument, close_context}};
        true ->
            {ok, Session#{
                status := closed,
                closed_at := At,
                close_reason := maps:get(reason, Ctx, undefined),
                version := maps:get(version, Session, 1) + 1,
                updated_at := At
            }}
    end;
apply_transition(Session, _From, queued, Ctx) ->
    At = maps:get(at, Ctx, undefined),
    {ok, Session#{version := maps:get(version, Session, 1) + 1, updated_at := At, queued_at => At}}.

%% ===================================================================
%% CAS 期望（A02 的 domain 判定真源）
%% ===================================================================

%% @doc 断言会话处于期望的 (status, version) 形状——DB 侧条件 UPDATE 之前后
%% 共用同一判定。不匹配时返回 `{error, {cas_mismatch, Detail}}`，其中 Detail
%% 同时携带期望值与实际值（便于并发失败方拿到可区分结论）。
-spec assert_cas_expectation(session(), status(), integer()) -> ok | {error, term()}.
assert_cas_expectation(Session, ExpectedStatus, ExpectedVersion) ->
    ActualStatus = maps:get(status, Session, undefined),
    ActualVersion = maps:get(version, Session, undefined),
    case ActualStatus =:= ExpectedStatus andalso ActualVersion =:= ExpectedVersion of
        true ->
            ok;
        false ->
            {error,
                {cas_mismatch, #{
                    expected_status => ExpectedStatus,
                    expected_version => ExpectedVersion,
                    actual_status => ActualStatus,
                    actual_version => ActualVersion
                }}}
    end.

%% ===================================================================
%% rating
%% ===================================================================

%% @doc 给 closed 会话打分（1..5）。评分与评分时间同一次写入，version + 1；
%% 重复评分一律 `{error, already_rated}`（评分不可改写，与审计一致）。
-spec rate(session(), integer(), integer()) -> {ok, session()} | {error, term()}.
rate(Session, Rating, At) when is_map(Session) ->
    case is_valid_rating(Rating) of
        false ->
            {error, {invalid_rating, Rating}};
        true ->
            case maps:get(status, Session, undefined) of
                closed ->
                    case maps:get(rating, Session, undefined) of
                        undefined ->
                            {ok, Session#{
                                rating := Rating,
                                rating_at := At,
                                version := maps:get(version, Session, 1) + 1,
                                updated_at := At
                            }};
                        _Existing ->
                            {error, already_rated}
                    end;
                Status ->
                    {error, {rating_requires_closed, Status}}
            end
    end;
rate(_Session, _Rating, _At) ->
    {error, {invalid_argument, rate}}.

is_valid_rating(R) when is_integer(R), R >= 1, R =< 5 -> true;
is_valid_rating(_) -> false.

%% ===================================================================
%% transfer（改绑经办 identity；主体字段零迁移）
%% ===================================================================

%% @doc 把会话改绑给另一个坐席 identity（owner/主体字段不变）。
%% closed 会话不可 transfer；queued 会话没有当前经办，也无处可转。
-spec transfer(session(), integer(), integer()) -> {ok, session()} | {error, term()}.
transfer(Session, ToIdentityId, At) when is_map(Session) ->
    case is_pos_int(ToIdentityId) andalso is_pos_int(At) of
        false ->
            {error, {invalid_argument, transfer}};
        true ->
            case maps:get(status, Session, undefined) of
                active ->
                    {ok, Session#{
                        business_identity_id := ToIdentityId,
                        version := maps:get(version, Session, 1) + 1,
                        updated_at := At
                    }};
                closed ->
                    {error, session_already_closed};
                Status ->
                    {error, {not_transferable, Status}}
            end
    end;
transfer(_Session, _ToIdentityId, _At) ->
    {error, {invalid_argument, transfer}}.

%% ===================================================================
%% A04：rebind 连续性判定
%% ===================================================================

%% @doc 断言 rebind（identity 换人）前后会话的**主体字段**完全一致：
%% id / organization_id / workspace_id / contact_id / conversation_id。
%% 这是「数据不动、历史连续」的 domain 判定真源；任何字段漂移都点名返回。
-spec assert_rebind_continuity(session(), session()) -> ok | {error, term()}.
assert_rebind_continuity(Before, After) when is_map(Before), is_map(After) ->
    continuity(Before, After, [id, organization_id, workspace_id, contact_id, conversation_id]);
assert_rebind_continuity(_Before, _After) ->
    {error, {invalid_argument, assert_rebind_continuity}}.

continuity(_Before, _After, []) ->
    ok;
continuity(Before, After, [Field | Rest]) ->
    case maps:get(Field, Before, undefined) =:= maps:get(Field, After, undefined) of
        true -> continuity(Before, After, Rest);
        false -> {error, {continuity_broken, Field}}
    end.

%% ===================================================================
%% A05：访客域判定（visit token ≠ member/seat 凭证）
%% ===================================================================

%% @doc 三参形态：无时钟上下文（如只有 digest 命中行），仍强制归属与吊销检查。
-spec assert_visitor_scope(visit_token(), integer(), integer()) -> ok | {error, term()}.
assert_visitor_scope(Token, OrgId, ContactId) ->
    assert_visitor_scope(Token, OrgId, ContactId, undefined).

%% @doc 判定访客 token 是否允许作用于 (OrgId, ContactId)。
%%
%% 判定顺序固定（失败原因唯一可复现）：
%%   1. 跨 Org → `{error, cross_org}`；
%%   2. contact 不符 → `{error, contact_mismatch}`；
%%   3. 已吊销（`Now` 未提供时：只要存在 revoked_at 即已吊销）→ `{error, token_revoked}`；
%%   4. 已过期（仅当提供 `Now`）→ `{error, token_expired}`。
%%
%% 判定**只**回答「能否以 cs_visit 身份作用于绑定的 Org+contact」；不授予任何
%% member / seat 能力（EB-D10）。
-spec assert_visitor_scope(visit_token(), integer(), integer(), integer() | undefined) ->
    ok | {error, term()}.
assert_visitor_scope(Token, OrgId, ContactId, Now) when is_map(Token) ->
    case maps:get(organization_id, Token, undefined) =:= OrgId of
        false ->
            {error, cross_org};
        true ->
            case maps:get(contact_id, Token, undefined) =:= ContactId of
                false ->
                    {error, contact_mismatch};
                true ->
                    revoked_at(
                        Now,
                        maps:get(revoked_at, Token, undefined),
                        maps:get(expires_at, Token, undefined)
                    )
            end
    end;
assert_visitor_scope(_Token, _OrgId, _ContactId, _Now) ->
    {error, {invalid_argument, assert_visitor_scope}}.

revoked_at(Now, RevokedAt, _ExpiresAt) when is_integer(RevokedAt) ->
    case Now of
        undefined -> {error, token_revoked};
        N when is_integer(N), N >= RevokedAt -> {error, token_revoked};
        _ -> ok
    end;
revoked_at(Now, _RevokedAt, ExpiresAt) ->
    case {Now, is_integer(ExpiresAt)} of
        {undefined, _} -> ok;
        {N, true} when is_integer(N), N >= ExpiresAt -> {error, token_expired};
        _ -> ok
    end.

is_pos_int(V) when is_integer(V), V > 0 -> true;
is_pos_int(_) -> false.
