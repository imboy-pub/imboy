-module(talk_record_speaker_repo).
-moduledoc "面聊说话人仓储（T15：清单/人工绑定 CAS/认领/目录校验；迁移 00000211）。".
%%%
% talk_record_speaker 数据仓库层（T15 发言人域）。
%
% 契约（§2/§4/§11.3）：
%   * identity_source ∈ anonymous/self_claim/manual/voice_suggestion*；
%     自动建议不得直接变本人确认（本卡只产生 anonymous/manual/self_claim）；
%   * PATCH（人工标记）由服务端写 manual + confirmed_by；认领写 self_claim；
%     客户端不可指定 identity_source（logic 层拒收该字段）；
%   * 不暴露 voice embedding（V1 无该数据；表内亦无向量列）；
%   * 目录校验（matched_user_id 目标）：复用 T05 creation-contexts 的
%     组织/工作区可见性谓词形态——org 记录要求目标为同 org active 成员
%     且（绑定 ws 时）有 ws 资源授权（organization_resource_access_repo
%     统一谓词）；personal 记录仅允许有效本人或双向有效好友。
%%%

-export([
    list_by_record_tx/2,
    find_tx/3,
    update_tx/5,
    claim_tx/4,
    target_in_directory_tx/3, target_in_directory_tx/4,
    participant_exists_tx/3
]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SPEAKER_COLUMNS,
    <<"sp.id,sp.record_id,sp.local_label,sp.display_name,sp.matched_user_id,",
        "sp.identity_source,sp.confirmed_by,sp.confidence,sp.revision,",
        "sp.created_at,sp.updated_at">>
).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 记录全部说话人（local_label/id 正序；§4 清单端点，量小不分页，
%% 上限 200 防御性钳制）。
-spec list_by_record_tx(any(), integer()) -> {ok, [map()]} | {error, term()}.
list_by_record_tx(Conn, RecordId) ->
    Sql =
        <<"SELECT ", ?SPEAKER_COLUMNS/binary, " FROM talk_record_speaker sp",
            " WHERE sp.record_id = $1 ORDER BY sp.local_label ASC, sp.id ASC LIMIT 200">>,
    elib_pg:query(Conn, Sql, [RecordId]).

%% @doc 按复合主键取 speaker（record_id + speaker_id 同时匹配，§8；
%% 跨记录子 ID → not_found → 404）。
-spec find_tx(any(), integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, RecordId, SpeakerId) ->
    Sql =
        <<"SELECT ", ?SPEAKER_COLUMNS/binary, " FROM talk_record_speaker sp",
            " WHERE sp.record_id = $1 AND sp.id = $2">>,
    case elib_pg:query(Conn, Sql, [RecordId, SpeakerId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc speaker 编辑 CAS（display_name / matched_user_id 绑定列由 logic
% 组装；revision+1）。命中 0 行由上层区分 not_found / precondition_failed。
%% Sets 约定：动态列（display_name/matched_user_id/identity_source/
%% confirmed_by）+ actor_id/updated_at 由本函数补齐。
-spec update_tx(any(), integer(), integer(), pos_integer(), map()) ->
    {ok, map()} | {error, not_found | term()}.
update_tx(Conn, RecordId, SpeakerId, ExpectedRevision, Sets) ->
    Editable = [
        <<"display_name">>, <<"matched_user_id">>, <<"identity_source">>, <<"confirmed_by">>
    ],
    Sets1 = [{K, maps:get(K, Sets)} || K <- Editable, maps:is_key(K, Sets)],
    SetSql = join_set_clause(Sets1, 1),
    Base = length(Sets1) + 1,
    Sql =
        <<"UPDATE talk_record_speaker sp SET ", SetSql/binary, ", revision = revision + 1",
            ", updated_at = $", (integer_to_binary(Base))/binary, " WHERE sp.record_id = $",
            (integer_to_binary(Base + 1))/binary, " AND sp.id = $",
            (integer_to_binary(Base + 2))/binary, " AND sp.revision = $",
            (integer_to_binary(Base + 3))/binary, " RETURNING ", ?SPEAKER_COLUMNS/binary>>,
    Params =
        [V || {_, V} <- Sets1] ++
            [elib_dt:now(), RecordId, SpeakerId, ExpectedRevision],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 本人认领（§4 claim）：matched_user_id IS NULL 或已等于本人时写入
%% self_claim；他人已绑定 → 0 行（上层回读判 conflict/not_found）。
%% 不接受 If-Match（认领为幂等动作，冲突以 409 语义仲裁，见 logic 注释）。
-spec claim_tx(any(), integer(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
claim_tx(Conn, RecordId, SpeakerId, Uid) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE talk_record_speaker sp",
            " SET matched_user_id = $1, identity_source = 'self_claim',",
            " confirmed_by = $1, revision = revision + 1, updated_at = $2",
            " WHERE sp.record_id = $3 AND sp.id = $4",
            " AND (sp.matched_user_id IS NULL OR sp.matched_user_id = $1)", " RETURNING ",
            ?SPEAKER_COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [Uid, Now, RecordId, SpeakerId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc matched_user_id 目录校验（§4 PATCH speakers：目标须符合可见目录）。
%% RecordRow 为 find_visible 的详情行（scope/organization_id/workspace_id）。
%% org：目标须为同 org active 成员，且记录绑定 ws 时有 ws 资源授权
%% （ws_member active 或 org owner/admin —— 复用统一谓词）；personal：目标
%% 必须为当前操作者有效本人或双向有效好友。
%% 参数：$1=目标 uid、$2=org id、$3=record id（ws 守卫引用记录行）。
-spec target_in_directory_tx(any(), map(), integer()) -> {ok, boolean()} | {error, term()}.
target_in_directory_tx(Conn, Row, TargetUid) ->
    target_in_directory_tx(Conn, Row, maps:get(<<"owner_user_id">>, Row, 0), TargetUid).

target_in_directory_tx(
    Conn,
    #{<<"scope">> := <<"organization">>, <<"organization_id">> := OrgId} = Row,
    _ActorUid,
    TargetUid
) when
    is_integer(OrgId)
->
    {WsGuard, WsParams} =
        case maps:get(<<"workspace_id">>, Row, null) of
            null ->
                {<<"true">>, []};
            WsId when is_integer(WsId) ->
                WsPred = organization_resource_access_repo:workspace_predicate_sql(
                    <<"tr.workspace_id">>, <<"$1">>
                ),
                {
                    <<"EXISTS (SELECT 1 FROM talk_record tr WHERE tr.id = $3",
                        " AND tr.workspace_id = ", (integer_to_binary(WsId))/binary, " AND ",
                        WsPred/binary, ")">>,
                    [maps:get(<<"id">>, Row)]
                }
        end,
    Sql =
        <<"SELECT 1 WHERE EXISTS (SELECT 1 FROM organization o",
            " WHERE o.id = $2 AND o.status = 'active')",
            " AND EXISTS (SELECT 1 FROM organization_member om",
            " WHERE om.organization_id = $2 AND om.user_id = $1", " AND om.status = 'active')",
            " AND EXISTS (SELECT 1 FROM \"user\" u WHERE u.id=$1 AND u.status=1 AND u.account_type=0)",
            " AND (", WsGuard/binary, ")">>,
    %% 参数与占位符严格一一对应（epgsql 按语句参数数 zip；无 ws 守卫时
    %% $3 不出现，多传参数即 function_clause）
    case elib_pg:query(Conn, Sql, [TargetUid, OrgId] ++ WsParams) of
        {ok, [_ | _]} -> {ok, true};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end;
target_in_directory_tx(Conn, #{<<"scope">> := <<"personal">>}, ActorUid, TargetUid) ->
    talk_record_directory_repo:personal_target_tx(Conn, ActorUid, TargetUid).

%% @doc 现场参与者判定（§4 claim：有记录访问权的现场参与者才可认领）。
%% 参与者 ≠ 授权 ≠ 已确认身份（§11.3），此处只判参与者名单事实。
-spec participant_exists_tx(any(), integer(), integer()) -> {ok, boolean()} | {error, term()}.
participant_exists_tx(Conn, RecordId, Uid) ->
    Sql =
        <<"SELECT 1 FROM talk_record_participant p",
            " WHERE p.record_id = $1 AND p.user_id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [RecordId, Uid]) of
        {ok, [_ | _]} -> {ok, true};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec join_set_clause([{binary(), term()}], pos_integer()) -> binary().
join_set_clause(Sets, Start) ->
    {Parts, _N} =
        lists:mapfoldl(
            fun({Col, _V}, N) ->
                {<<Col/binary, " = $", (integer_to_binary(N))/binary>>, N + 1}
            end,
            Start,
            Sets
        ),
    iolist_to_binary(join_binary(Parts, <<", ">>)).

join_binary([], _Sep) ->
    [];
join_binary([P], _Sep) ->
    [P];
join_binary([P | Rest], Sep) ->
    [P, Sep | join_binary(Rest, Sep)].
