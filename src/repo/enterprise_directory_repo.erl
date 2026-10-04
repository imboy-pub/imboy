-module(enterprise_directory_repo).

-moduledoc "受限 cursor directory 的 tx 仓储层。".
%%%
% enterprise_directory_repo 是**受限 cursor directory** 的 tx 仓储层
% （FULL-02 / plan-full §3.1「外部身份 … 受限 cursor directory，不允许无界导出」、
% §4 `/identity-mappings` cursor list 与 `/directory/users`）。
%
%% 硬边界（本模块的**唯一**读取面形态）：
%   * 每条查询都带 `ORDER BY <key> LIMIT $N`——**不存在**无 LIMIT、无游标、
%     OFFSET 翻页或 COUNT 导出的函数。keyset pagination（WHERE key > cursor）
%     而非 OFFSET：OFFSET 在大表上等价于全量扫描，且游标不可复现。
%   * 每页最多 ?MAX_PAGE 行由 logic 层强制（本层只接受已校验的 Limit）；
%     多取一行（Limit+1）用于判定 has_more，调用方不得把它当数据返回。
%   * 只返回**最小字段**（id 与状态），不含姓名/手机号/邮箱/头像等 PII。
%   * 全部查询按 (organization_id, application_id) 复合过滤：跨 Org/跨 App
%     的键在 SQL 内即不可见（IDOR 的 DB 层兜底）。
%%%

-export([
    page_mappings_tx/5,
    page_members_tx/5,
    page_members_in_workspace_tx/6,
    max_page/0
]).

%% 每页硬上限（logic 层超出即拒，绝不静默截断）。
-define(MAX_PAGE, 100).

%% 取行数上限（+1 供 has_more 判定；调用方不得把它当数据返回）。
-spec clamp(pos_integer()) -> pos_integer().
clamp(Limit) when is_integer(Limit), Limit > 0 ->
    min(Limit, ?MAX_PAGE + 1);
clamp(_Limit) ->
    ?MAX_PAGE + 1.

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 每页硬上限（本层独立于 logic 的第二道闸：即使调用方传了超大 Limit，
%% 本层也只取 ?MAX_PAGE + 1 行——「无界导出」在任何调用路径上都不可能）。
-spec max_page() -> pos_integer().
max_page() ->
    ?MAX_PAGE.

%% @doc 映射目录一页（keyset：external_user_id 严格递增）。
%% 只取 active 行与最小字段（external_user_id / user_id / status）。
%% AfterExt 为上一页最后一个 external_user_id（首页传 undefined）。
-spec page_mappings_tx(any(), integer(), integer(), undefined | binary(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
page_mappings_tx(Conn, OrgId, AppId, AfterExt, Limit0) when is_integer(Limit0), Limit0 > 0 ->
    Limit = clamp(Limit0),
    Base = <<
        "SELECT external_user_id, user_id, status FROM enterprise_external_identity"
        " WHERE organization_id = $1 AND application_id = $2 AND status = 'active'"
    >>,
    {Sql, Params} =
        case AfterExt of
            undefined ->
                {<<Base/binary, " ORDER BY external_user_id LIMIT $3">>, [OrgId, AppId, Limit]};
            Ext when is_binary(Ext) ->
                {
                    <<Base/binary,
                        " AND external_user_id > $3 ORDER BY external_user_id LIMIT $4">>,
                    [OrgId, AppId, Ext, Limit]
                }
        end,
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 成员目录一页（keyset：user_id 严格递增）：本 Org 的 **active Human**
%% 成员（organization_member.status='active' 且 user.account_type=0 且
%% user.status=1），LEFT JOIN 本 (org, app) 的 active 映射给出 external_user_id
%% （未映射为 NULL——目录只回答「可寻址与否」，不泄露其他 App 的映射）。
%% 最小字段：user_id / external_user_id / member_role / member_status。
%% 首页/续页用两条静态 SQL（不用 `$N IS NULL OR ...` 形态：epgsql 对
%% 「同一参数既当类型标注又当比较项」会落 indeterminate_datatype 42P18）。
-spec page_members_tx(any(), integer(), integer(), undefined | integer(), pos_integer()) ->
    {ok, [map()]} | {error, term()}.
page_members_tx(Conn, OrgId, AppId, AfterUid, Limit0) when is_integer(Limit0), Limit0 > 0 ->
    Limit = clamp(Limit0),
    Base = <<
        "SELECT om.user_id, om.role AS member_role, om.status AS member_status,"
        " m.external_user_id"
        " FROM organization_member om"
        " JOIN \"user\" u ON u.id = om.user_id"
        " LEFT JOIN enterprise_external_identity m"
        "   ON m.organization_id = $1 AND m.application_id = $2"
        "  AND m.user_id = om.user_id AND m.status = 'active'"
        " WHERE om.organization_id = $1 AND om.status = 'active'"
        "   AND u.account_type = 0 AND u.status = 1"
    >>,
    {Sql, Params} =
        case AfterUid of
            undefined ->
                {<<Base/binary, " ORDER BY om.user_id LIMIT $3">>, [OrgId, AppId, Limit]};
            Uid when is_integer(Uid) ->
                {<<Base/binary, " AND om.user_id > $3 ORDER BY om.user_id LIMIT $4">>, [
                    OrgId, AppId, Uid, Limit
                ]}
        end,
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 同 page_members_tx/5，但限定为该 Org 内某 Workspace 的 active 成员
%% （Workspace Grant 过滤：workspace 必须属于本 Org，在 SQL 内以
%% w.organization_id = $1 复合条件强制——跨 Org workspace 一行都不返回）。
%% 同样用两条静态 SQL（见 page_members_tx/5 的 42P18 说明）。
-spec page_members_in_workspace_tx(
    any(), integer(), integer(), integer(), undefined | integer(), pos_integer()
) ->
    {ok, [map()]} | {error, term()}.
page_members_in_workspace_tx(Conn, OrgId, AppId, WsId, AfterUid, Limit0) when
    is_integer(Limit0), Limit0 > 0
->
    Limit = clamp(Limit0),
    Base = <<
        "SELECT om.user_id, om.role AS member_role, om.status AS member_status,"
        " m.external_user_id"
        " FROM organization_member om"
        " JOIN \"user\" u ON u.id = om.user_id"
        " JOIN workspace w ON w.id = $3 AND w.organization_id = $1"
        " JOIN workspace_member wm ON wm.workspace_id = w.id"
        "   AND wm.user_id = om.user_id AND wm.status = 'active'"
        " LEFT JOIN enterprise_external_identity m"
        "   ON m.organization_id = $1 AND m.application_id = $2"
        "  AND m.user_id = om.user_id AND m.status = 'active'"
        " WHERE om.organization_id = $1 AND om.status = 'active'"
        "   AND u.account_type = 0 AND u.status = 1"
    >>,
    {Sql, Params} =
        case AfterUid of
            undefined ->
                {<<Base/binary, " ORDER BY om.user_id LIMIT $4">>, [OrgId, AppId, WsId, Limit]};
            Uid when is_integer(Uid) ->
                {<<Base/binary, " AND om.user_id > $4 ORDER BY om.user_id LIMIT $5">>, [
                    OrgId, AppId, WsId, Uid, Limit
                ]}
        end,
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
