-module(moya_org_settings_repo).
%%%
% 机构设置（organization.settings jsonb）读写 —— 墨芽教学域。
% Org settings repository (organization.settings jsonb)
%
% 存储决策：不新建表/列。organization.settings 是迁移 00000095 就绪的 jsonb
% 列（COMMENT「机构设置 jsonb：仅存必要字段」），机构级开关放这里与「机构级
% 配置」语义天然对齐，也避免为单键开列。
%
% 本模块只暴露教学域需要的键：ai_assist_enabled（AI 辅助批改开关）。
%   - 缺省即**开启**：AI 初评是本产品的默认价值，机构显式关掉才为 false。
%     因此 键缺失 / NULL / 脏值（非布尔）一律按 true——这是**产品默认**，
%     不是「读失败兜底」；读不到机构行本身仍 fail closed（{error, not_found}），
%     由调用方决定「读不到就不调 AI」（moya_ai_worker 与入队门都这么用）。
%   - 写入用 jsonb_set 只动该键，绝不整表覆盖 settings（保护其它键与
%     未来新增键；机构设置是多方共享的一份 jsonb，不能当私有白板用）。
%%%

-export([
    org_row_tx/2,
    ai_assist_enabled_tx/2,
    ai_assist_enabled_of/1,
    set_ai_assist_enabled_tx/3
]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% @doc 读机构原始行（事务内）：#{<<"name">> => binary(), <<"settings">> => map()}。
%% 机构不存在（id 查无）→ {error, not_found}，由调用方映射为 404 / 门控关闭。
-spec org_row_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
org_row_tx(Conn, OrgId) when is_integer(OrgId) ->
    Sql =
        <<"SELECT name, settings FROM ", (elib_pg_sql:public_tablename(<<"organization">>))/binary,
            " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId]) of
        {ok, [Row | _]} ->
            {ok, Row#{<<"settings">> => to_map(maps:get(<<"settings">>, Row, null))}};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("teaching_org_settings org_row db error ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 读 AI 辅助开关（事务内）。缺省 true（见模块头）。
-spec ai_assist_enabled_tx(any(), integer()) -> {ok, boolean()} | {error, not_found | term()}.
ai_assist_enabled_tx(Conn, OrgId) ->
    case org_row_tx(Conn, OrgId) of
        {ok, Row} ->
            {ok, ai_assist_enabled_of(Row)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 从 org_row_tx 的行取开关（Logic 层回读 payload 用）。纯函数、无 DB。
-spec ai_assist_enabled_of(map()) -> boolean().
ai_assist_enabled_of(Row) when is_map(Row) ->
    ai_assist_from(maps:get(<<"settings">>, Row, #{}));
ai_assist_enabled_of(_Other) ->
    true.

%% @doc 写 AI 辅助开关（事务内）。只 jsonb_set 该键；机构不存在 → not_found。
-spec set_ai_assist_enabled_tx(any(), integer(), boolean()) ->
    ok | {error, not_found | term()}.
set_ai_assist_enabled_tx(Conn, OrgId, Enabled) when is_integer(OrgId), is_boolean(Enabled) ->
    Sql =
        <<"UPDATE ", (elib_pg_sql:public_tablename(<<"organization">>))/binary,
            " SET settings = jsonb_set("
            "       COALESCE(settings, '{}'::jsonb), '{ai_assist_enabled}', $2::jsonb, true"
            "     ) "
            " WHERE id = $1">>,
    case elib_pg:execute(Conn, Sql, [OrgId, bool_json(Enabled)]) of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("teaching_org_settings write db error ~p", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 开关解析：只有**显式** false 才是关闭。键缺失 / null / 脏值 → true（产品默认）。
%% 兼容字符串形态（"false"）的原因：settings 是自由 jsonb，历史/人工写入可能是
%% 字符串而非布尔；把它当「关闭」比当「开启」更符合机构意图。
-spec ai_assist_from(map()) -> boolean().
ai_assist_from(Settings) when is_map(Settings) ->
    case maps:get(<<"ai_assist_enabled">>, Settings, undefined) of
        false -> false;
        <<"false">> -> false;
        _ -> true
    end;
ai_assist_from(_Other) ->
    true.

-spec bool_json(boolean()) -> binary().
bool_json(true) -> <<"true">>;
bool_json(false) -> <<"false">>.

%% jsonb 读回形态归一：当前 codec 返回 binary（列原生文本），jsone 解一层；
%% 若未来 codec 改为直接解码 map 也照样吃下（防御，不依赖 codec 细节）。
-spec to_map(null | binary() | map() | term()) -> map().
to_map(null) ->
    #{};
to_map(Bin) when is_binary(Bin) ->
    try jsone:decode(Bin) of
        M when is_map(M) -> M;
        _ -> #{}
    catch
        _:_ -> #{}
    end;
to_map(M) when is_map(M) ->
    M;
to_map(_Other) ->
    #{}.
