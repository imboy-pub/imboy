-module(ai_agent_repo).

%%%
% AI Agent 元数据仓库 / AI Agent metadata repository
% 表 ai_agent（见 priv/migrations/00000027_ai_agent）：user_id 主键 = agent 的 user.id
% 所有 SQL 经 elib_pg 参数化；trigger_policy 为 jsonb（写入用 $N::jsonb cast）。
%%%

-export([tablename/0]).
-export([upsert/1]).
-export([find/1]).
-export([active_ids/0]).
-export([set_status/2]).
-export([page/2]).
-export([page_assistants/3]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"ai_agent">>).

%% @doc 创建或更新 agent 绑定（按 user_id upsert）
%% Data 键：user_id(必填), provider(必填), model, role_id, system_prompt,
%%          owner_uid, trigger_policy(已编码 JSON binary), status,
%%          description(专属简介), visibility(0=私有 1=公开可发现)
-spec upsert(map()) -> {ok, [map()]} | {error, term()}.
upsert(#{user_id := UserId, provider := Provider} = Data) ->
    Tb = tablename(),
    Model = maps:get(model, Data, <<>>),
    RoleId = maps:get(role_id, Data, <<>>),
    SystemPrompt = maps:get(system_prompt, Data, <<>>),
    OwnerUid = maps:get(owner_uid, Data, 0),
    TriggerJson = maps:get(trigger_policy, Data, <<"{}">>),
    Status = maps:get(status, Data, 1),
    Description = maps:get(description, Data, <<>>),
    Visibility = maps:get(visibility, Data, 0),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (user_id, provider, model, role_id, system_prompt, owner_uid,"
            "  trigger_policy, status, description, visibility, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7::jsonb,$8,$9,$10,NOW(),NOW())"
            " ON CONFLICT (user_id) DO UPDATE SET"
            " provider = EXCLUDED.provider, model = EXCLUDED.model,"
            " role_id = EXCLUDED.role_id, system_prompt = EXCLUDED.system_prompt,"
            " owner_uid = EXCLUDED.owner_uid, trigger_policy = EXCLUDED.trigger_policy,"
            " status = EXCLUDED.status, description = EXCLUDED.description,"
            " visibility = EXCLUDED.visibility, updated_at = NOW()"
            " RETURNING user_id">>,
    case
        elib_pg:query(Sql, [
            UserId,
            Provider,
            Model,
            RoleId,
            SystemPrompt,
            OwnerUid,
            TriggerJson,
            Status,
            Description,
            Visibility
        ])
    of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?ERROR_LOG("ai_agent_repo:upsert user_id=~p error ~p~n", [UserId, Reason]),
            {error, Reason}
    end.

%% @doc 按 user_id 查单个 agent 元数据行
-spec find(integer()) -> {ok, map()} | {error, notfound | term()}.
find(UserId) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT user_id, provider, model, role_id, system_prompt, owner_uid,"
            " trigger_policy, status, description, visibility FROM ",
            Tb/binary,
            " WHERE user_id = $1"
        >>,
    case elib_pg:query(Sql, [UserId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 列出所有启用中 agent 的 user_id（presence runtime 上线用）
-spec active_ids() -> {ok, [integer()]} | {error, term()}.
active_ids() ->
    Tb = tablename(),
    Sql = <<"SELECT user_id FROM ", Tb/binary, " WHERE status = 1">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} -> {ok, [maps:get(<<"user_id">>, R) || R <- Rows]};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 启用/停用 agent（status 1=启用 0=停用）
-spec set_status(integer(), 0 | 1) -> {ok, non_neg_integer()} | {error, term()}.
set_status(UserId, Status) ->
    Tb = tablename(),
    elib_pg:update(Tb, #{status => Status, updated_at => elib_dt:now()}, <<"user_id = $1">>, [
        UserId
    ]).

%% @doc 分页列出 agent（管理后台）
-spec page(pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
%% @doc admin 管理列表：JOIN user 拿 nickname/avatar，补 visibility/description，
%% 便于后台识别与编辑（不含 system_prompt 长文本，编辑走 find/1 详情）。
%% ponytail: LIMIT/OFFSET 整数来自已校验分页（Page/Size>0），内联安全。
page(Page, Size) when Page > 0, Size > 0 ->
    ATb = tablename(),
    UTb = user_repo:tablename(),
    From = <<" FROM ", ATb/binary, " a JOIN ", UTb/binary, " u ON u.id = a.user_id ">>,
    case elib_pg:query(<<"SELECT count(*) AS total", From/binary>>, []) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql =
                <<
                    "SELECT a.user_id, u.nickname, u.avatar, a.provider, a.model,"
                    " a.description, a.visibility, a.status, a.owner_uid, a.created_at",
                    From/binary,
                    " ORDER BY a.created_at DESC LIMIT ",
                    (integer_to_binary(Size))/binary,
                    " OFFSET ",
                    (integer_to_binary(Offset))/binary
                >>,
            case elib_pg:query(ListSql, []) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_repo:page list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("ai_agent_repo:page count error ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 面向普通用户的「可发现助手」分页列表（供 Flutter 发起 C2S 会话）。
%% 可见性口径：ai_agent.status=1（启用）AND visibility=1（公开可发现）
%%   AND user.status=1（账号正常）。visibility 独立列（迁移 000031），公开即可
%%   被发现，不再绑死 owner_uid=0；官方助手已 backfill visibility=1。
%% 卡片字段：name/avatar JOIN user 表；description 取 ai_agent.description 真实列。
%% ponytail: 内联 LIMIT/OFFSET（整数来自已校验分页），keyword 走参数化 $1 防注入。
-spec page_assistants(binary(), pos_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
page_assistants(Keyword, Page, Size) when Page > 0, Size > 0 ->
    ATb = tablename(),
    UTb = user_repo:tablename(),
    {KwWhere, Params} =
        case Keyword of
            <<>> ->
                {<<>>, []};
            _ ->
                Like = <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>,
                {<<" AND u.nickname ILIKE $1">>, [Like]}
        end,
    From =
        <<" FROM ", ATb/binary, " a JOIN ", UTb/binary, " u ON u.id = a.user_id ">>,
    Where =
        <<"WHERE a.status = 1 AND a.visibility = 1 AND u.status = 1", KwWhere/binary>>,
    CountSql = <<"SELECT count(*) AS total", From/binary, Where/binary>>,
    case elib_pg:query(CountSql, Params) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql =
                <<"SELECT a.user_id, u.nickname, u.avatar, a.description", From/binary,
                    Where/binary, " ORDER BY a.created_at DESC LIMIT ",
                    (integer_to_binary(Size))/binary, " OFFSET ",
                    (integer_to_binary(Offset))/binary>>,
            case elib_pg:query(ListSql, Params) of
                {ok, Rows} ->
                    {ok, #{total => Total, page => Page, size => Size, list => Rows}};
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_repo:page_assistants list error ~p~n", [Reason]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, empty_page(Page, Size)};
        {error, Reason} ->
            ?ERROR_LOG("ai_agent_repo:page_assistants count error ~p~n", [Reason]),
            {error, Reason}
    end.

-spec empty_page(pos_integer(), pos_integer()) -> map().
empty_page(Page, Size) ->
    #{total => 0, page => Page, size => Size, list => []}.
