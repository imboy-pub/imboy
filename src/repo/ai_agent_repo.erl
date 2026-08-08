-module(ai_agent_repo).

%%%
% AI Agent 元数据仓库 / AI Agent metadata repository
% 表 ai_agent（见 priv/migrations/00000027_ai_agent）：user_id 主键 = agent 的 user.id
% 所有 SQL 经 elib_pg 参数化；trigger_policy 为 jsonb（写入用 $N::jsonb cast）。
%%%

-export([tablename/0]).
-export([upsert/1]).
-export([patch/2]).
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
%%          description(专属简介), visibility(0=私有 1=公开可发现),
%%          category, voice_id, greeting, capabilities(已编码 JSON binary),
%%          temperature（迁移 000057 扩展列）
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
    Category = maps:get(category, Data, <<>>),
    VoiceId = maps:get(voice_id, Data, <<>>),
    Greeting = maps:get(greeting, Data, <<>>),
    Capabilities = maps:get(capabilities, Data, <<"{}">>),
    Temperature = maps:get(temperature, Data, 0.7),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (user_id, provider, model, role_id, system_prompt, owner_uid,"
            "  trigger_policy, status, description, visibility, category, voice_id,"
            "  greeting, capabilities, temperature, created_at, updated_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7::jsonb,$8,$9,$10,$11,$12,$13,$14::jsonb,$15,NOW(),NOW())"
            " ON CONFLICT (user_id) DO UPDATE SET"
            " provider = EXCLUDED.provider, model = EXCLUDED.model,"
            " role_id = EXCLUDED.role_id, system_prompt = EXCLUDED.system_prompt,"
            " owner_uid = EXCLUDED.owner_uid, trigger_policy = EXCLUDED.trigger_policy,"
            " status = EXCLUDED.status, description = EXCLUDED.description,"
            " visibility = EXCLUDED.visibility, category = EXCLUDED.category,"
            " voice_id = EXCLUDED.voice_id, greeting = EXCLUDED.greeting,"
            " capabilities = EXCLUDED.capabilities, temperature = EXCLUDED.temperature,"
            " updated_at = NOW()"
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
            Visibility,
            Category,
            VoiceId,
            Greeting,
            Capabilities,
            Temperature
        ])
    of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?ERROR_LOG("ai_agent_repo:upsert user_id=~p error ~p~n", [UserId, Reason]),
            {error, Reason}
    end.

%% @doc 部分更新既有 agent；未提交的行为字段保持原值，避免管理端编辑资料时清空兼容配置。
-spec patch(integer(), map()) -> {ok, [map()]} | {error, term()}.
patch(UserId, Data) when is_integer(UserId), is_map(Data) ->
    Tb = tablename(),
    Fields = patch_fields(Data),
    case Fields of
        [] ->
            {ok, []};
        _ ->
            Assignments = [
                patch_assignment(Column, Cast, Index)
             || {{Column, _Value, Cast}, Index} <- lists:zip(
                    Fields, lists:seq(1, length(Fields))
                )
            ],
            WhereIndex = length(Fields) + 1,
            Sql = iolist_to_binary([
                <<"UPDATE ">>,
                Tb,
                <<" SET ">>,
                join_binary(Assignments),
                <<" WHERE user_id = $">>,
                integer_to_binary(WhereIndex),
                <<" RETURNING user_id">>
            ]),
            Params = [Value || {_Column, Value, _Cast} <- Fields] ++ [UserId],
            case elib_pg:query(Sql, Params) of
                {ok, Rows} ->
                    {ok, Rows};
                {error, Reason} ->
                    ?ERROR_LOG("ai_agent_repo:patch user_id=~p error ~p~n", [UserId, Reason]),
                    {error, Reason}
            end
    end.

patch_fields(Data) ->
    [
        {Column, Value, Cast}
     || {Key, Column, Cast} <- [
            {provider, <<"provider">>, <<>>},
            {model, <<"model">>, <<>>},
            {role_id, <<"role_id">>, <<>>},
            {system_prompt, <<"system_prompt">>, <<>>},
            {owner_uid, <<"owner_uid">>, <<>>},
            {trigger_policy, <<"trigger_policy">>, <<"::jsonb">>},
            {status, <<"status">>, <<>>},
            {description, <<"description">>, <<>>},
            {visibility, <<"visibility">>, <<>>},
            {category, <<"category">>, <<>>},
            {voice_id, <<"voice_id">>, <<>>},
            {greeting, <<"greeting">>, <<>>},
            {capabilities, <<"capabilities">>, <<"::jsonb">>},
            {temperature, <<"temperature">>, <<>>}
        ],
        {ok, Value} <- [maps:find(Key, Data)]
    ].

patch_assignment(Column, Cast, Index) ->
    iolist_to_binary([
        Column,
        <<" = $">>,
        integer_to_binary(Index),
        Cast
    ]).

join_binary([]) ->
    <<>>;
join_binary([First | Rest]) ->
    lists:foldl(
        fun(Item, Acc) -> <<Acc/binary, ", ", Item/binary>> end,
        First,
        Rest
    ).

%% @doc 按 user_id 查单个 agent 元数据行
-spec find(integer()) -> {ok, map()} | {error, notfound | term()}.
find(UserId) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT user_id, provider, model, role_id, system_prompt, owner_uid,"
            " trigger_policy, status, description, visibility, category, voice_id,"
            " greeting, capabilities, temperature FROM ",
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
page(Page, Size) ->
    page(Page, Size, <<>>).

%% @doc 分页列出 agent（管理后台，可选分类筛选）
%% @doc admin 管理列表：JOIN user 拿 nickname/avatar，补 visibility/description，
%% 便于后台识别与编辑（不含 system_prompt 长文本，编辑走 find/1 详情）。
%% category 为空时回退全量（不带 WHERE）；非空时 WHERE a.category = $1 参数化。
%% ponytail: LIMIT/OFFSET 整数来自已校验分页（Page/Size>0），内联安全。
%% 上限：安全性实际由 integer_to_binary/1 兜底——非整数直接 badarg 崩在拼串前，
%%   不存在可注入的字符串路径；代价是该保证只覆盖这两个位置。
%% 升级触发：无升级路径（设计约束，非延期）——只要 LIMIT/OFFSET 仍经
%%   integer_to_binary/1 拼接，注入面恒为零；反之若改成直传 binary/字符串，
%%   就不再是简化而是缺陷，必须换回参数化。
page(Page, Size, Category) when Page > 0, Size > 0 ->
    ATb = tablename(),
    UTb = user_repo:tablename(),
    RoleTb = elib_pg_sql:public_tablename(<<"ai_agent_role">>),
    From =
        <<" FROM ", ATb/binary, " a JOIN ", UTb/binary, " u ON u.id = a.user_id", " LEFT JOIN ",
            RoleTb/binary, " r ON r.code = a.role_id ">>,
    {Where, Params} =
        case Category of
            <<>> -> {<<>>, []};
            _ -> {<<" WHERE a.category = $1">>, [Category]}
        end,
    case elib_pg:query(<<"SELECT count(*) AS total", From/binary, Where/binary>>, Params) of
        {ok, [#{<<"total">> := 0} | _]} ->
            {ok, empty_page(Page, Size)};
        {ok, [#{<<"total">> := Total} | _]} ->
            Offset = (Page - 1) * Size,
            ListSql =
                <<
                    "SELECT a.user_id, u.nickname, u.avatar, a.provider, a.model,"
                    " a.description, a.visibility, a.status, a.owner_uid, a.category,"
                    " a.role_id, r.name AS role_name, r.active_version AS role_version,"
                    " a.created_at",
                    From/binary,
                    Where/binary,
                    " ORDER BY a.created_at DESC LIMIT ",
                    (integer_to_binary(Size))/binary,
                    " OFFSET ",
                    (integer_to_binary(Offset))/binary
                >>,
            case elib_pg:query(ListSql, Params) of
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
%% 上限：同 page/2——integer_to_binary/1 是类型闸门，非整数在拼串前就 badarg；
%%   Keyword 经 elib_pg:escape_like/1 + $1，不进 SQL 文本。
%% 升级触发：无升级路径（设计约束，非延期）——除非有人把 Size/Offset 改成直传
%%   binary 或把 Keyword 挪出参数位，那属于缺陷回归而非升级。
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
