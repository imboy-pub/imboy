-module(workspace_ds).
-compile([nowarn_deprecated_catch]).
%%%
% workspace_ds 是 workspace domain service 缩写
% 工作区领域服务（双体验 v2.5.2 WP3/T4）
%
% 核心职责：
%   1. Template 原子初始化（I13）：单事务创建 Workspace + Owner workspace_member
%      + 默认 Channel（Announcements，scope=workspace）+ 默认 Group（General，
%      scope=workspace）+ 创建者 Channel Admin/Subscriber + Group Member 关系，
%      任一步失败全部回滚。
%   2. request_id 幂等：workspace 表无 request_id 列（T3 迁移未提供），
%      采用任务卡允许的"先查后插于同事务"：
%        a) 语义键幂等：同 Owner + 同名 active 工作区 → 直接返回既有资源
%           （镜像 group_ds:find_by_creator_and_sum/2 的既有幂等模式）；
%        b) request_id 幂等：request_id 写入 branding 的内部键 "_request_id"
%           （下划线前缀键不出现在 API 输出——branding 白名单只读
%           name/logo/primaryColor），同 Owner + 同 _request_id 命中 → 返回既有资源。
%      并发同请求竞态由语义键兜底（最终仍可能产生同名双工作区，但单请求重试
%      路径不产生重复资源；彻底唯一化需要后续迁移加唯一约束，移交记录）。
%   3. branding 白名单键治理：仅 name/logo/primaryColor 可读写。
%%%

-export([create_template/3]).
-export([find_by_id/1]).
-export([find_by_id/2]).
-export([page_by_member/3]).
-export([update_profile/3]).
-export([update_branding/3]).
-export([read_branding/1]).
-export([overview/2]).
-export([ws_transfer_tx/3]).
-export([branding_public_view/1]).
-export([resource_counts/1]).

-include("log.hrl").

%% Template 常量（计划 §1.4 Template 初始化）
-define(DEFAULT_CHANNEL_NAME, <<"Announcements">>).
-define(DEFAULT_GROUP_NAME, <<"General">>).
-define(DEFAULT_GROUP_TYPE, 2).
-define(MAX_WORKSPACES_PER_OWNER, 100).
-define(BRANDING_KEYS, [<<"name">>, <<"logo">>, <<"primaryColor">>]).
-define(MEMBER_PREVIEW_LIMIT, 20).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Template 原子初始化（I13）
%% 幂等命中时返回 {ok, Result, existing}；新建返回 {ok, Result, created}。
%% Result = #{workspace_id => Id, workspace => Row(public view),
%%            channel_id => CId, group_id => GId}
-spec create_template(integer(), binary(), binary() | undefined) ->
    {ok, map(), created | existing} | {error, term()}.
create_template(OwnerUid, Name, RequestId) ->
    case elib_pg:with_tx(fun(Conn) -> create_template_tx(Conn, OwnerUid, Name, RequestId) end) of
        {ok, Result} -> {ok, Result, created};
        {error, {idempotent_hit, Result}} -> {ok, Result, existing};
        {error, Reason} -> {error, Reason}
    end.

create_template_tx(Conn, OwnerUid, Name, RequestId) ->
    %% 幂等前置（同事务先查后插）：
    %% 1) request_id 精确命中（同 Owner）→ 返回既有
    case normalize_request_id(RequestId) of
        <<>> ->
            check_semantic_idempotent(Conn, OwnerUid, Name);
        RequestId2 ->
            case workspace_repo:find_by_request_id(OwnerUid, RequestId2, Conn) of
                WS when map_size(WS) > 0 ->
                    existing_workspace_result(Conn, WS);
                _ ->
                    check_semantic_idempotent(Conn, OwnerUid, Name, RequestId2)
            end
    end.

%% 无 request_id：仅语义键幂等
check_semantic_idempotent(Conn, OwnerUid, Name) ->
    check_semantic_idempotent(Conn, OwnerUid, Name, <<>>).

check_semantic_idempotent(Conn, OwnerUid, Name, RequestId) ->
    case workspace_repo:count_by_owner(OwnerUid) >= ?MAX_WORKSPACES_PER_OWNER of
        true ->
            throw({abort_tx, owner_workspace_limit});
        false ->
            case workspace_repo:find_by_owner_and_name(OwnerUid, Name, Conn) of
                WS when map_size(WS) > 0 ->
                    existing_workspace_result(Conn, WS);
                _ ->
                    do_create_template(Conn, OwnerUid, Name, RequestId)
            end
    end.

%% @doc 幂等命中：回读 Template 资源（默认 Channel/Group 缺失属历史数据异常，
%% 不在此补偿创建——Template 原子性由创建路径保证）。
existing_workspace_result(Conn, WS) ->
    WsId = maps:get(<<"id">>, WS),
    {ok, DefaultChannel} = default_channel_of(Conn, WsId),
    {ok, DefaultGroup} = default_group_of(Conn, WsId),
    throw(
        {abort_tx,
            {idempotent_hit, #{
                workspace_id => WsId,
                workspace => public_view(WS),
                channel_id => DefaultChannel,
                group_id => DefaultGroup
            }}}
    ).

%% @doc 真正的 Template 单事务创建
do_create_template(Conn, OwnerUid, Name, RequestId) ->
    Now = elib_dt:now(),
    %% 1. workspace 行（branding 内嵌 _request_id 内部幂等标记 + name 品牌键）
    Branding0 = #{<<"name">> => Name},
    Branding =
        case RequestId of
            <<>> -> Branding0;
            _ -> Branding0#{<<"_request_id">> => RequestId}
        end,
    BrandingJson = jsone:encode(Branding, [native_utf8]),
    {ok, WsId} = workspace_repo:add(Conn, #{
        <<"name">> => Name,
        <<"owner_id">> => OwnerUid,
        <<"status">> => <<"active">>,
        <<"branding">> => BrandingJson,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    }),
    %% 2. Owner workspace_member（创建者有且仅有一条 Owner 记录，§1.4.2 规则 1）
    ok = workspace_member_repo:insert_member_tx(Conn, WsId, #{
        <<"user_id">> => OwnerUid,
        <<"role">> => <<"owner">>,
        <<"joined_at">> => Now,
        <<"status">> => <<"active">>,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    }),
    %% 3. 默认 Group（General，scope=workspace；创建者为群主 role=4）
    Gid = elib_tsid:generate(group_info),
    GroupData = #{
        <<"id">> => Gid,
        <<"title">> => ?DEFAULT_GROUP_NAME,
        <<"type">> => ?DEFAULT_GROUP_TYPE,
        <<"owner_uid">> => OwnerUid,
        <<"creator_uid">> => OwnerUid,
        <<"user_id_sum">> => OwnerUid,
        <<"scope">> => <<"workspace">>,
        <<"workspace_id">> => WsId,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },
    {Sql, Params} = elib_pg_sql:insert(group_repo:tablename(), GroupData),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, 1} -> ok;
        {error, Reason} -> throw({abort_tx, {group_create_failed, Reason}})
    end,
    case
        group_member_ds:join_group(
            Conn, <<"workspace_template">>, OwnerUid, Gid, #{role => 4}
        )
    of
        {ok, _} -> ok;
        {error, Reason2} -> throw({abort_tx, {group_member_create_failed, Reason2}})
    end,
    %% 4. 默认 Channel（Announcements，scope=workspace；创建者成为
    %%    Channel Admin（role 3，镜像 channel_ds:create_channel/3 的创建者写法）
    %%    + Channel Subscriber）
    CId = elib_tsid:generate(channel),
    ChannelData = #{
        <<"id">> => CId,
        <<"name">> => ?DEFAULT_CHANNEL_NAME,
        <<"creator_uid">> => OwnerUid,
        <<"scope">> => <<"workspace">>,
        <<"workspace_id">> => WsId,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    },
    {CSql, CParams} = elib_pg_sql:insert(channel_repo:tablename(), ChannelData),
    case elib_pg:execute(Conn, CSql, CParams) of
        {ok, 1} -> ok;
        {error, Reason3} -> throw({abort_tx, {channel_create_failed, Reason3}})
    end,
    AdminData = #{
        <<"channel_id">> => CId,
        <<"user_id">> => OwnerUid,
        <<"role">> => 3,
        <<"created_at">> => Now
    },
    case channel_admin_repo:add(Conn, AdminData) of
        {ok, _} -> ok;
        {error, Reason4} -> throw({abort_tx, {channel_admin_create_failed, Reason4}})
    end,
    case channel_subscription_repo:upsert_active(Conn, CId, OwnerUid) of
        {ok, _} -> ok;
        {error, Reason5} -> throw({abort_tx, {channel_subscribe_failed, Reason5}})
    end,
    WS = workspace_repo:find_by_id(WsId, <<"id,name,logo,owner_id,status,branding,created_at">>),
    #{
        workspace_id => WsId,
        workspace => public_view(WS),
        channel_id => CId,
        group_id => Gid
    }.

%% @doc 默认 Channel 查找（幂等命中路径回读）
default_channel_of(Conn, WsId) ->
    Sql =
        <<"SELECT id FROM channel WHERE workspace_id = $1 AND scope = 'workspace'",
            " AND status = 1 ORDER BY created_at ASC, id ASC LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [WsId]) of
        {ok, [#{<<"id">> := CId} | _]} -> {ok, CId};
        _ -> {ok, 0}
    end.

%% @doc 默认 Group 查找（幂等命中路径回读）
default_group_of(Conn, WsId) ->
    Sql =
        <<"SELECT id FROM \"group\" WHERE workspace_id = $1 AND scope = 'workspace'",
            " AND status = 1 ORDER BY created_at ASC, id ASC LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [WsId]) of
        {ok, [#{<<"id">> := GId} | _]} -> {ok, GId};
        _ -> {ok, 0}
    end.

%% @doc 工作区详情（公共视图）
-spec find_by_id(integer() | binary()) -> map() | {error, term()}.
find_by_id(WsId) ->
    workspace_repo:find_by_id(
        WsId, <<"id,name,logo,owner_id,status,branding,created_at,updated_at">>
    ).

-spec find_by_id(integer() | binary(), binary()) -> map() | {error, term()}.
find_by_id(WsId, Column) ->
    workspace_repo:find_by_id(WsId, Column).

%% @doc 我的工作区列表（稳定排序 created_at DESC,id DESC；limit 上限 100）
-spec page_by_member(integer(), integer(), integer()) -> {ok, map()} | {error, term()}.
page_by_member(Uid, Page, Size0) ->
    Size = clamp(Size0, 1, 100),
    Page2 = max(Page, 1),
    case
        workspace_repo:page_by_member(
            Uid,
            Page2,
            Size,
            <<"w.id,w.name,w.logo,w.owner_id,w.status,w.created_at">>
        )
    of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 改名/改 logo（白名单字段）
-spec update_profile(integer(), binary(), map()) -> {ok, map()} | {error, term()}.
update_profile(WsId, Name, Logo) ->
    Data =
        maps:merge(
            #{<<"updated_at">> => elib_dt:now()},
            whitelist_fields(#{<<"name">> => Name, <<"logo">> => Logo})
        ),
    case workspace_repo:update_by_id(WsId, Data) of
        {ok, _} -> {ok, find_by_id(WsId)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc branding 白名单读（仅 name/logo/primaryColor）
-spec read_branding(integer()) -> {ok, map()} | {error, term()}.
read_branding(WsId) ->
    case find_by_id(WsId, <<"branding">>) of
        #{<<"branding">> := Branding} -> {ok, branding_public_view(Branding)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc branding 白名单写（合并式：只更新提交的白名单键，其余保留）
-spec update_branding(integer(), map(), map()) -> {ok, map()} | {error, term()}.
update_branding(WsId, NewFields0, CurrentBrandings) ->
    NewFields = whitelist_fields(NewFields0),
    case map_size(NewFields) of
        0 ->
            {ok, branding_public_view(CurrentBrandings)};
        _ ->
            Merged = maps:merge(CurrentBrandings, NewFields),
            %% 保留内部键（_request_id 幂等标记）
            InternalKeys = maps:filter(
                fun
                    (<<"_", _/binary>>, _) -> true;
                    (_, _) -> false
                end,
                CurrentBrandings
            ),
            Merged2 = maps:merge(Merged, InternalKeys),
            Data = #{
                <<"branding">> => jsone:encode(Merged2, [native_utf8]),
                <<"updated_at">> => elib_dt:now()
            },
            case workspace_repo:update_by_id(WsId, Data) of
                {ok, _} -> {ok, branding_public_view(Merged2)};
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc branding 公共视图：剥内部键 + 仅白名单键
-spec branding_public_view(map() | binary()) -> map().
branding_public_view(Branding) when is_binary(Branding) ->
    case catch jsone:decode(Branding, [{object_format, map}]) of
        Map when is_map(Map) -> branding_public_view(Map);
        _ -> #{}
    end;
branding_public_view(Branding) when is_map(Branding) ->
    maps:with(?BRANDING_KEYS, Branding).

%% @doc Overview：资源摘要（Projects/Groups/Channels 计数）+ 工作区成员预览
-spec overview(integer(), integer()) -> {ok, map()} | {error, term()}.
overview(WsId, PreviewLimit0) ->
    PreviewLimit = clamp(PreviewLimit0, 1, ?MEMBER_PREVIEW_LIMIT),
    Counts = resource_counts(WsId),
    case workspace_member_repo:list_by_workspace(WsId, PreviewLimit) of
        {ok, Members} ->
            {ok, Counts#{
                member_preview => Members
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 资源计数（一次查询三条 COUNT 走各自索引，数量级恒小）
-spec resource_counts(integer()) -> map().
resource_counts(WsId) ->
    #{
        project_count => count_rows(<<"project">>, WsId, <<"status = 'active'">>),
        group_count => count_rows(<<"\"group\"">>, WsId, <<"scope = 'workspace' AND status = 1">>),
        channel_count => count_rows(<<"channel">>, WsId, <<"scope = 'workspace' AND status = 1">>)
    }.

count_rows(Tb, WsId, ExtraWhere) ->
    Sql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE workspace_id = $1 AND ",
            ExtraWhere/binary>>,
    case elib_pg:one(Sql, [WsId]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc 事务内主 Owner 转移（workspace.owner_id + 双角色调整由调用方完成）
-spec ws_transfer_tx(any(), integer(), integer()) -> ok | {error, term()}.
ws_transfer_tx(Conn, WsId, NewOwnerUid) ->
    workspace_repo:update_owner_tx(Conn, WsId, NewOwnerUid).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec normalize_request_id(term()) -> binary().
normalize_request_id(RequestId) when is_binary(RequestId), RequestId =/= <<>> ->
    elib_str:trunc(RequestId, 64);
normalize_request_id(_) ->
    <<>>.

-spec whitelist_fields(map()) -> map().
whitelist_fields(Fields) ->
    maps:filter(
        fun(K, V) ->
            lists:member(K, ?BRANDING_KEYS) andalso is_binary(V)
        end,
        Fields
    ).

%% 工作区行公共视图：branding 解码为对象（剥内部键）
-spec public_view(map()) -> map().
public_view(WS) when is_map(WS) ->
    case maps:find(<<"branding">>, WS) of
        {ok, Branding} when is_binary(Branding) ->
            WS#{<<"branding">> => branding_public_view(Branding)};
        _ ->
            WS
    end.

-spec clamp(integer(), integer(), integer()) -> integer().
clamp(Value, Min, Max) ->
    max(Min, min(Max, Value)).
