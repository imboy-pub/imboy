-module(project_channel_logic).
-compile([nowarn_deprecated_catch]).
%%%
% project_channel_logic 项目↔频道关联业务逻辑（channel-firstclass W2 ZC-04）
%
% W2 权限模型（计划 §权限语义）：
%   关联/解除/写 links（写路径）：
%     Project Owner 或 active Project Member，且 workspace role ≠ guest；
%     guest 只读（403）；无 active project_member 关系 → 403；项目不存在 404；
%     archived workspace 拒写（workspace_guard，稳定错误码 980）
%   四聚合 / 关联列表（只读路径）：
%     Project Owner 或 active Project Member 可读（guest 可读）；
%     Owner 亦须 active workspace_member（M-1 fail-closed，写路径同标准）；
%     无关系 403
%
% ⚠️ 权限校验不依赖 project_member_logic（ZC-02 并行产出）：
%   经 project_channel_rel_repo:find_project_member/2 直查 project_member
%   （只读；ZC-05 统一整合到 project_member_repo）。
%
% update-links 应用层校验（DB trg_project_links_shape 之外的入参防线）：
%   links 必须是 [{name,url}] 对象数组：name 非空 ≤200 字符、url 非空
%   ≤2048 字符、条数 ≤20；校验失败 400 且不落任何写。
%
% 输出边界：
%   * Related Posts / Pinned 只含元数据列（DS 层列白名单）；
%   * Activity payload 在本层展示前清洗 content/message/body/text 键。
%
% 错误约定：{error, {Code, Msg}}，Code 取 error_code.hrl 语义码。
%%%

-export([link/3]).
-export([unlink/3]).
-export([list_channels/4]).
-export([update_links/3]).
-export([pinned/4]).
-export([related_posts/2]).
-export([activity/4]).
-export([resources/2]).
-export([admin_channels/3]).
-export([admin_aggregation/4]).

-include("log.hrl").

%% links 上限（应用层防线；DB 触发器只强制形状）
-define(MAX_LINKS, 20).
-define(MAX_NAME_LEN, 200).
-define(MAX_URL_LEN, 2048).
%% Activity payload 正文类键清洗清单（事件写入端契约本就不含正文，双保险）
-define(PAYLOAD_CONTENT_KEYS, [<<"content">>, <<"message">>, <<"body">>, <<"text">>]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 关联频道（Owner/active Member 非 guest；幂等 created|existing）
-spec link(integer(), integer(), integer()) ->
    {ok, created | existing} | {error, {integer(), binary()}}.
link(Uid, ProjectId, ChannelId) ->
    case ensure_can_write(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            case project_channel_ds:link(Uid, ProjectId, ChannelId) of
                {ok, Status} ->
                    {ok, Status};
                %% DS 层错误已归一 {Code, Msg}（success typing 穷尽；
                %% 新增形态由 dialyzer 基线门拦截）
                {error, {Code, Msg}} when is_integer(Code) ->
                    {error, {Code, Msg}}
            end
    end.

%% @doc 解除关联（缺失关联 404；channel_unlinked 事件同事务）
-spec unlink(integer(), integer(), integer()) ->
    {ok, unlinked} | {error, {integer(), binary()}}.
unlink(Uid, ProjectId, ChannelId) ->
    case ensure_can_write(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            case project_channel_ds:unlink(Uid, ProjectId, ChannelId) of
                {ok, Status} ->
                    {ok, Status};
                %% 同 link/3：DS 错误已归一 {Code, Msg}
                {error, {Code, Msg}} when is_integer(Code) ->
                    {error, {Code, Msg}}
            end
    end.

%% @doc 项目关联频道列表（active Member/guest 可读；稳定分页）
-spec list_channels(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
list_channels(Uid, ProjectId, Page, Size) ->
    case ensure_can_read(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            case project_channel_ds:list_channels(ProjectId, Page, Size) of
                {ok, Result} ->
                    {ok, Result};
                {error, Reason} ->
                    _ = ?ERROR_LOG([project_channel_list_failed, ProjectId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

%% @doc 全量替换 links（应用层形状校验 400；全量替换 + links_updated 事件）
-spec update_links(integer(), integer(), term()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
update_links(Uid, ProjectId, Links) ->
    case ensure_can_write(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            case validate_links(Links) of
                {error, Msg} ->
                    {error, {400, Msg}};
                {ok, Norm} ->
                    case project_channel_ds:update_links(Uid, ProjectId, Norm) of
                        {ok, Saved} ->
                            {ok, Saved};
                        %% 同 link/3：DS 错误已归一 {Code, Msg}
                        {error, {Code, Msg2}} when is_integer(Code) ->
                            {error, {Code, Msg2}}
                    end
            end
    end.

%% @doc Pinned 聚合（置顶消息元数据，排除公告形态；guest 可读）
-spec pinned(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
pinned(Uid, ProjectId, Page, Size) ->
    read_agg(Uid, ProjectId, fun(ProjectId2) ->
        project_channel_ds:pinned(ProjectId2, Page, Size)
    end).

%% @doc Related Posts 聚合（有界摘要元数据，不含正文；guest 可读）
-spec related_posts(integer(), integer()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
related_posts(Uid, ProjectId) ->
    read_agg(Uid, ProjectId, fun(ProjectId2) ->
        project_channel_ds:related_posts(ProjectId2)
    end).

%% @doc Activity 聚合（事件元数据流；payload 清洗正文类键；guest 可读）
-spec activity(integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
activity(Uid, ProjectId, Page, Size) ->
    read_agg(Uid, ProjectId, fun(ProjectId2) ->
        case project_channel_ds:activity(ProjectId2, Page, Size) of
            {ok, #{list := Rows} = Result} ->
                {ok, Result#{list => sanitize_activity_rows(Rows)}};
            Other ->
                Other
        end
    end).

%% @doc Resources 聚合（project.links 原样返回；guest 可读）
-spec resources(integer(), integer()) ->
    {ok, [map()]} | {error, {integer(), binary()}}.
resources(Uid, ProjectId) ->
    read_agg(Uid, ProjectId, fun(ProjectId2) ->
        project_channel_ds:resources(ProjectId2)
    end).

%% ===================================================================
%% Admin 治理只读（ZC-05；adm ACL workspaces:read 门，不走 workspace
%% 成员权限语义；聚合经由 ds 层既有有界查询，SQL 上限不变）
%% ===================================================================

%% @doc Admin：项目关联频道分页
-spec admin_channels(integer(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_channels(ProjectId, Page0, Size0) ->
    Page = max(1, Page0),
    Size = max(1, min(100, Size0)),
    case project_channel_rel_repo:page_channels_by_project(ProjectId, Page, Size) of
        {ok, P} ->
            {ok, P};
        {error, Reason} ->
            _ = ?ERROR_LOG([admin_channels_failed, ProjectId, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

%% @doc Admin：四类聚合只读（type = pinned | resources | activity | related_posts）。
%% related_posts 为有界摘要（每频道 ?RP_PER_CHANNEL 条/总量 ?RP_TOTAL_CAP 上限），
%% 无分页语义，page/size 忽略；resources 同为全量有界数组。
-spec admin_aggregation(integer(), binary(), integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
admin_aggregation(ProjectId, Type, Page0, Size0) ->
    Page = max(1, Page0),
    Size = max(1, min(100, Size0)),
    Res =
        case Type of
            <<"pinned">> ->
                project_channel_ds:pinned(ProjectId, Page, Size);
            <<"activity">> ->
                project_channel_ds:activity(ProjectId, Page, Size);
            <<"related_posts">> ->
                project_channel_ds:related_posts(ProjectId);
            <<"resources">> ->
                project_channel_ds:resources(ProjectId);
            _ ->
                {error, {400, <<"未知的聚合类型"/utf8>>}}
        end,
    case Res of
        {ok, List} when is_list(List) ->
            %% 无分页聚合统一包 page 结构，前端消费形态一致
            Total = length(List),
            {ok, #{list => List, page => Page, size => Size, total => Total, total_page => 1}};
        {ok, #{list := _} = P} ->
            {ok, P};
        {error, {Code, Msg}} when is_integer(Code) ->
            {error, {Code, Msg}};
        {error, Reason} ->
            _ = ?ERROR_LOG([admin_aggregation_failed, ProjectId, Type, Reason]),
            {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
    end.

%% ===================================================================
%% 权限校验（不依赖 project_member_logic；ZC-05 统一整合点）
%% ===================================================================

%% @doc 项目读权限：Owner 或 active project_member（guest 可读）；
%% 无关系 403；项目不存在 404
-spec ensure_can_read(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_read(Uid, ProjectId) ->
    case load_project(ProjectId) of
        {error, NotFound} ->
            {error, NotFound};
        {ok, Project} ->
            OwnerId = maps:get(<<"owner_id">>, Project, 0),
            case OwnerId =:= Uid of
                true ->
                    %% M-1：Owner 读亦须 active workspace_member（fail-closed
                    %% 403），与 ensure_can_write 及 member/milestone 逻辑一致
                    case owner_ws_active(Project, Uid) of
                        true ->
                            {ok, Project};
                        false ->
                            {error, {403, <<"无权限访问该项目"/utf8>>}}
                    end;
                false ->
                    case project_channel_rel_repo:find_project_member(ProjectId, Uid) of
                        #{<<"status">> := <<"active">>} ->
                            {ok, Project};
                        _ ->
                            {error, {403, <<"无权限访问该项目"/utf8>>}}
                    end
            end
    end.

%% Owner 的 workspace_member 身份是否 active（M-1 读路径 fail-closed 防线；
%% 正常 API 流程下由 DB 三重防线保证 owner 必有 active wm，此处为人工
%% DB 治理态的一致性兜底）
-spec owner_ws_active(map(), integer()) -> boolean().
owner_ws_active(Project, Uid) ->
    WsId = maps:get(<<"workspace_id">>, Project, undefined),
    case workspace_member_repo:find(WsId, Uid, <<"status">>) of
        #{<<"status">> := <<"active">>} -> true;
        _ -> false
    end.

%% @doc 项目写权限：读权限基础上要求 active workspace_member 且 role ≠ guest
%% （Owner 亦然——Owner 的 ws 身份失效时 fail-closed 403）
-spec ensure_can_write(integer(), integer()) ->
    {ok, map()} | {error, {integer(), binary()}}.
ensure_can_write(Uid, ProjectId) ->
    case ensure_can_read(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, Project} ->
            WsId = maps:get(<<"workspace_id">>, Project),
            case workspace_member_repo:find(WsId, Uid, <<"role,status">>) of
                #{<<"status">> := <<"active">>, <<"role">> := <<"guest">>} ->
                    {error, {403, <<"Guest 角色为只读，不能修改项目"/utf8>>}};
                #{<<"status">> := <<"active">>} ->
                    {ok, Project};
                _ ->
                    {error, {403, <<"无权限执行该操作"/utf8>>}}
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec read_agg(integer(), integer(), fun((integer()) -> {ok, term()} | {error, term()})) ->
    {ok, term()} | {error, {integer(), binary()}}.
read_agg(Uid, ProjectId, Fun) ->
    case ensure_can_read(Uid, ProjectId) of
        {error, Reason} ->
            {error, Reason};
        {ok, _Project} ->
            case Fun(ProjectId) of
                {ok, _} = Ok ->
                    Ok;
                {error, Reason} ->
                    _ = ?ERROR_LOG([project_channel_agg_failed, ProjectId, Reason]),
                    {error, {500, <<"查询失败，请稍后重试"/utf8>>}}
            end
    end.

-spec load_project(integer()) -> {ok, map()} | {error, {404, binary()}}.
load_project(ProjectId) ->
    case project_repo:find_by_id(ProjectId, <<"id,workspace_id,owner_id">>) of
        Project when is_map(Project), map_size(Project) > 0 ->
            {ok, Project};
        _ ->
            {error, {404, <<"项目不存在"/utf8>>}}
    end.

%% links 入参校验（DB trg_project_links_shape 之外的应用层防线）：
%% 对象数组、每项 name 非空 ≤200 字符、url 非空 ≤2048 字符、条数 ≤20
-spec validate_links(term()) -> {ok, [map()]} | {error, binary()}.
validate_links(Links) when is_list(Links) ->
    case length(Links) =< ?MAX_LINKS of
        false ->
            {error, <<"links 数量不能超过 20 条"/utf8>>};
        true ->
            validate_link_elements(Links, [])
    end;
validate_links(_) ->
    {error, <<"links 必须是对象数组"/utf8>>}.

validate_link_elements([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_link_elements([El | Rest], Acc) when is_map(El) ->
    Name = maps:get(<<"name">>, El, undefined),
    Url = maps:get(<<"url">>, El, undefined),
    case valid_link_name(Name) andalso valid_link_url(Url) of
        false ->
            {error, <<"links 每项必须包含非空 name（≤200 字符）与 url（≤2048 字符）"/utf8>>};
        true ->
            validate_link_elements(Rest, [#{<<"name">> => Name, <<"url">> => Url} | Acc])
    end;
validate_link_elements(_, _) ->
    {error, <<"links 每项必须是含 name 与 url 的对象"/utf8>>}.

valid_link_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    %% 按字符数校验防多字节截断报错
    string:length(Name) =< ?MAX_NAME_LEN;
valid_link_name(_) ->
    false.

valid_link_url(Url) when is_binary(Url), byte_size(Url) > 0 ->
    byte_size(Url) =< ?MAX_URL_LEN;
valid_link_url(_) ->
    false.

%% Activity payload 正文类键清洗（元数据契约双保险；解析失败归一空对象）
sanitize_activity_rows(Rows) ->
    [sanitize_activity_row(Row) || Row <- Rows].

sanitize_activity_row(Row) ->
    case maps:get(<<"payload">>, Row, undefined) of
        Payload when is_binary(Payload) ->
            Row#{<<"payload">> => sanitize_payload(Payload)};
        Payload when is_map(Payload) ->
            Row#{<<"payload">> => strip_content_keys(Payload)};
        _ ->
            Row#{<<"payload">> => #{}}
    end.

sanitize_payload(Bin) when is_binary(Bin) ->
    case catch jsone:decode(Bin, [{object_format, map}]) of
        M when is_map(M) -> strip_content_keys(M);
        _ -> #{}
    end.

strip_content_keys(Payload) ->
    maps:without(?PAYLOAD_CONTENT_KEYS, Payload).
