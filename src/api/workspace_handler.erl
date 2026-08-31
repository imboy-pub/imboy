-module(workspace_handler).
%%%
% 工作区 HTTP handler（双体验 v2.5.2 WP3/T4）
%
% 跨域文案约定（计划 §1.4）：本模块对外文案/审计一律使用全称"工作区成员"
% （Workspace Member），与群成员（Group Member）/频道订阅者（Channel Subscriber）
% 严格区分；API 路径使用复数资源 workspaces + members 命名。
%
%%% =====================================================================
%%% 路由片段清单（T7 统一注册 imboy_router.erl 时按此合并；本 WP 不改 router）
%%% =====================================================================
%%% METHOD path                                              → handler 动作
%%% POST   /api/v1/workspaces                                 → create
%%% GET    /api/v1/workspaces/:workspace_id                   → show
%%% GET    /api/v1/workspaces/mine                            → mine（须注册在 :id 之前）
%%% POST   /api/v1/workspaces/join                            → join（团队码加入；静态路径，须注册在 :id 之前）
%%% POST   /api/v1/workspaces/:workspace_id/invite_code       → invite_code（生成团队码，Owner only；重新生成即旧码失效）
%%% POST   /api/v1/workspaces/:workspace_id/invite_code/revoke → invite_code_revoke（撤销团队码，Owner only）
%%% POST   /api/v1/workspaces/:workspace_id/update            → update
%%% GET    /api/v1/workspaces/:workspace_id/branding          → branding（method 分派：GET=读）
%%% POST   /api/v1/workspaces/:workspace_id/branding          → branding（method 分派：POST=写）
%%% GET    /api/v1/workspaces/:workspace_id/overview          → overview
%%% GET    /api/v1/workspaces/:workspace_id/channels          → channel_list（T5 scope 分区列表）
%%% GET    /api/v1/workspaces/:workspace_id/groups            → group_list（T5 scope 分区列表）
%%% GET    /api/v1/workspaces/:workspace_id/members           → member_list
%%% POST   /api/v1/workspaces/:workspace_id/members/invite    → member_invite
%%% POST   /api/v1/workspaces/:workspace_id/members/remove    → member_remove
%%% POST   /api/v1/workspaces/:workspace_id/members/role      → member_role
%%% POST   /api/v1/workspaces/:workspace_id/members/transfer_owner → owner_transfer
%%% POST   /api/v1/workspaces/:workspace_id/archive           → archive（T7 归档，Owner only）
%%% POST   /api/v1/workspaces/:workspace_id/restore           → restore（T7 恢复，Owner only）
%%%
%%% 全部路由 JWT 保护（auth_middleware /api/v1/* 默认门），State 携带 current_uid。
%%% =====================================================================

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = resolve_action(maps:get(action, State0), Req0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% branding 路径同路径双语义：POST=写 / GET=读（T7 统一注册时合并为单一
%% action，method 分派镜像 adm_sso_handler / agent_mandate_handler 模式）
-spec resolve_action(atom(), cowboy_req:req()) -> atom().
resolve_action(branding, Req) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> branding_write;
        _ -> branding_read
    end;
resolve_action(Action, _Req) ->
    Action.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) -> create(Req, State);
handle_action(show, Req, State) -> show(Req, State);
handle_action(mine, Req, State) -> mine(Req, State);
handle_action(update, Req, State) -> update(Req, State);
handle_action(branding_read, Req, State) -> branding_read(Req, State);
handle_action(branding_write, Req, State) -> branding_write(Req, State);
handle_action(overview, Req, State) -> overview(Req, State);
handle_action(channel_list, Req, State) -> channel_list(Req, State);
handle_action(group_list, Req, State) -> group_list(Req, State);
handle_action(member_list, Req, State) -> member_list(Req, State);
handle_action(member_invite, Req, State) -> member_invite(Req, State);
handle_action(invite_code, Req, State) -> invite_code(Req, State);
handle_action(invite_code_revoke, Req, State) -> invite_code_revoke(Req, State);
handle_action(join, Req, State) -> join(Req, State);
handle_action(member_remove, Req, State) -> member_remove(Req, State);
handle_action(member_role, Req, State) -> member_role(Req, State);
handle_action(owner_transfer, Req, State) -> owner_transfer(Req, State);
handle_action(archive, Req, State) -> archive(Req, State);
handle_action(restore, Req, State) -> restore(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 创建工作区（Template 原子初始化；request_id 幂等）
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case throttle:check(three_second_once, {workspace_create, Uid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ ->
            PostVals = elib_param:post(Req0),
            Name = maps:get(<<"name">>, PostVals, <<>>),
            RequestId = maps:get(<<"request_id">>, PostVals, undefined),
            case workspace_logic:create(Uid, Name, RequestId) of
                {ok, Result, created} ->
                    elib_response:success(Req0, Result#{status => created});
                {ok, Result, existing} ->
                    %% 幂等命中：不产生重复资源，回带既有 Template 结果
                    elib_response:success(Req0, Result#{status => existing});
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 工作区详情（active 工作区成员可读）
-spec show(cowboy_req:req(), map()) -> cowboy_req:req().
show(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_workspace_id(Req0) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            case workspace_logic:detail(Uid, WsId) of
                {ok, WS} ->
                    elib_response:success(Req0, WS);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 我的工作区列表（分页；稳定排序 created_at DESC,id DESC；limit ≤ 100）
-spec mine(cowboy_req:req(), map()) -> cowboy_req:req().
mine(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    {ok, Result} = workspace_logic:mine(Uid, Page, Size),
    elib_response:success(Req0, Result).

%% @doc 改名/改 logo（仅 Owner）
-spec update(cowboy_req:req(), map()) -> cowboy_req:req().
update(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_workspace_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            Name = maps:get(<<"name">>, PostVals, undefined),
            Logo = maps:get(<<"logo">>, PostVals, undefined),
            case workspace_logic:update_profile(Uid, WsId, Name, Logo) of
                {ok, WS} ->
                    elib_response:success(Req0, WS);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc branding 读（白名单 name/logo/primaryColor）
-spec branding_read(cowboy_req:req(), map()) -> cowboy_req:req().
branding_read(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_workspace_id(Req0) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            case workspace_logic:read_branding(Uid, WsId) of
                {ok, Branding} ->
                    elib_response:success(Req0, #{workspace_id => WsId, branding => Branding});
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc branding 写（仅 Owner；白名单外键静默丢弃）
-spec branding_write(cowboy_req:req(), map()) -> cowboy_req:req().
branding_write(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_workspace_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            Branding = maps:get(<<"branding">>, PostVals, #{}),
            case is_map(Branding) of
                false ->
                    elib_response:error(Req0, <<"branding 必须是对象"/utf8>>, 400);
                true ->
                    case workspace_logic:update_branding(Uid, WsId, Branding) of
                        {ok, View} ->
                            elib_response:success(Req0, #{workspace_id => WsId, branding => View});
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% @doc Overview：资源摘要（Projects/Groups/Channels）+ 工作区成员预览
-spec overview(cowboy_req:req(), map()) -> cowboy_req:req().
overview(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_workspace_id(Req0) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            case workspace_logic:overview(Uid, WsId) of
                {ok, Data} ->
                    elib_response:success(Req0, Data#{workspace_id => WsId});
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 工作区频道列表（T5 scope 严格分区：仅 scope='workspace' 且 status=1；
%% active 工作区成员可读；personal 频道列表接口零行为变化）
-spec channel_list(cowboy_req:req(), map()) -> cowboy_req:req().
channel_list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_workspace_id(Req0) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            case workspace_logic:ensure_member(WsId, Uid) of
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code);
                {ok, _Role} ->
                    Limit = elib_param:int(limit, Req0, 100),
                    Limit2 = max(1, min(Limit, 200)),
                    case channel_logic:list_workspace_channels(WsId, Limit2) of
                        {ok, Channels} ->
                            elib_response:success(Req0, #{workspace_id => WsId, list => Channels});
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2)
                    end
            end
    end.

%% @doc 工作区群列表（T5 scope 严格分区：仅 scope='workspace' 且 status=1；
%% active 工作区成员可读；personal 群列表接口零行为变化）
-spec group_list(cowboy_req:req(), map()) -> cowboy_req:req().
group_list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_workspace_id(Req0) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            case workspace_logic:ensure_member(WsId, Uid) of
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code);
                {ok, _Role} ->
                    Limit = elib_param:int(limit, Req0, 100),
                    Limit2 = max(1, min(Limit, 200)),
                    case group_logic:list_workspace_groups(WsId, Limit2) of
                        {ok, Groups} ->
                            elib_response:success(Req0, #{workspace_id => WsId, list => Groups});
                        {error, Msg2} ->
                            elib_response:error(Req0, Msg2)
                    end
            end
    end.

%% @doc 工作区成员列表（active 成员可读；分页 ≤100）
-spec member_list(cowboy_req:req(), map()) -> cowboy_req:req().
member_list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case resolve_workspace_id(Req0) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            {Page, Size} = elib_param:page(Req0),
            case workspace_logic:member_list(Uid, WsId, Page, Size) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 邀请工作区成员（仅 Owner；仅已注册用户；幂等；不自动入群/订阅）
-spec member_invite(cowboy_req:req(), map()) -> cowboy_req:req().
member_invite(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_workspace_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            TargetUid = elib_cnv:safe_to_integer(maps:get(<<"user_id">>, PostVals, 0)),
            Role = maps:get(<<"role">>, PostVals, <<"member">>),
            case workspace_logic:invite(Uid, WsId, TargetUid, Role) of
                {ok, changed, Member} ->
                    elib_response:success(Req0, Member#{status_flag => changed});
                {ok, unchanged, Member} ->
                    elib_response:success(Req0, Member#{status_flag => unchanged});
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 生成工作区团队码（仅 Owner；归档 409；8 位 A-Z2-9、7 天有效、一码多人）
%% 节流口径同 create（three_second_once）。
-spec invite_code(cowboy_req:req(), map()) -> cowboy_req:req().
invite_code(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case throttle:check(three_second_once, {workspace_invite_code, Uid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ ->
            case resolve_workspace_id(Req0) of
                {error, Req} ->
                    Req;
                {ok, WsId} ->
                    case workspace_logic:generate_invite_code(Uid, WsId) of
                        {ok, #{code := Code, expires_at := ExpiresAt}} ->
                            elib_response:success(Req0, #{code => Code, expires_at => ExpiresAt});
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% @doc 撤销工作区团队码（仅 Owner；幂等：无 active 码 → revoked 0）。
%% 撤销后输码即 981。节流口径与生成共用（three_second_once）。
-spec invite_code_revoke(cowboy_req:req(), map()) -> cowboy_req:req().
invite_code_revoke(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case throttle:check(three_second_once, {workspace_invite_code, Uid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ ->
            case resolve_workspace_id(Req0) of
                {error, Req} ->
                    Req;
                {ok, WsId} ->
                    case workspace_logic:revoke_invite_code(Uid, WsId) of
                        {ok, #{revoked := Count}} ->
                            elib_response:success(Req0, #{revoked => Count});
                        {error, {Code, Msg}} ->
                            elib_response:error(Req0, Msg, Code)
                    end
            end
    end.

%% @doc 团队码加入工作区（任意登录用户；幂等：已加入重复输码 → unchanged）
%% payload：{status => joined|unchanged, workspace => 工作区 map（detail 同源字段）}；
%% 错误码 981 码无效/已失效（含非字符串/空 code）、982 已过期、980 已归档
%% （envelope，HTTP 恒 200）。节流键独立于 generate（join 是全模块最易被
%% 刷的 DB 放大点：任意登录用户 + 每次至少一个事务）。
-spec join(cowboy_req:req(), map()) -> cowboy_req:req().
join(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case throttle:check(three_second_once, {workspace_join, Uid}) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"在处理中，请稍后重试"/utf8>>);
        _ ->
            PostVals = elib_param:post(Req0),
            Code0 = maps:get(<<"code">>, PostVals, <<>>),
            %% 非 binary（JSON number/array 等）或 trim 后为空统一按无效码
            %% 981，避免 string:trim 抛 function_clause 落 500
            Code =
                case is_binary(Code0) of
                    true -> string:uppercase(string:trim(Code0));
                    false -> <<>>
                end,
            case workspace_logic:join_by_code(Uid, Code) of
                {ok, joined, WS} ->
                    elib_response:success(Req0, #{status => joined, workspace => WS});
                {ok, unchanged, WS} ->
                    elib_response:success(Req0, #{status => unchanged, workspace => WS});
                {error, {Code2, Msg}} ->
                    elib_response:error(Req0, Msg, Code2)
            end
    end.

%% @doc 移除工作区成员（仅 Owner；冲突 409 全回滚；无冲突级联禁用下属群成员）
-spec member_remove(cowboy_req:req(), map()) -> cowboy_req:req().
member_remove(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_workspace_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            TargetUid = elib_cnv:safe_to_integer(maps:get(<<"user_id">>, PostVals, 0)),
            case workspace_logic:remove_member(Uid, WsId, TargetUid) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 改角色（仅 Owner；最后 Owner 保护）
-spec member_role(cowboy_req:req(), map()) -> cowboy_req:req().
member_role(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_workspace_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            TargetUid = elib_cnv:safe_to_integer(maps:get(<<"user_id">>, PostVals, 0)),
            Role = maps:get(<<"role">>, PostVals, <<>>),
            case workspace_logic:change_role(Uid, WsId, TargetUid, Role) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 主 Owner 转移（仅 Owner；目标须为非 Guest active 成员）
-spec owner_transfer(cowboy_req:req(), map()) -> cowboy_req:req().
owner_transfer(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_workspace_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            TargetUid = elib_cnv:safe_to_integer(maps:get(<<"user_id">>, PostVals, 0)),
            case workspace_logic:transfer_owner(Uid, WsId, TargetUid) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 归档工作区（T7；仅 Owner；审计 archived_at/archived_by；归档后写路径
%% 被 workspace_guard 以稳定错误码 980 拒绝，读取/历史浏览不受影响）
-spec archive(cowboy_req:req(), map()) -> cowboy_req:req().
archive(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_workspace_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            case workspace_logic:archive(Uid, WsId) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% @doc 恢复工作区（T7；仅 Owner；清空审计列；恢复后写操作放行）
-spec restore(cowboy_req:req(), map()) -> cowboy_req:req().
restore(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    case resolve_workspace_id(Req0, PostVals) of
        {error, Req} ->
            Req;
        {ok, WsId} ->
            case workspace_logic:restore(Uid, WsId) of
                {ok, Result} ->
                    elib_response:success(Req0, Result);
                {error, {Code, Msg}} ->
                    elib_response:error(Req0, Msg, Code)
            end
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 解析 workspace_id：路径 binding 优先，body 兜底
-spec resolve_workspace_id(cowboy_req:req()) -> {ok, integer()} | {error, cowboy_req:req()}.
resolve_workspace_id(Req0) ->
    resolve_workspace_id(Req0, #{}).

-spec resolve_workspace_id(cowboy_req:req(), map()) -> {ok, integer()} | {error, cowboy_req:req()}.
resolve_workspace_id(Req0, PostVals) ->
    Raw =
        case cowboy_req:binding(workspace_id, Req0) of
            undefined -> maps:get(<<"workspace_id">>, PostVals, undefined);
            Binding -> Binding
        end,
    WsId = elib_cnv:safe_to_integer(Raw),
    case is_integer(WsId) andalso WsId > 0 of
        true -> {ok, WsId};
        false -> {error, elib_response:error(Req0, <<"workspace id 必须"/utf8>>, 400)}
    end.
