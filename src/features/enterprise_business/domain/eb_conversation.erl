%%% @doc 企业会话（enterprise_conversation）scope 与经办人交接的领域纯函数。
%%%
%%% 依据：plan v4.1 EB-D01 / EB-D03 / EB-D05、§2.1 #15、§4.3。
%%%
%%% 纯净性（铁律 4）：无 I/O、无进程、无隐式时间/随机源。
%%%
%%% 冻结的不变量：
%%%   * conversation 的 `workspace_id` 必须属于同一 `organization_id`
%%%     （跨 Org Workspace 绑定在 domain 与 DB 两层都被拒）；
%%%   * `organization_id` 是 owner 且不可由业务操作改变，`conversation_id`
%%%     是稳定 resource id；
%%%   * handover 只改当前经办 `business_identity_id`，owner / resource id 逐字不变。
-module(eb_conversation).

-export([open/1, handover/2, caretaker/1]).

-type conversation() :: map().
-type params() :: map().

-export_type([conversation/0]).

%% @doc 校验并归一化一个企业会话描述。
%%
%% 校验顺序固定：organization_id → workspace_id → workspace_organization_id
%% （必须等于 organization_id）→ contact_id → business_identity_id →
%% conversation_id。任一缺失/非整数一律 fail-closed，不得默认任何值。
-spec open(params()) -> {ok, conversation()} | {error, term()}.
open(Params) when is_map(Params) ->
    OrganizationId = maps:get(organization_id, Params, undefined),
    WorkspaceId = maps:get(workspace_id, Params, undefined),
    WorkspaceOrganizationId = maps:get(workspace_organization_id, Params, undefined),
    case require_integer(organization_id, OrganizationId) of
        {error, _} = Err ->
            Err;
        {ok, OrgId} ->
            case require_integer(workspace_id, WorkspaceId) of
                {error, _} = Err ->
                    Err;
                {ok, WsId} ->
                    open_with_scope(Params, OrgId, WsId, WorkspaceOrganizationId)
            end
    end;
open(_NotAMap) ->
    {error, invalid_params}.

open_with_scope(Params, OrgId, WsId, WorkspaceOrganizationId) ->
    case require_integer(workspace_organization_id, WorkspaceOrganizationId) of
        {error, _} = Err ->
            Err;
        {ok, WsOrgId} when WsOrgId =/= OrgId ->
            {error, {workspace_org_mismatch, OrgId, WsOrgId}};
        {ok, _SameOrg} ->
            open_with_resources(Params, OrgId, WsId)
    end.

open_with_resources(Params, OrgId, WsId) ->
    ContactId = maps:get(contact_id, Params, undefined),
    IdentityId = maps:get(business_identity_id, Params, undefined),
    ConversationId = maps:get(conversation_id, Params, undefined),
    case require_integer(contact_id, ContactId) of
        {error, _} = Err ->
            Err;
        {ok, Cid} ->
            case require_integer(business_identity_id, IdentityId) of
                {error, _} = Err ->
                    Err;
                {ok, IdentId} ->
                    case require_integer(conversation_id, ConversationId) of
                        {error, _} = Err ->
                            Err;
                        {ok, ConvId} ->
                            {ok, #{
                                organization_id => OrgId,
                                workspace_id => WsId,
                                contact_id => Cid,
                                business_identity_id => IdentId,
                                conversation_id => ConvId,
                                resource_id => ConvId,
                                status => active,
                                version => 1
                            }}
                    end
            end
    end.

%% @doc 把会话的当前经办 identity 从旧值换到 `NewIdentityId`。
%%
%% 只改 `business_identity_id` 与 `version`；`organization_id`、`workspace_id`、
%% `contact_id`、`conversation_id` / `resource_id` 必须逐字不变。
%% 自交接（新旧同一 identity）被拒，避免产生无意义的空交接审计。
-spec handover(conversation(), term()) -> {ok, conversation()} | {error, term()}.
handover(Conversation, NewIdentityId) when is_map(Conversation) ->
    Current = maps:get(business_identity_id, Conversation, undefined),
    case is_integer(NewIdentityId) of
        false ->
            {error, {invalid_identity, NewIdentityId}};
        true when NewIdentityId =:= Current ->
            {error, same_identity_handover};
        true ->
            Version = maps:get(version, Conversation, 1),
            {ok, Conversation#{
                business_identity_id := NewIdentityId,
                version := Version + 1
            }}
    end;
handover(_NotAMap, _NewIdentityId) ->
    {error, invalid_conversation}.

%% @doc 读取会话当前经办 identity（assignee）。owner 判定不使用本函数的返回值。
-spec caretaker(conversation()) -> {ok, integer()} | {error, term()}.
caretaker(Conversation) when is_map(Conversation) ->
    case maps:get(business_identity_id, Conversation, undefined) of
        IdentityId when is_integer(IdentityId) -> {ok, IdentityId};
        _Missing -> {error, {missing_field, business_identity_id}}
    end;
caretaker(_NotAMap) ->
    {error, invalid_conversation}.

require_integer(Field, Value) ->
    case is_integer(Value) of
        true -> {ok, Value};
        false -> {error, {missing_field, Field}}
    end.
