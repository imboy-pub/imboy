-module(organization_member_handler).

-behavior(cowboy_rest).

-export([init/2, handle_action/3]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    {ok, handle_action(Action, Req0, State), State}.

-spec handle_action(atom(), cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(collection, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> list(Req0, State);
        <<"POST">> -> invite(Req0, State);
        _ -> method_not_allowed(Req0, <<"GET, POST">>)
    end;
handle_action(role, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"PUT">> -> change_role(Req0, State);
        _ -> method_not_allowed(Req0, <<"PUT">>)
    end;
handle_action(owner_transfer, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> transfer_owner(Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
handle_action(member, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"DELETE">> -> remove(Req0, State);
        _ -> method_not_allowed(Req0, <<"DELETE">>)
    end;
%% —— 成员生命周期命令（EB-D07/EB-08）：suspend / restore / offboard ——
handle_action(member_suspend, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> member_command(suspend, Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
handle_action(member_restore, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> member_command(restore, Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end;
handle_action(member_offboard, Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> member_command(offboard, Req0, State);
        _ -> method_not_allowed(Req0, <<"POST">>)
    end.

list(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        respond(Req0, organization_member_logic:list(Uid, OrgId, Page, Size))
    end).

invite(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        TargetUid = positive_id(maps:get(<<"user_id">>, Params, undefined)),
        Role = maps:get(<<"role">>, Params, <<"member">>),
        case organization_member_logic:invite(Uid, OrgId, TargetUid, Role) of
            {ok, Status, Member} ->
                elib_response:success(Req0, Member#{status_flag => Status});
            {error, {Code, Msg}} ->
                elib_response:error(Req0, Msg, Code)
        end
    end).

change_role(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    with_ids(Req0, fun(OrgId, TargetUid) ->
        Role = maps:get(<<"role">>, Params, undefined),
        case organization_member_logic:change_role(Uid, OrgId, TargetUid, Role) of
            {ok, Status, Member} ->
                elib_response:success(Req0, Member#{status_flag => Status});
            {error, {Code, Msg}} ->
                elib_response:error(Req0, Msg, Code)
        end
    end).

remove(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_ids(Req0, fun(OrgId, TargetUid) ->
        respond(Req0, organization_member_logic:remove(Uid, OrgId, TargetUid))
    end).

%% 成员生命周期命令共用形状（organization_id + user_id 绑定，POST 命令语义）：
%%   * suspend  → logic suspend/3（active → suspended，可逆撤权第一步）；
%%   * restore  → logic restore/3（suspended → active，EB-D07 复位端）；
%%   * offboard → logic remove/3（active|suspended → removed 终态，EB-08 两步
%%     离场的 S3；「offboard」与 DB 守卫 trg_organization_member_offboarding_guard
%%     同名同义，不另发明语义）。
member_command(suspend, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_ids(Req0, fun(OrgId, TargetUid) ->
        respond(Req0, organization_member_logic:suspend(Uid, OrgId, TargetUid))
    end);
member_command(restore, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_ids(Req0, fun(OrgId, TargetUid) ->
        respond(Req0, organization_member_logic:restore(Uid, OrgId, TargetUid))
    end);
member_command(offboard, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    with_ids(Req0, fun(OrgId, TargetUid) ->
        respond(Req0, organization_member_logic:remove(Uid, OrgId, TargetUid))
    end).

transfer_owner(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Params = elib_param:post(Req0),
    with_organization_id(Req0, fun(OrgId) ->
        TargetUid = positive_id(maps:get(<<"user_id">>, Params, undefined)),
        respond(Req0, organization_member_logic:transfer_owner(Uid, OrgId, TargetUid))
    end).

respond(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
respond(Req0, {error, {Code, Msg}}) ->
    elib_response:error(Req0, Msg, Code).

with_organization_id(Req0, Fun) ->
    case positive_binding(organization_id, Req0) of
        {ok, OrgId} -> Fun(OrgId);
        error -> elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400)
    end.

with_ids(Req0, Fun) ->
    case {positive_binding(organization_id, Req0), positive_binding(user_id, Req0)} of
        {{ok, OrgId}, {ok, TargetUid}} ->
            Fun(OrgId, TargetUid);
        {error, _} ->
            elib_response:error(Req0, <<"organization_id 必须是正整数"/utf8>>, 400);
        _ ->
            elib_response:error(Req0, <<"user_id 必须是正整数"/utf8>>, 400)
    end.

positive_binding(Name, Req0) ->
    case positive_id(cowboy_req:binding(Name, Req0)) of
        Id when is_integer(Id), Id > 0 -> {ok, Id};
        _ -> error
    end.

positive_id(Value) ->
    case elib_cnv:safe_to_integer(Value) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> 0
    end.

method_not_allowed(Req0, Allow) ->
    cowboy_req:reply(
        405,
        #{<<"allow">> => Allow, <<"content-type">> => <<"text/plain; charset=utf-8">>},
        <<"Method Not Allowed">>,
        Req0
    ).
