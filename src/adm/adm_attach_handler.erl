-module(adm_attach_handler).

%%%
% adm_attach 控制器模块
% adm_attach controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            auth -> auth(Method, Req0, State);
            stats -> stats(Method, Req0, State);
            index -> index(Method, Req0, State);
            disable -> disable(Method, Req0, State);
            enable -> enable(Method, Req0, State);
            delete -> delete(Method, Req0, State);
            orphan -> orphan(Method, Req0, State);
            orphan_cleanup -> orphan_cleanup(Method, Req0, State);
            false -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec auth(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
auth(<<"POST">>, Req0, _State) ->
    PostVals = elib_param:post(Req0),
    Uri = maps:get(<<"uri">>, PostVals, ""),
    Result = [elib_uri:check_auth(I) || I <- binary:split(Uri, <<",">>)],
    elib_response:success(Req0, #{<<"uri">> => Result}, "success.");
auth(_, Req0, _State) ->
    Req0.

-spec stats(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
stats(<<"GET">>, Req0, _State) ->
    Result = attachment_ds:stats(),
    elib_response:success(Req0, Result, "success.");
stats(_, Req0, _State) ->
    Req0.

-spec index(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    Qs = cowboy_req:parse_qs(Req0),
    MimeType = proplists:get_value(<<"mime_type">>, Qs, undefined),
    Keyword = proplists:get_value(<<"keyword">>, Qs, undefined),
    Opts = #{mime_type => MimeType, keyword => Keyword},
    case attachment_ds:page(Page, Size, Opts) of
        {ok, Result} ->
            elib_response:success(Req0, Result, "success.");
        {error, _Reason} ->
            elib_response:error(Req0, <<"查询失败"/utf8>>)
    end;
index(_, Req0, _State) ->
    Req0.

-spec disable(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
disable(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"storage:disable">>, Req0) of
        ok ->
            case parse_id(Req0) of
                {ok, Id} ->
                    case attachment_ds:disable(Id) of
                        ok ->
                            elib_response:success(Req0, #{});
                        {error, R} ->
                            elib_response:error(
                                Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                {error, Req1} ->
                    Req1
            end;
        {error, Req1} ->
            Req1
    end;
disable(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec enable(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
enable(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"storage:enable">>, Req0) of
        ok ->
            case parse_id(Req0) of
                {ok, Id} ->
                    case attachment_ds:enable(Id) of
                        ok ->
                            elib_response:success(Req0, #{});
                        {error, R} ->
                            elib_response:error(
                                Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                {error, Req1} ->
                    Req1
            end;
        {error, Req1} ->
            Req1
    end;
enable(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"storage:delete">>, Req0) of
        ok ->
            case parse_id(Req0) of
                {ok, Id} ->
                    case attachment_ds:soft_delete(Id) of
                        ok ->
                            elib_response:success(Req0, #{});
                        {error, R} ->
                            elib_response:error(
                                Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR
                            )
                    end;
                {error, Req1} ->
                    Req1
            end;
        {error, Req1} ->
            Req1
    end;
delete(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec orphan(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
orphan(<<"GET">>, Req0, State) ->
    case ensure_permission(State, <<"storage:cleanup">>, Req0) of
        ok ->
            Qs = cowboy_req:parse_qs(Req0),
            AgeDaysRaw = proplists:get_value(<<"age_days">>, Qs, <<"30">>),
            AgeDays = max(7, safe_int(AgeDaysRaw, 30)),
            case attachment_ds:orphan_stats(#{age_days => AgeDays}) of
                {ok, Stats} ->
                    elib_response:success(Req0, Stats, "success.");
                {error, R} ->
                    elib_response:error(Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR)
            end;
        {error, Req1} ->
            Req1
    end;
orphan(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

-spec orphan_cleanup(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
orphan_cleanup(<<"POST">>, Req0, State) ->
    case ensure_permission(State, <<"storage:cleanup">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            AgeDays = max(7, safe_int(maps:get(<<"age_days">>, PostVals, 30), 30)),
            case attachment_ds:orphan_cleanup(#{age_days => AgeDays}) of
                {ok, Stats} ->
                    elib_response:success(Req0, Stats, "success.");
                {error, R} ->
                    elib_response:error(Req0, ec_cnv:to_binary(R), ?ERR_INTERNAL_SERVER_ERROR)
            end;
        {error, Req1} ->
            Req1
    end;
orphan_cleanup(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===================================================================
%% 权限工具函数（源自 adm_feedback_handler 模式）
%% ===================================================================

-spec ensure_permission(map(), binary(), cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_permission(State, Permission, Req0) ->
    AdmUserId = maps:get(adm_user_id, State, 0),
    case has_permission(AdmUserId, Permission) of
        true ->
            ok;
        false ->
            {error, elib_response:error(Req0, <<"无权限操作"/utf8>>, ?ERR_FORBIDDEN)}
    end.

-spec has_permission(term(), binary()) -> boolean().
has_permission(AdmUserId, Permission) when
    is_integer(AdmUserId), AdmUserId > 0, is_binary(Permission)
->
    Permissions = resolve_permissions_by_adm_user_id(AdmUserId),
    lists:member(Permission, Permissions);
has_permission(_, _) ->
    false.

-spec resolve_permissions_by_adm_user_id(integer()) -> list(binary()).
resolve_permissions_by_adm_user_id(AdmUserId) ->
    Key = {adm_user_storage_permission, AdmUserId},
    case catch adm_user_logic:find(AdmUserId, <<"id,role_id">>, Key) of
        AdmUser when is_map(AdmUser) ->
            RoleIds = normalize_role_ids(maps:get(<<"role_id">>, AdmUser, 0)),
            lists:usort(lists:append([role_permissions(RoleId) || RoleId <- RoleIds]));
        _ ->
            []
    end.

-spec role_permissions(integer()) -> list(binary()).
role_permissions(RoleId) ->
    try adm_index_handler:role_acl(RoleId) of
        {_RoleName, Permissions, _MenuPaths} when is_list(Permissions) ->
            Permissions;
        _ ->
            []
    catch
        _:_ ->
            []
    end.

-spec normalize_role_ids(term()) -> list(integer()).
normalize_role_ids(RoleId) when is_integer(RoleId), RoleId > 0 ->
    [RoleId];
normalize_role_ids(RoleIds) when is_list(RoleIds) ->
    lists:usort([Id || Value <- RoleIds, Id <- [normalize_role_id(Value)], Id > 0]);
normalize_role_ids(RoleValue) ->
    case normalize_role_id(RoleValue) of
        Id when Id > 0 -> [Id];
        _ -> []
    end.

-spec normalize_role_id(term()) -> integer().
normalize_role_id(Value) when is_integer(Value), Value > 0 ->
    Value;
normalize_role_id(Value) when is_binary(Value); is_list(Value) ->
    try ec_cnv:to_integer(Value) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> 0
    catch
        _:_ -> 0
    end;
normalize_role_id(_) ->
    0.

-spec parse_id(cowboy_req:req()) -> {ok, integer()} | {error, cowboy_req:req()}.
parse_id(Req0) ->
    PostVals = elib_param:post(Req0),
    IdRaw = maps:get(<<"id">>, PostVals, 0),
    case safe_int(IdRaw, 0) of
        Id when is_integer(Id), Id > 0 ->
            {ok, Id};
        _ ->
            {error, elib_response:error(Req0, <<"id 无效"/utf8>>, ?ERR_BAD_REQUEST)}
    end.

%% @doc 安全解析整数，非法输入回退默认值（ec_cnv:to_integer 对非数字会抛 badarg）
-spec safe_int(term(), integer()) -> integer().
safe_int(V, Default) ->
    try
        ec_cnv:to_integer(V)
    catch
        _:_ -> Default
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
