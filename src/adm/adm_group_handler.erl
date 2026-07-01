-module(adm_group_handler).
-dialyzer({nowarn_function, [kick_member/3]}).
%%%
% adm_group 控制器模块
% 群组管理 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-import(adm_group_helper, [parse_gid_param/1, normalize_positive_int/1, audit_group_governance/5]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("error_code.hrl").

-define(ADM_GROUP_AUDIT_TYPE, 902).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case imboy_plugin_registry:required_feature(admin, adm_group_handler, Action) of
            undefined ->
                dispatch(Action, Method, Req0, State);
            Feature ->
                case imboy_feature:ensure_enabled(Req0, Feature) of
                    ok ->
                        dispatch(Action, Method, Req0, State);
                    {error, RespReq} ->
                        RespReq
                end
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec dispatch(atom(), binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dispatch(list, Method, Req0, State) -> list(Method, Req0, State);
dispatch(detail, Method, Req0, State) -> detail(Method, Req0, State);
dispatch(update, Method, Req0, State) -> update(Method, Req0, State);
dispatch(dissolve, Method, Req0, State) -> dissolve(Method, Req0, State);
dispatch(search, Method, Req0, State) -> search(Method, Req0, State);
dispatch(members, Method, Req0, State) -> members(Method, Req0, State);
dispatch(kick_member, Method, Req0, State) -> kick_member(Method, Req0, State);
dispatch(_, _Method, Req0, _State) -> Req0.

%% @doc 群组列表
-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {Page, Size} = elib_param:page(Req0),
            {ok, Status} = elib_param:int(status, Req0, -1),
            {ok, Type} = elib_param:int(type, Req0, -1),
            {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),

            Where = build_where(Status, Type, Keyword),
            {ok, P} = group_ds:page(Page, Size, Where, <<"created_at DESC">>),
            P2 = normalize_group_payload(P),
            elib_response:success(Req0, P2)
    end.

%% @doc 群组详情
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            case Gid > 0 of
                true ->
                    Column =
                        <<"id,title,avatar,introduction,owner_uid,creator_uid,member_count,member_max,type,join_limit,status,created_at">>,
                    Group = group_ds:find_by_id(Gid, Column),
                    case map_size(Group) > 0 of
                        true ->
                            % 获取群主信息
                            OwnerUid = maps:get(<<"owner_uid">>, Group),
                            Owner = user_ds:find_by_id(OwnerUid, <<"id,nickname,avatar">>),
                            Group2 = normalize_group(Group),
                            Result = Group2#{owner => normalize_user(Owner)},
                            elib_response:success(Req0, Result);
                        false ->
                            elib_response:error(Req0, "群组不存在")
                    end;
                false ->
                    elib_response:error(Req0, "参数错误")
            end
    end.

%% @doc 更新群组信息
-spec update(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
update(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            case Gid > 0 of
                true ->
                    PostVals = elib_param:post(Req0),
                    Data0 = #{id => Gid},
                    Data1 = maybe_put(Data0, title, maps:get(<<"title">>, PostVals, undefined)),
                    Data2 = maybe_put(
                        Data1, introduction, maps:get(<<"introduction">>, PostVals, undefined)
                    ),
                    Data3 = maybe_put(
                        Data2, join_limit, maps:get(<<"join_limit">>, PostVals, undefined)
                    ),
                    Data4 = maybe_put(
                        Data3, member_max, maps:get(<<"member_max">>, PostVals, undefined)
                    ),
                    case map_size(Data4) > 1 of
                        true ->
                            case group_ds:update(Data4) of
                                {ok, _} ->
                                    elib_response:success(Req0, #{}, "操作成功");
                                {error, Reason} ->
                                    ?ERROR_LOG(["update group error: ", Reason]),
                                    elib_response:error(Req0, "操作失败")
                            end;
                        false ->
                            elib_response:error(Req0, "无有效更新字段")
                    end;
                false ->
                    elib_response:error(Req0, "参数错误")
            end
    end;
update(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 解散群组
-spec dissolve(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
dissolve(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            case Gid > 0 of
                true ->
                    case group_ds:update(#{id => Gid, status => -1}) of
                        {ok, _} ->
                            _ = audit_group_governance(
                                maps:get(adm_user_id, State, 0),
                                Gid,
                                <<"dissolve_group">>,
                                Gid,
                                #{<<"scope">> => <<"group">>}
                            ),
                            elib_response:success(Req0, #{}, "操作成功");
                        {error, Reason} ->
                            ?ERROR_LOG(["dissolve group error: ", Reason]),
                            elib_response:error(Req0, "操作失败")
                    end;
                false ->
                    elib_response:error(Req0, "参数错误")
            end
    end.

%% @doc 搜索群组
-spec search(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
search(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
            {Page, Size} = elib_param:page(Req0),

            case byte_size(Keyword) > 0 of
                true ->
                    Where = #{
                        'or' => [
                            #{title => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}},
                            #{
                                introduction =>
                                    {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}
                            }
                        ]
                    },
                    {ok, P} = group_ds:page(Page, Size, Where, <<"created_at DESC">>),
                    P2 = normalize_group_payload(P),
                    elib_response:success(Req0, P2);
                false ->
                    elib_response:error(Req0, "请输入搜索关键词")
            end
    end.

%% @doc 群组成员列表
-spec members(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
members(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Gid = parse_gid_param(Req0),
            {Page, Size} = elib_param:page(Req0),

            case Gid > 0 of
                true ->
                    Column = <<"id,group_id,user_id,nickname,avatar,role,status,joined_at">>,
                    {ok, P} = group_member_ds:page_by_gid(Gid, Page, Size, Column),
                    P2 = normalize_member_payload(P),
                    elib_response:success(Req0, P2);
                false ->
                    elib_response:error(Req0, "参数错误")
            end
    end.

-spec normalize_user(map()) -> map().
normalize_user(User) ->
    elib_id:tsid_keys_to_bin(User, [<<"id">>]).

%% @doc 管理员踢出群成员
-spec kick_member(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
kick_member(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"groups:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            GidRaw = maps:get(<<"gid">>, PostVals, <<>>),
            UidRaw = maps:get(<<"uid">>, PostVals, <<>>),
            Gid = normalize_positive_int(GidRaw),
            Uid = normalize_positive_int(UidRaw),
            AdminUid = maps:get(adm_user_id, State, 0),
            case Gid > 0 andalso Uid > 0 of
                false ->
                    elib_response:error(Req0, <<"参数错误"/utf8>>);
                true ->
                    case group_member_logic:leave(Uid, Gid, AdminUid) of
                        ok ->
                            ok;
                        {error, LeaveErr} ->
                            ?ERROR_LOG("kick_member leave failed gid=~p uid=~p err=~p", [
                                Gid, Uid, LeaveErr
                            ])
                    end,
                    _ = audit_group_governance(
                        AdminUid,
                        Gid,
                        <<"kick_member">>,
                        Uid,
                        #{<<"scope">> => <<"member">>}
                    ),
                    elib_response:success(Req0, #{gid => Gid, uid => Uid}, "操作成功")
            end
    end;
kick_member(_, Req0, _State) ->
    Req0.

-spec maybe_put(map(), atom(), term()) -> map().
maybe_put(Data, _Key, undefined) -> Data;
maybe_put(Data, _Key, <<>>) -> Data;
maybe_put(Data, Key, Value) -> Data#{Key => Value}.

build_where(Status, Type, Keyword) ->
    Base = build_where_status_type(Status, Type),
    case byte_size(Keyword) > 0 of
        true ->
            Esc = elib_pg:escape_like(Keyword),
            Like = <<"%", Esc/binary, "%">>,
            Base#{
                'or' => [
                    #{title => {like, Like}},
                    #{introduction => {like, Like}}
                ]
            };
        false ->
            Base
    end.

build_where_status_type(Status, Type) when Status >= 0, Type >= 0 ->
    #{status => Status, type => Type};
build_where_status_type(Status, _Type) when Status >= 0 -> #{status => Status};
build_where_status_type(_Status, Type) when Type >= 0 -> #{type => Type};
build_where_status_type(_Status, _Type) ->
    #{}.

normalize_group_payload(Payload) ->
    List = maps:get(list, Payload, []),
    List2 = [normalize_group(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

-spec normalize_group(map()) -> map().
normalize_group(Group) ->
    elib_id:tsid_keys_to_bin(Group, [<<"id">>, <<"owner_uid">>, <<"creator_uid">>]).

-spec normalize_member_payload(map()) -> map().
normalize_member_payload(Payload) ->
    List = maps:get(list, Payload, []),
    List2 = [normalize_member(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

-spec normalize_member(map()) -> map().
normalize_member(Member) ->
    elib_id:tsid_keys_to_bin(Member, [<<"id">>, <<"group_id">>, <<"user_id">>]).
