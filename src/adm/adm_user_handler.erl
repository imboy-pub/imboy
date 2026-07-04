-module(adm_user_handler).
-compile([nowarn_deprecated_catch]).
%%%
% adm_user 控制器模块
% 用户管理 API
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

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
            list -> list(Method, Req0, State);
            detail -> detail(Method, Req0, State);
            ban -> ban(Method, Req0, State);
            unban -> unban(Method, Req0, State);
            force_logout -> force_logout(Method, Req0, State);
            devices -> devices(Method, Req0, State);
            device_kick -> device_kick(Method, Req0, State);
            search -> search(Method, Req0, State);
            tag_list -> tag_list(Method, Req0, State);
            tag_delete -> tag_delete(Method, Req0, State);
            collect_list -> collect_list(Method, Req0, State);
            collect_remove -> collect_remove(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 用户列表
-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    {ok, Status} = elib_param:int(status, Req0, -1),
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),

    Where = build_where(Status, Keyword),
    {ok, P} = user_ds:page(Page, Size, Where, <<"created_at DESC">>),
    P2 = normalize_user_payload(P),
    elib_response:success(Req0, P2).

%% @doc 用户详情
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, _State) ->
    Uid = parse_uid_param(Req0),
    case Uid > 0 of
        true ->
            Column =
                <<"id,account,nickname,mobile,email,avatar,gender,region,sign,status,experience,created_at,updated_at">>,
            User = user_ds:find_by_id(Uid, Column),
            case map_size(User) > 0 of
                true ->
                    % 获取用户设备数
                    DeviceCount = user_device_ds:count_by_uid(Uid),
                    % 获取用户好友数
                    FriendCount = friend_ds:count_by_uid(Uid),
                    % 获取用户群组数
                    GroupCount = group_member_ds:count_by_uid(Uid),
                    Result = User#{
                        device_count => DeviceCount,
                        friend_count => FriendCount,
                        group_count => GroupCount
                    },
                    Result2 = normalize_user(Result),
                    elib_response:success(Req0, Result2);
                false ->
                    elib_response:error(Req0, "用户不存在")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 封禁用户
-spec ban(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
ban(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"users:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            set_user_status(Req0, State, 0, <<"ban_user">>)
    end.

%% @doc 解封用户
-spec unban(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
unban(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"users:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            set_user_status(Req0, State, 1, <<"unban_user">>)
    end.

%% @doc 封禁/解封共用：更新用户 status 并写管理员操作审计日志（含前后值与原因）
-spec set_user_status(cowboy_req:req(), map(), integer(), binary()) -> cowboy_req:req().
set_user_status(Req0, State, NewStatus, Action) ->
    Uid = parse_uid_param(Req0),
    case Uid > 0 of
        true ->
            {ok, OpReason} = elib_param:binary(reason, Req0, <<>>),
            OldStatus = maps:get(<<"status">>, user_ds:find_by_id(Uid, <<"status">>), null),
            case user_ds:update(Uid, #{status => NewStatus}) of
                {ok, _} ->
                    _ = adm_operation_log_ds:insert(
                        maps:get(adm_user_id, State, 0),
                        Action,
                        Uid,
                        <<"user">>,
                        #{
                            <<"before">> => #{<<"status">> => OldStatus},
                            <<"after">> => #{<<"status">> => NewStatus},
                            <<"reason">> => OpReason
                        },
                        elib_req:peer_ip(Req0)
                    ),
                    elib_response:success(Req0, #{}, "操作成功");
                {error, Reason} ->
                    ?ERROR_LOG(["set user status error: ", Action, Reason]),
                    elib_response:error(Req0, "操作失败")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 强制下线用户所有设备（GAP-06）
%% POST /adm/user/force_logout?uid=<uid>
-spec force_logout(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
force_logout(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"users:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Uid = parse_uid_param(Req0),
            AdmUserId = maps:get(adm_user_id, State, 0),
            case Uid > 0 of
                true ->
                    _ = user_device_logic:kick_all_other_devices(
                        Uid, {<<"all">>, <<"admin_action">>}
                    ),
                    % GAP-01 关联：写强制下线审计日志（type=120）
                    _ = elib_async:async(fun() ->
                        {ok, LogBody} = jsone_encode:encode(
                            #{
                                <<"operator">> => AdmUserId,
                                <<"reason">> => <<"admin_force_logout">>
                            },
                            [native_utf8]
                        ),
                        _ = user_log_ds:add_internal(undefined, 120, Uid, LogBody, elib_dt:now())
                    end),
                    Ip = elib_req:peer_ip(Req0),
                    _ = adm_operation_log_ds:insert(
                        AdmUserId, <<"force_logout">>, Uid, <<"user">>, #{}, Ip
                    ),
                    elib_response:success(Req0, #{}, "操作成功");
                false ->
                    elib_response:error(Req0, "参数错误")
            end
    end;
force_logout(_, Req0, _State) ->
    elib_response:error(Req0, "Method Not Allowed").

%% @doc 列出指定用户的设备（分页，含在线状态）
%% GET /adm/user/devices?user_id=<uid>
-spec devices(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
devices(<<"GET">>, Req0, _State) ->
    Uid = parse_uid_param(Req0),
    case Uid > 0 of
        true ->
            {Page, Size} = elib_param:page(Req0),
            Payload = user_device_logic:page(Uid, Page, Size),
            elib_response:success(Req0, Payload);
        false ->
            elib_response:error(Req0, "参数错误")
    end;
devices(_, Req0, _State) ->
    elib_response:error(Req0, "Method Not Allowed").

%% @doc 踢出指定用户的单个设备（结束该设备的 WS 会话，强制重新登录）
%% POST /adm/user/device/kick {user_id, did}
-spec device_kick(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
device_kick(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"users:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            Uid = parse_id(
                maps:get(<<"user_id">>, PostVals, maps:get(<<"uid">>, PostVals, 0))
            ),
            DID = normalize_did(maps:get(<<"did">>, PostVals, <<>>)),
            AdmUserId = maps:get(adm_user_id, State, 0),
            case {Uid > 0, DID =/= <<>>} of
                {false, _} ->
                    elib_response:error(Req0, "参数错误");
                {_, false} ->
                    elib_response:error(Req0, "did不能为空");
                _ ->
                    %% kick_device 仅结束在线会话；设备离线时无会话可踢，
                    %% 视作已登出，同样返回成功（token 为 uid 级，重连须重认证）。
                    _ = user_device_logic:kick_device(Uid, <<>>, DID),
                    _ = adm_operation_log_ds:insert(
                        AdmUserId,
                        <<"kick_device">>,
                        Uid,
                        <<"user">>,
                        #{<<"did">> => DID},
                        elib_req:peer_ip(Req0)
                    ),
                    elib_response:success(Req0, #{}, "操作成功")
            end
    end;
device_kick(_, Req0, _State) ->
    elib_response:error(Req0, "Method Not Allowed").

-spec normalize_did(term()) -> binary().
normalize_did(Value) when is_binary(Value) ->
    Value;
normalize_did(Value) when is_list(Value) ->
    ec_cnv:to_binary(Value);
normalize_did(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_did(_) ->
    <<>>.

%% @doc 搜索用户
-spec search(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
search(<<"GET">>, Req0, _State) ->
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {Page, Size} = elib_param:page(Req0),

    case byte_size(Keyword) > 0 of
        true ->
            Where = #{
                'or' => [
                    #{account => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}},
                    #{nickname => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}},
                    #{email => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}},
                    #{mobile => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}}
                ]
            },
            {ok, P} = user_ds:page(Page, Size, Where, <<"created_at DESC">>),
            P2 = normalize_user_payload(P),
            elib_response:success(Req0, P2);
        false ->
            elib_response:error(Req0, "请输入搜索关键词")
    end.

%% @doc 用户标签列表
-spec tag_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
tag_list(<<"GET">>, Req0, _State) ->
    Uid = parse_uid_param(Req0),
    {Page, Size} = elib_param:page(Req0),
    {ok, SceneRaw} = elib_param:binary(scene, Req0, <<"friend">>),
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    Scene = parse_scene(SceneRaw),
    case {Uid > 0, Scene > 0} of
        {false, _} ->
            elib_response:error(Req0, "参数错误");
        {_, false} ->
            elib_response:error(Req0, "不支持的 Scene");
        _ ->
            Where0 = #{creator_user_id => Uid, scene => Scene},
            Where =
                case byte_size(Keyword) > 0 of
                    true ->
                        EscKw = elib_pg:escape_like(Keyword),
                        Where0#{name => {op, <<"LIKE">>, <<"%", EscKw/binary, "%">>}};
                    false ->
                        Where0
                end,
            Payload = user_tag_logic:page(Scene, Page, Size, Where, <<"id desc">>),
            elib_response:success(Req0, maps:remove(items, Payload))
    end.

%% @doc 删除用户标签
-spec tag_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
tag_delete(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"users:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            Uid = parse_id(maps:get(<<"uid">>, PostVals, 0)),
            Scene = parse_scene(maps:get(<<"scene">>, PostVals, <<"friend">>)),
            TagFromBody = maps:get(<<"tag">>, PostVals, <<>>),
            TagId = parse_id(maps:get(<<"tag_id">>, PostVals, 0)),
            Tag =
                case {TagFromBody, TagId > 0, Uid > 0, Scene > 0} of
                    {<<>>, true, true, true} ->
                        user_tag_ds:find_name_by_id(TagId, Uid, Scene);
                    _ ->
                        TagFromBody
                end,
            case {Uid > 0, Scene > 0, Tag =/= <<>>} of
                {false, _, _} ->
                    elib_response:error(Req0, "参数错误");
                {_, false, _} ->
                    elib_response:error(Req0, "不支持的 Scene");
                {_, _, false} ->
                    elib_response:error(Req0, "tag不能为空");
                _ ->
                    _ = user_tag_logic:delete(Uid, Scene, Tag),
                    elib_response:success(Req0, #{}, "操作成功")
            end
    end.

%% @doc 用户收藏列表
-spec collect_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
collect_list(<<"GET">>, Req0, _State) ->
    Uid = parse_uid_param(Req0),
    {Page, Size} = elib_param:page(Req0),
    {ok, Kind} = elib_param:int(kind, Req0, 0),
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {ok, Tag} = elib_param:binary(tag, Req0, <<>>),
    {ok, OrderBy} = elib_param:binary(order, Req0, <<>>),
    case Uid > 0 of
        false ->
            elib_response:error(Req0, "参数错误");
        true ->
            Where0 = #{user_id => Uid, status => 1},
            Where1 =
                case Kind of
                    0 -> Where0;
                    _ -> Where0#{kind => Kind}
                end,
            Where2 =
                case byte_size(Tag) > 0 of
                    true ->
                        EscTag = elib_pg:escape_like(Tag),
                        Where1#{tag => {op, <<"LIKE">>, <<"%", EscTag/binary, ",%">>}};
                    false ->
                        Where1
                end,
            Where =
                case byte_size(Keyword) > 0 of
                    true ->
                        EscKw2 = elib_pg:escape_like(Keyword),
                        Like = <<"%", EscKw2/binary, "%">>,
                        Where2#{
                            <<"__or">> => [
                                #{source => {op, <<"LIKE">>, Like}},
                                #{remark => {op, <<"LIKE">>, Like}},
                                #{info => {op, <<"LIKE">>, Like}}
                            ]
                        };
                    false ->
                        Where2
                end,
            Order =
                case OrderBy of
                    <<"recent_use">> -> <<"updated_at desc, id desc">>;
                    _ -> <<"id desc">>
                end,
            Info = elib_hasher:decoded_field(<<"info">>),
            Column = <<"kind, kind_id, source, created_at, updated_at, tag, ", Info/binary>>,
            case user_collect_ds:page(Column, Where, Order, Page, Size) of
                {ok, Payload0} ->
                    List = maps:get(list, Payload0, []),
                    Payload = maps:put(
                        list, elib_response:json_decode_list_field(List, <<"info">>), Payload0
                    ),
                    elib_response:success(Req0, maps:remove(items, Payload));
                {error, Reason} ->
                    ?ERROR_LOG(["adm collect list error: ", Reason]),
                    elib_response:error(Req0, "查询失败")
            end
    end.

%% @doc 删除用户收藏
-spec collect_remove(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
collect_remove(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"users:update">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            Uid = parse_id(maps:get(<<"uid">>, PostVals, 0)),
            KindId = maps:get(<<"kind_id">>, PostVals, <<>>),
            case {Uid > 0, KindId =/= <<>>} of
                {false, _} ->
                    elib_response:error(Req0, "参数错误");
                {_, false} ->
                    elib_response:error(Req0, "kind_id不能为空");
                _ ->
                    _ = user_collect_logic:remove(Uid, KindId),
                    elib_response:success(Req0, #{}, "操作成功")
            end
    end.

%% @doc 构建查询条件
-spec build_where(integer(), binary()) -> map().
build_where(Status, Keyword) when byte_size(Keyword) > 0 ->
    KeywordWhere = #{
        'or' => [
            #{account => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}},
            #{nickname => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}},
            #{email => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}},
            #{mobile => {like, <<"%", (elib_pg:escape_like(Keyword))/binary, "%">>}}
        ]
    },
    case Status >= 0 of
        true -> #{status => Status, 'and' => KeywordWhere};
        false -> KeywordWhere
    end;
build_where(Status, _Keyword) ->
    case Status >= 0 of
        true -> #{status => Status};
        false -> #{}
    end.

-spec parse_uid_param(cowboy_req:req()) -> integer().
parse_uid_param(Req0) ->
    % T22/BE-17: prefer user_id, fall back to uid
    Uid0 = lists:foldl(
        fun
            (_, Acc) when Acc > 0 ->
                Acc;
            (Key, _) ->
                case catch elib_param:int(Key, Req0, 0) of
                    {ok, Uid} when is_integer(Uid), Uid > 0 -> Uid;
                    _ -> 0
                end
        end,
        0,
        [user_id, uid]
    ),
    case Uid0 > 0 of
        true ->
            Uid0;
        false ->
            lists:foldl(
                fun
                    (_, Acc) when Acc > 0 ->
                        Acc;
                    (Key, _) ->
                        case catch elib_param:binary(Key, Req0, <<>>) of
                            {ok, UidBin} -> parse_id(UidBin);
                            _ -> 0
                        end
                end,
                0,
                [user_id, uid]
            )
    end.

-spec parse_id(term()) -> integer().
parse_id(Value) when is_integer(Value), Value > 0 ->
    Value;
parse_id(Value) when is_list(Value) ->
    parse_id(ec_cnv:to_binary(Value));
parse_id(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            ec_cnv:to_integer(Value);
        false ->
            case catch ec_cnv:to_integer(Value) of
                Id when is_integer(Id), Id > 0 -> Id;
                _ -> 0
            end
    end;
parse_id(_) ->
    0.

-spec parse_scene(term()) -> integer().
parse_scene(1) ->
    1;
parse_scene(2) ->
    2;
parse_scene(<<"1">>) ->
    1;
parse_scene(<<"2">>) ->
    2;
parse_scene(<<"collect">>) ->
    1;
parse_scene(<<"friend">>) ->
    2;
parse_scene(Value) when is_list(Value) ->
    parse_scene(ec_cnv:to_binary(Value));
parse_scene(_) ->
    0.

%% ===================================================================
%% 数据规范化函数（ID编码）
%% ===================================================================

%% @doc 规范化用户分页数据（编码ID字段）
-spec normalize_user_payload(map()) -> map().
normalize_user_payload(Payload) ->
    List = maps:get(list, Payload, []),
    List2 = [normalize_user(Item) || Item <- List],
    maps:remove(items, Payload#{list => List2}).

%% @doc 规范化单条用户数据（编码ID字段）
%% Admin API 将 TSID 整数转为字符串，避免 JS 精度丢失。
-spec normalize_user(map()) -> map().
normalize_user(User) ->
    elib_id:tsid_keys_to_bin(User, [<<"id">>]).
