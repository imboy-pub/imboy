-module(adm_user_handler).
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
    Req1 = case Action of
        list -> list(Method, Req0, State);
        detail -> detail(Method, Req0, State);
        ban -> ban(Method, Req0, State);
        unban -> unban(Method, Req0, State);
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
    {ok, P} = user_repo:page(Page, Size, Where, <<"created_at DESC">>),
    P2 = normalize_user_payload(P),
    elib_response:success(Req0, P2).

%% @doc 用户详情
-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, _State) ->
    Uid = parse_uid_param(Req0),
    case Uid > 0 of
        true ->
            Column = <<"id,account,nickname,mobile,email,avatar,gender,region,sign,status,experience,created_at">>,
            User = user_repo:find_by_id(Uid, Column),
            case map_size(User) > 0 of
                true ->
                    % 获取用户设备数
                    DeviceCount = user_device_repo:count_by_uid(Uid),
                    % 获取用户好友数
                    FriendCount = friend_repo:count_by_uid(Uid),
                    % 获取用户群组数
                    GroupCount = group_member_repo:count_by_uid(Uid),
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
ban(<<"POST">>, Req0, _State) ->
    Uid = parse_uid_param(Req0),
    case Uid > 0 of
        true ->
            case user_repo:update(Uid, #{status => 0}) of
                {ok, _} ->
                    elib_response:success(Req0, #{}, "操作成功");
                {error, Reason} ->
                    ?ERROR_LOG(["ban user error: ", Reason]),
                    elib_response:error(Req0, "操作失败")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 解封用户
-spec unban(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
unban(<<"POST">>, Req0, _State) ->
    Uid = parse_uid_param(Req0),
    case Uid > 0 of
        true ->
            case user_repo:update(Uid, #{status => 1}) of
                {ok, _} ->
                    elib_response:success(Req0, #{}, "操作成功");
                {error, Reason} ->
                    ?ERROR_LOG(["unban user error: ", Reason]),
                    elib_response:error(Req0, "操作失败")
            end;
        false ->
            elib_response:error(Req0, "参数错误")
    end.

%% @doc 搜索用户
-spec search(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
search(<<"GET">>, Req0, _State) ->
    {ok, Keyword} = elib_param:binary(keyword, Req0, <<>>),
    {Page, Size} = elib_param:page(Req0),
    
    case byte_size(Keyword) > 0 of
        true ->
            Where = #{
                'or' => [
                    #{account => {like, <<"%", Keyword/binary, "%">>}},
                    #{nickname => {like, <<"%", Keyword/binary, "%">>}},
                    #{email => {like, <<"%", Keyword/binary, "%">>}},
                    #{mobile => {like, <<"%", Keyword/binary, "%">>}}
                ]
            },
            {ok, P} = user_repo:page(Page, Size, Where, <<"created_at DESC">>),
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
                    true -> Where0#{name => {op, <<"LIKE">>, <<"%", Keyword/binary, "%">>}};
                    false -> Where0
                end,
            Payload = user_tag_logic:page(Scene, Page, Size, Where, <<"id desc">>),
            elib_response:success(Req0, maps:remove(items, Payload))
    end.

%% @doc 删除用户标签
-spec tag_delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
tag_delete(<<"POST">>, Req0, _State) ->
    PostVals = elib_param:post(Req0),
    Uid = parse_hashid_or_int(maps:get(<<"uid">>, PostVals, 0)),
    Scene = parse_scene(maps:get(<<"scene">>, PostVals, <<"friend">>)),
    TagFromBody = maps:get(<<"tag">>, PostVals, <<>>),
    TagId = parse_hashid_or_int(maps:get(<<"tag_id">>, PostVals, 0)),
    Tag =
        case {TagFromBody, TagId > 0, Uid > 0, Scene > 0} of
            {<<>>, true, true, true} ->
                Tb = user_tag_repo:tablename(),
                elib_pg:pluck_value(
                    Tb,
                    <<"name">>,
                    #{id => TagId, creator_user_id => Uid, scene => Scene},
                    <<>>
                );
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
                        Where1#{tag => {op, <<"LIKE">>, <<"%", Tag/binary, ",%">>}};
                    false ->
                        Where1
                end,
            Where =
                case byte_size(Keyword) > 0 of
                    true ->
                        Like = <<"%", Keyword/binary, "%">>,
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
            Tb = user_collect_repo:tablename(),
            case elib_pg:page_with_total(Tb, Column, Where, Order, Page, Size) of
                {ok, Payload0} ->
                    List = maps:get(list, Payload0, []),
                    Payload = maps:put(list, elib_response:json_decode_list_field(List, <<"info">>), Payload0),
                    elib_response:success(Req0, maps:remove(items, Payload));
                {error, Reason} ->
                    ?ERROR_LOG(["adm collect list error: ", Reason]),
                    elib_response:error(Req0, "查询失败")
            end
    end.

%% @doc 删除用户收藏
-spec collect_remove(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
collect_remove(<<"POST">>, Req0, _State) ->
    PostVals = elib_param:post(Req0),
    Uid = parse_hashid_or_int(maps:get(<<"uid">>, PostVals, 0)),
    KindId = maps:get(<<"kind_id">>, PostVals, <<>>),
    case {Uid > 0, KindId =/= <<>>} of
        {false, _} ->
            elib_response:error(Req0, "参数错误");
        {_, false} ->
            elib_response:error(Req0, "kind_id不能为空");
        _ ->
            _ = user_collect_logic:remove(Uid, KindId),
            elib_response:success(Req0, #{}, "操作成功")
    end.

%% @doc 构建查询条件
-spec build_where(integer(), binary()) -> map().
build_where(Status, Keyword) when byte_size(Keyword) > 0 ->
    KeywordWhere = #{
        'or' => [
            #{account => {like, <<"%", Keyword/binary, "%">>}},
            #{nickname => {like, <<"%", Keyword/binary, "%">>}},
            #{email => {like, <<"%", Keyword/binary, "%">>}},
            #{mobile => {like, <<"%", Keyword/binary, "%">>}}
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
    Uid0 =
        case catch elib_param:int(uid, Req0, 0) of
            {ok, Uid} when is_integer(Uid), Uid > 0 ->
                Uid;
            _ ->
                0
        end,
    case Uid0 > 0 of
        true ->
            Uid0;
        false ->
            case catch elib_param:binary(uid, Req0, <<>>) of
                {ok, UidBin} ->
                    parse_hashid_or_int(UidBin);
                _ ->
                    0
            end
    end.

-spec parse_hashid_or_int(term()) -> integer().
parse_hashid_or_int(Value) when is_integer(Value), Value > 0 ->
    Value;
parse_hashid_or_int(Value) when is_list(Value) ->
    parse_hashid_or_int(ec_cnv:to_binary(Value));
parse_hashid_or_int(Value) when is_binary(Value), Value =/= <<>> ->
    case elib_type:is_numeric(Value) of
        true ->
            ec_cnv:to_integer(Value);
        false ->
            case catch elib_hashids:decode(Value) of
                Id when is_integer(Id), Id > 0 -> Id;
                _ -> 0
            end
    end;
parse_hashid_or_int(_) ->
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
-spec normalize_user(map()) -> map().
normalize_user(User) ->
    elib_hashids:replace_fields(User, [<<"id">>]).
