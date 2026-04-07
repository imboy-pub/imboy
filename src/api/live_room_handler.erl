-module(live_room_handler).
%% 直播间 HTTP API 处理器
%% 提供直播间的创建、开始/停止直播、查询等功能

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(list, Req, State) -> list(Req, State);
handle_action(my_list, Req, State) -> my_list(Req, State);
handle_action(create, Req, State) -> create(Req, State);
handle_action(start, Req, State) -> start(Req, State);
handle_action(stop, Req, State) -> stop(Req, State);
handle_action(detail, Req, State) -> detail(Req, State);
handle_action(false, Req, _State) -> Req.

%% @doc 查询直播中的房间列表（status=1），分页
-spec list(cowboy_req:req(), map()) -> cowboy_req:req().
list(Req0, _State) ->
    {Page, Size} = elib_param:page(Req0),
    case live_room_repo:page_active(Page, Size) of
        {ok, Data} ->
            Rows = maps:get(<<"list">>, Data, []),
            Rows2 = [encode_room(R) || R <- Rows],
            elib_response:success(Req0, Data#{<<"list">> => Rows2}, "success.");
        {error, _Reason} ->
            elib_response:error(Req0, <<"查询失败"/utf8>>)
    end.

%% @doc 查询当前用户的直播间列表
-spec my_list(cowboy_req:req(), map()) -> cowboy_req:req().
my_list(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    {Page, Size} = elib_param:page(Req0),
    case live_room_repo:page_by_uid(CurrentUid, Page, Size) of
        {ok, Data} ->
            Rows = maps:get(<<"list">>, Data, []),
            Rows2 = [encode_room(R) || R <- Rows],
            elib_response:success(Req0, Data#{<<"list">> => Rows2}, "success.");
        {error, _Reason} ->
            elib_response:error(Req0, <<"查询失败"/utf8>>)
    end.

%% @doc 创建直播间
%% 参数：title（必填），cover（可选）
-spec create(cowboy_req:req(), map()) -> cowboy_req:req().
create(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    Title = maps:get(<<"title">>, PostVals, <<>>),
    case Title of
        <<>> ->
            elib_response:error(Req0, <<"直播间标题不能为空"/utf8>>);
        _ when byte_size(Title) > 100 ->
            elib_response:error(Req0, <<"标题不能超过100字节"/utf8>>);
        _ ->
            Cover = maps:get(<<"cover">>, PostVals, <<>>),
            case byte_size(Cover) > 255 of
                true ->
                    elib_response:error(Req0, <<"封面URL不合法"/utf8>>);
                false ->
                    StreamKey = generate_stream_key(),
                    Now = elib_dt:now(),
                    Data = #{
                        user_id => CurrentUid,
                        title => Title,
                        cover => Cover,
                        stream_key => StreamKey,
                        status => 0,
                        viewer_count => 0,
                        tag_id => 0,
                        scene => 1,
                        created_at => Now,
                        updated_at => Now
                    },
                    case live_room_repo:create(Data) of
                        {ok, RoomId, _Row} ->
                            Room = Data#{
                                <<"id">> => RoomId,
                                <<"user_id">> => CurrentUid,
                                <<"stream_key">> => StreamKey,
                                <<"status">> => 0,
                                <<"viewer_count">> => 0
                            },
                            elib_response:success(Req0, Room, "success.");
                        {error, _Reason} ->
                            elib_response:error(Req0, <<"创建直播间失败"/utf8>>)
                    end
            end
    end.

%% @doc 开始直播（更新 status=1）
%% 只有房间创建者才能操作
-spec start(cowboy_req:req(), map()) -> cowboy_req:req().
start(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    RoomIdEnc = maps:get(<<"room_id">>, PostVals, <<>>),
    case RoomIdEnc of
        <<>> ->
            elib_response:error(Req0, <<"直播间ID不能为空"/utf8>>);
        _ ->
            RoomId = ec_cnv:to_integer(RoomIdEnc),
            case live_room_repo:find_by_id(RoomId) of
                #{<<"user_id">> := OwnerId} = _Room when OwnerId =:= CurrentUid ->
                    Now = elib_dt:now(),
                    live_room_repo:update(RoomId, #{status => 1, updated_at => Now}),
                    elib_response:success(Req0, #{}, "success.");
                #{<<"user_id">> := _OtherUid} ->
                    elib_response:error(Req0, <<"无权操作此直播间"/utf8>>);
                #{} ->
                    elib_response:error(Req0, <<"直播间不存在"/utf8>>);
                {error, _} ->
                    elib_response:error(Req0, <<"直播间不存在"/utf8>>)
            end
    end.

%% @doc 停止直播（更新 status=2）
%% 只有房间创建者才能操作
-spec stop(cowboy_req:req(), map()) -> cowboy_req:req().
stop(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    PostVals = elib_param:post(Req0),
    RoomIdEnc = maps:get(<<"room_id">>, PostVals, <<>>),
    case RoomIdEnc of
        <<>> ->
            elib_response:error(Req0, <<"直播间ID不能为空"/utf8>>);
        _ ->
            RoomId = ec_cnv:to_integer(RoomIdEnc),
            case live_room_repo:find_by_id(RoomId) of
                #{<<"user_id">> := OwnerId} = _Room when OwnerId =:= CurrentUid ->
                    Now = elib_dt:now(),
                    live_room_repo:update(RoomId, #{status => 2, updated_at => Now}),
                    elib_response:success(Req0, #{}, "success.");
                #{<<"user_id">> := _OtherUid} ->
                    elib_response:error(Req0, <<"无权操作此直播间"/utf8>>);
                #{} ->
                    elib_response:error(Req0, <<"直播间不存在"/utf8>>);
                {error, _} ->
                    elib_response:error(Req0, <<"直播间不存在"/utf8>>)
            end
    end.

%% @doc 获取直播间详情
%% stream_key 仅对房间所有者返回
-spec detail(cowboy_req:req(), map()) -> cowboy_req:req().
detail(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    RoomIdEnc = proplists:get_value(<<"room_id">>, Qs, <<>>),
    case RoomIdEnc of
        <<>> ->
            elib_response:error(Req0, <<"直播间ID不能为空"/utf8>>);
        _ ->
            RoomId = ec_cnv:to_integer(RoomIdEnc),
            case live_room_repo:find_by_id(RoomId) of
                #{<<"user_id">> := OwnerId} = Room when map_size(Room) > 0 ->
                    Room2 = encode_room(Room),
                    %% 仅房间所有者可看到 stream_key
                    Room3 = case OwnerId =:= CurrentUid of
                        true -> Room2;
                        false -> maps:remove(<<"stream_key">>, Room2)
                    end,
                    elib_response:success(Req0, Room3, "success.");
                _ ->
                    elib_response:error(Req0, <<"直播间不存在"/utf8>>)
            end
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc 生成推流密钥（16字节随机数，base64 编码）
-spec generate_stream_key() -> binary().
generate_stream_key() ->
    RawBytes = crypto:strong_rand_bytes(16),
    base64:encode(RawBytes).

%% @doc 对房间数据进行格式化（列表用，不含推流地址）
-spec encode_room(map()) -> map().
encode_room(Room) ->
    Room.

