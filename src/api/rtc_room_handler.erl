-module(rtc_room_handler).

%%%
% RTC 房间 HTTP 入口（LiveKit SFU）
% RTC room HTTP entry (LiveKit SFU)
%
% 薄适配层：解析请求参数后转 rtc_room_logic。
%%%

-behavior(cowboy_rest).

-export([init/2]).

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            join ->
                join(cowboy_req:method(Req0), Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 加入 RTC 房间，返回 LiveKit 接入凭证
%% POST /api/v1/rtc/room/join  {kind, target_id, did?}
-spec join(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
join(<<"POST">>, Req0, State) ->
    Uid = maps:get(current_uid, State),
    PostVals = elib_req:post_params(Req0),
    Kind = maps:get(<<"kind">>, PostVals, <<>>),
    TargetId0 = maps:get(<<"target_id">>, PostVals, 0),
    Did = maps:get(<<"did">>, PostVals, <<"default">>),
    TargetId = ec_cnv:to_integer(TargetId0),
    if
        TargetId =< 0 ->
            elib_response:error(Req0, "target_id 无效");
        true ->
            case rtc_room_logic:join(Uid, Did, Kind, TargetId) of
                {ok, Grant} ->
                    elib_response:success(Req0, Grant, "success.");
                {error, Msg} ->
                    elib_response:error(Req0, Msg)
            end
    end;
join(_, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
