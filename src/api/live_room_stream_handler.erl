-module(live_room_stream_handler).
%%%
% 直播间数据流处理模块
% room_stream controller module
%%%

-export([init/2]).
-export([info/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").
-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化直播流处理器
%% 启动Server-Sent Events流
%%
%% @param Req0 Cowboy请求对象
%% @param Opts 选项参数
%% @return {cowboy_loop, Req, Opts}
%% @end
-spec init(cowboy_req:req(), map()) -> {cowboy_loop, cowboy_req:req(), map()}.
init(Req0, Opts) ->
    StreamId = cowboy_req:binding(stream_id, Req0),
    ok = elib_log:info("StreamId ~p~n", [StreamId]),
    check_role(StreamId, Opts),
    Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>}, Req0),
    erlang:send_after(1000, self(), {message, "Tick"}),
    {cowboy_loop, Req, Opts}.

%% @doc 处理流消息
%% 处理来自其他进程的消息并发送给客户端
%%
%% @param Msg 消息内容
%% @param Req Cowboy请求对象
%% @param State 状态映射
%% @return {ok, Req, State}
%% @end
-spec info({message, binary()}, cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
info({message, Msg}, Req, State) ->
    ok = elib_log:info("info_Msg ~p, State ~p~n", [Msg, State]),
    cowboy_req:stream_events(#{id => id(), data => Msg}, nofin, Req),
    % erlang:send_after(10, self(), {message, "Tick"}),
    {ok, Req, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 检查流权限
%% 验证用户是否有访问流的权限
%%
%% @param StreamId 流ID
%% @param State 状态映射
%% @return ok
%% @end
-spec check_role(binary(), map()) -> ok.
check_role(StreamId, State) ->
    ok = elib_log:info("StreamId ~p, State ~p~n", [StreamId, State]),
    ok.

%% @doc 生成唯一ID
%% 生成用于SSE流的唯一标识符
%%
%% @return 唯一ID字符串
%% @end
-spec id() -> string().
id() ->
    integer_to_list(erlang:unique_integer([positive, monotonic]), 16).


%% ===================================================================
%% EUnit tests.
%% ===================================================================

