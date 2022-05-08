-module(handler_websocket).
-behavior(cowboy_websocket).
%%%
%% websocket API 优先获取 header里面的token
%%%
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("common.hrl").


%%websocket 握手
init(Req0, State0) ->
    Env = os:getenv("IMBOYENV"),
    DType = cowboy_req:header(<<"cos">>, Req0, <<"">>),
    DID = cowboy_req:header(<<"did">>, Req0, <<"">>),
    HeaderAuth = cowboy_req:header(<<"authorization">>, Req0),
    Subprotocols = cowboy_req:header(<<"sec-websocket-protocol">>,
                                     Req0),
    QsAuth = cowboy_req:match_qs([{'authorization', [], undefined}],
                                 Req0),
    Opt0 = #{num_acceptors => infinity,
             max_connections => infinity,
             max_frame_size => 1048576,  % 1MB
             idle_timeout => 120000  %  % Cowboy关闭连接空闲120秒 默认值为 60000
        },

    State1 = [{'dtype', DType} | State0],
    State2 = [{'did', DID} | State1],

    if
        Env == "local",HeaderAuth =/= undefined ->
            ds_websocket:auth(HeaderAuth, Req0, State2, Opt0);
        Env == "local",QsAuth =/= undefined ->
            Token = maps:get(authorization, QsAuth),
            ds_websocket:auth(Token, Req0, State2, Opt0);
        % 为了安全考虑，非 local 环境必须要 DID 和 HeaderAuth，必须check subprotocols
        bit_size(DID) > 0,
        HeaderAuth =/= undefined ->
            case ds_websocket:check_subprotocols(Subprotocols,
                                                 Req0,
                                                 State2) of
                {ok, Req1, State3} ->
                    {ok, Req1, State3};
                {cowboy_websocket, Req1, State3, Opt} ->
                    ds_websocket:auth(HeaderAuth,
                                      Req1,
                                      State3,
                                      Opt)
            end;
        true ->
            ?LOG([Req0, State0]),
            % token无效 (包含缺失token情况) 或者设备ID不存在
            {cowboy_websocket, Req0, [{error, 706} | State0]}
    end.


%%连接初始 onopen
websocket_init(State) ->
    CurrentPid = self(),
    ?LOG([websocket_init, lists:keyfind(error, 1, State), State]),
    case lists:keyfind(error, 1, State) of
        {error, Code} ->
            Msg = [{<<"type">>, <<"error">>},
                   {<<"code">>, Code},
                   {<<"timestamp">>, util_dt:milliseconds()}],
            {reply, {text, jsone:encode(Msg)}, State, hibernate};
        false ->
            CurrentUid = proplists:get_value(current_uid, State),
            % 用户上线
            DType = proplists:get_value('dtype', State, <<"">>),
            DID = proplists:get_value('did', State, <<"">>),
            logic_user:online(CurrentUid, CurrentPid, DType, DID),
            {ok, proplists:delete('dtype', State), hibernate}
    end.


%%处理客户端发送投递的消息 onmessage
websocket_handle(ping, State) ->
    % ?LOG([ping, cowboy_clock:rfc1123(), State]),
    case lists:keyfind(error, 1, State) of
        {error, _Code} ->
            {stop, State};
        false ->
            {reply, pong, State, hibernate}
    end;
websocket_handle({text, <<"ping">>}, State) ->
    % ?LOG([<<"ping">>, cowboy_clock:rfc1123(), State]),
    case lists:keyfind(error, 1, State) of
        {error, _Code} ->
            {stop, State};
        false ->
            {reply, {text, <<"pong2">>}, State, hibernate}
    end;
websocket_handle({text, <<"logout">>}, State) ->
    ?LOG([<<"logout">>, cowboy_clock:rfc1123(), State]),
    {stop, State};
% 客户端确认消息
websocket_handle(
    {text, <<"C_ACK", MsgId:20/binary, ",DID", DID/binary>>},
    State
) ->
    ?LOG(["C_ACK", MsgId, DID, State]),
    CurrentUid = proplists:get_value(current_uid, State),
    logic_websocket:c2c_client_ack(MsgId, CurrentUid, DID),
    {ok, State, hibernate};
websocket_handle({text, Msg}, State) ->
    ?LOG([State, Msg]),
    % ?LOG(State),
    try
        CurrentUid = proplists:get_value(current_uid, State),
        Data = jsone:decode(Msg, [{object_format, proplist}]),
        Id = proplists:get_value(<<"id">>, Data),
        Type = proplists:get_value(<<"type">>, Data),
        ?LOG([Id, Type, Data]),
        % 逻辑层负责IM系统各项功能的核心逻辑实现
        % Type 包括单聊（c2c）、上报(c2s)、推送(s2c)、群聊(c2g)
        Result0 =
            case cowboy_bstr:to_upper(Type) of
                <<"C2C">> ->  % 单聊消息
                    logic_websocket:c2c(Id, CurrentUid, Data);
                <<"C2C_REVOKE">> ->  % 客户端撤回消息
                    logic_websocket:c2c_revoke(Id, Data, Type);
                <<"C2C_REVOKE_ACK">> ->  % 客户端撤回消息ACK
                    logic_websocket:c2c_revoke(Id, Data, Type);
                <<"C2G">> ->  % 群聊消息
                    logic_websocket:c2g(Id, CurrentUid, Data)
            end,
        case Result0 of
            ok ->
                ok;
            {reply, Msg2} ->
                {reply, Msg2}
        end
    of
        Result ->
            % ?LOG(Result),
            case Result of
                ok ->
                    {ok, State, hibernate};
                {reply, Msg4} ->
                    {reply, {text, jsone:encode(Msg4, [native_utf8])},
                            State,
                            hibernate}
            end
    catch
        Class:Reason ->
            ?LOG(["websocket_handle try catch: Class:",
                  Class,
                  "Reason:",
                  Reason,
                  "trace:",
                  erlang:trace(all, true, [call])]),
            {ok, State, hibernate}
    end;
websocket_handle({binary, Msg}, State) ->
    {[{binary, Msg}], State};
websocket_handle(_Frame, State) ->
    {ok, State, hibernate}.


%% 处理erlang 发送的消息
websocket_info({timeout, _Ref, Msg}, State) ->
    ?LOG([timeout, cowboy_clock:rfc1123(), _Ref, Msg, State]),
    {reply, {text, Msg}, State, hibernate};
websocket_info(stop, State) ->
    ?LOG([stop, State]),
    {stop, State};
websocket_info(_Info, State) ->
    {ok, State}.


%% 断开socket onclose
%% Rename websocket_terminate/3 to terminate/3
%% link: https://github.com/ninenines/cowboy/issues/787
terminate(Reason, _Req, State) ->
    ?LOG([terminate, cowboy_clock:rfc1123(), State, Reason]),

    DID = proplists:get_value('did', State, <<"">>),
    case lists:keyfind(current_uid, 1, State) of
        {current_uid, Uid} ->
            logic_user:offline(Uid, self(), DID);
        false ->
            repo_chat_store:dirty_delete(self())
    end,
    ok.
