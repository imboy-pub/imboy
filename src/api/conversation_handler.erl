-module(conversation_handler).

-behavior(cowboy_rest).

-export([init/2]).

-include("log.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化会话处理器
%% 根据请求中的 action 参数调用相应的处理函数
%%
%% @param Req0 Cowboy请求对象
%% @param State0 状态映射，包含 action 和 current_uid 等信息
%% @return {ok, Req1, State} 处理后的请求对象和状态
%% @end
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            online ->
                online(Req0, State);
            mine ->
                mine(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 获取在线用户信息
%% 返回当前在线用户数量和列表
%%
%% @param Req0 Cowboy请求对象，包含查询参数
%% @param _State 状态映射
%% @return 返回包含在线用户信息的响应
%% @end
-spec online(cowboy_req:req(), map()) -> cowboy_req:req().
online(Req0, _State) ->
    {ok, Vsn} = application:get_key(imboy, vsn),
    CountUser = imboy_syn:count_user(),
    Count = imboy_syn:count(),
    Msg = io_lib:format("vsn ~s, node ~p, 在线总人数: ~p, 在线设备数~p",
                        [Vsn, node(), CountUser, Count]),
    Res = cowboy_req:match_qs([{type, [], undefined}], Req0),
    % ?DEBUG_LOG(Res),
    List2 = case maps:get(type, Res) of
        <<"list">> ->
            #{limit := Limit} = cowboy_req:match_qs([{limit, [], "10"}], Req0),
            % ?DEBUG_LOG([limit, Limit]),
            {Limit2, _} = string:to_integer(Limit),
            % imboy_syn:list_by_limit(Limit);
            List1 = imboy_syn:list_by_limit(Limit2),
            Column = [<<"uid">>, <<"pid">>, <<"dtype">>, <<"did">>, <<"time">>, <<"ref">>, <<"node">>],
            [lists:zipwith(fun(X, Y) -> {X, Y} end,
                           Column,
                    [Uid, Pid, DType, DID, elib_dt:to_rfc3339(Nano), Ref, Node])
              || {{Uid, Pid}, {DType, DID}, Nano, Ref, Node} <- List1 ];
        _ ->
            []
    end,
    elib_response:success(Req0, List2, Msg).

%% @doc 获取我的会话列表
%% 获取当前用户的会话消息列表
%%
%% @param Req0 Cowboy请求对象，包含 last_server_ts 参数
%% @param State 状态映射，包含 current_uid
%% @return 返回包含会话列表的响应
%% @end
-spec mine(cowboy_req:req(), map()) -> cowboy_req:req().
mine(Req0, State) ->
    #{last_server_ts := ServerTS} =
        cowboy_req:match_qs([{last_server_ts, [], undefined}], Req0),
    % ?DEBUG_LOG(ServerTS),
    CurrentUid = auth_ds:current_uid(State),
    List = msg_c2c_ds:read_msg(CurrentUid, 1000, ServerTS),
    % ?DEBUG_LOG(["mine_list", List]),
    List2 = mine_transfer(List),
    elib_response:success(Req0, List2).

%% @doc 转换会话消息列表
%% 将会话消息数据进行格式转换
%%
%% @param List 原始消息列表
%% @return 转换后的消息列表
%% @end
-spec mine_transfer(list(map())) -> list(map()).
mine_transfer(List) ->
    [begin
         DbId = maps:get(<<"id">>, Msg),
         Payload0 = maps:get(<<"payload">>, Msg, #{}),
         Payload =
             case Payload0 of
                 Bin when is_binary(Bin) ->
                     jsone:decode(Bin, [{object_format, map}]);
                 M when is_map(M) ->
                     M;
                 _ ->
                     #{}
             end,
         Payload#{<<"id">> => DbId}
     end
     || Msg <- List].
