-module (websocket_ds).
%%%
% websocket_ds 是 websocket domain service 缩写
%%%
-export ([check_subprotocols/2]).
-export ([auth/4]).

-include("common.hrl").

check_subprotocols(Req0, State0) ->
    ImOpts = #{
        num_acceptors => infinity,
        max_connections => infinity,
        max_frame_size => 1048576, % 1MB
        idle_timeout => 120000 %  % Cowboy关闭连接空闲120秒 默认值为 60000
    },
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        undefined ->
            % HTTP 400 - 请求无效
            Req = cowboy_req:reply(400, Req0),
            {ok, Req, State0};
        Subprotocols ->
            % ?LOG([self(), State0, Subprotocols]),
            IsText = lists:member(<<"text">>, Subprotocols),
            if
                IsText == true ->
                    Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                        <<"text">>, Req0),
                    {cowboy_websocket, Req, State0, ImOpts};
                true ->
                    % HTTP 406 - 无法接受
                    Req1 = cowboy_req:reply(406, Req0),
                    {ok, Req1, State0}
            end
    end.

-spec auth(DID::binary(), Req1::any(), State1::list(), Opt::any()) -> any().
auth(DID, Req1, State1, Opt) ->
    SystemState = [{'did', DID}|State1],
    case cowboy_req:header(<<"authorization">>, Req1, undefined) of
        undefined ->
            % HTTP 412 - 先决条件失败
            % Req2 = cowboy_req:reply(412, Req1),
            % {ok, Req2, State1};
            case cowboy_req:match_qs([{'authorization', [], undefined}], Req1) of
                #{'authorization' := undefined} ->
                    % HTTP 412 - 先决条件失败
                    Req2 = cowboy_req:reply(412, Req1),
                    {ok, Req2, State1};
                #{'authorization' := Token} ->
                    case catch token_ds:decrypt_token(Token) of
                        {ok, Uid, _ExpireAt, _Type} ->
                            Timeout = user_logic:idle_timeout(Uid),
                            {cowboy_websocket, Req1, [{current_uid, Uid}|SystemState], Opt#{idle_timeout := Timeout}};
                        {error, 705, Msg, _Li} ->
                            Req3 = resp_json_dto:error(Req1, Msg),
                            {ok, Req3, State1};
                        {error, Code, _Msg, _Li} ->
                            {cowboy_websocket, Req1, [{error, Code} | State1], Opt}
                    end
            end;
        Authorization ->
            case catch token_ds:decrypt_token(Authorization) of
                {ok, Uid, _ExpireAt, _Type} ->
                    Timeout = user_logic:idle_timeout(Uid),
                    {cowboy_websocket, Req1, [{current_uid, Uid}|SystemState], Opt#{idle_timeout := Timeout}};
                {error, 705, Msg, _Li} ->
                    Req3 = resp_json_dto:error(Req1, Msg),
                    {ok, Req3, State1};
                {error, Code, _Msg, _Li} ->
                    {cowboy_websocket, Req1, [{error, Code} | State1], Opt}
            end
    end.
