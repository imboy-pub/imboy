-module(whip_handler).
%%%
% whip 控制器模块
% whip controller module
% 参考 https://blog.csdn.net/sweibd/article/details/124552793
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

-define(crlf, <<"\r\n">>).

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            publish ->
                publish(Req0, State);
            check ->
                Method = cowboy_req:method(Req0),
                check(Method, Req0, State);
            unpublish ->
                unpublish(Req0, State);
            subscribe ->
                subscribe(Req0, State);
            unsubscribe ->
                unsubscribe(Req0, State);
            % candidate ->
            %     candidate(Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


% webrtc推流接口
publish(Req0, State) ->
    % imboy_log:error(io_lib:format("whip_handler/publish state:~p ~n", [State])),
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),
    RoomId = cowboy_req:binding(room_id, Req0),
    StreamId = cowboy_req:binding(stream_id, Req0),
    {ok, Sdp, _Req} = cowboy_req:read_body(Req0),
    % imboy_log:info(io_lib:format("whip_handler/publish State ~p, ~n", [State])),
    % imboy_log:info(io_lib:format("whip_handler/publish Sdp: ~p ~n", [Sdp])),

    {ok, OfferSdp} = sdp_parse(Sdp),

    Origin = ersip_sdp:origin(OfferSdp),
    Username = ersip_sdp_origin:username(Origin),
    imboy_log:info(io_lib:format("whip_handler/publish Username: ~p ~n", [Username])),
    SessionId = ersip_sdp_origin:session_id(Origin),
    imboy_log:info(io_lib:format("whip_handler/publish SessionId: ~p ~n", [SessionId])),
    AsswerSdp = generate_answer_sdp(OfferSdp, "a=setup:passive", State),
    NewReq = cowboy_req:reply(201,
                              #{
                                <<"Content-Type">> => "application/sdp",
                                <<"server">> => "cowboy",
                                <<"location">> => imboy_func:implode("/", ["/whip", StreamId, RoomId])
                               },
                              AsswerSdp
                              % , generate_answer_sdp2()
                              ,
                              Req0),
    {ok, NewReq, State}.


% check(<<"PATCH">>, Req0, State) ->
check(<<"PATCH_NO_SUPERT">>, Req0, State) ->
    {ok, Ice, _Req} = cowboy_req:read_body(Req0),
    imboy_log:error(io_lib:format("whip_handler/check PATCH Ice:~p ~n", [Ice])),
    % <<"candidate:2921655801 1 tcp 1518214911 169.254.244.209 64754 typ host tcptype passive generation 0 ufrag ZZt4 network-id 4 network-cost 50">>
    % <<"candidate:2435201596 1 tcp 1518157055 ::1 64756 typ host tcptype passive generation 0 ufrag ZZt4 network-id 3">>
    % <<"candidate:3388485749 1 tcp 1518083839 127.0.0.1 64755 typ host tcptype passive generation 0 ufrag ZZt4 network-id 2">>
    % 不执行 ICE 重启
    NewReq = cowboy_req:reply(200,
                              #{
                                <<"Content-Type">> => "application/trickle-ice-sdpfrag", <<"server">> => "cowboy"
                                % , <<"location">> => imboy_func:implode("/", ["/whip", StreamId, Id])
                               },
                              <<"candidate: candidate:2152662189 1 udp 2122194687 192.168.0.144 64291 typ host generation 0 ufrag CNcn network-id 0 network-cost 50, sdpMid: 0, sdpMLineIndex: 0">>,
                              Req0),
    {ok, NewReq, State};

% 执行ice重启
%  NewReq = cowboy_req:reply(200
%      , #{
%          <<"Content-Type">> => "application/trickle-ice-sdpfrag"
%          , <<"If-Match">> => "*"
%          , <<"server">> => "cowboy"
%          % , <<"location">> => imboy_func:implode("/", ["/whip", StreamId, Id])
%      }
%      % , ersip_sdp_ice_candidate
%      , Ice
%      , Req0),
% {ok, NewReq, State};

check(<<"DELETE">>, Req0, State) ->
    % To explicitly terminate a session, the WHIP client MUST perform an HTTP DELETE request to the resource URL returned in the Location header field of the initial HTTP POST. Upon receiving the HTTP DELETE request, the WHIP resource will be removed and the resources freed on the Media Server, terminating the ICE and DTLS sessions.
    Req1 = cowboy_req:reply(200, Req0),
    % imboy_log:error(io_lib:format("whip_handler/publish Req1:~p ~n", [Req1])),
    {ok, Req1, State};
check(_Method, Req0, State) ->
    Req1 = cowboy_req:reply(405, Req0),
    % imboy_log:error(io_lib:format("whip_handler/publish Req1:~p ~n", [Req1])),
    {ok, Req1, State}.


% webrtc unpublish
% 也可以暴力的关闭可以直接在客户端进行PeerConnection.Close(), 或者暴力关闭网页；
unpublish(Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),,
    imboy_response:success(Req0).


% webrtc拉流接口
subscribe(Req0, State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),
    RoomId = cowboy_req:binding(room_id, Req0),
    StreamId = cowboy_req:binding(stream_id, Req0),
    {ok, Sdp, _Req} = cowboy_req:read_body(Req0),
    {ok, OfferSdp} = sdp_parse(Sdp),
    AsswerSdp = generate_answer_sdp(OfferSdp, "a=setup:active", State),
    NewReq = cowboy_req:reply(201,
                              #{
                                <<"Content-Type">> => "application/sdp",
                                <<"server">> => "cowboy",
                                <<"location">> => imboy_func:implode("/", ["/whip", StreamId, RoomId])
                               },
                              AsswerSdp
                              % , generate_answer_sdp2()
                              ,
                              Req0),
    {ok, NewReq, State}.


% webrtc unsubscribe
% 也可以暴力的关闭可以直接在客户端进行PeerConnection.Close(), 或者暴力关闭网页；
unsubscribe(Req0, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    % Uid = imboy_hashids:uid_encode(CurrentUid),,
    imboy_response:success(Req0).


% candidate(Req0, _State) ->
%     % CurrentUid = maps:get(current_uid, State),
%     % Uid = imboy_hashids:uid_encode(CurrentUid),
%     {ok, Candidate, _Req} = cowboy_req:read_body(Req0),
%     ok.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.


sdp_parse(Sdp) ->
    % imboy_log:error(io_lib:format("whip_handler/publish OfferSdp ~p :~p ~n", [is_binary(OfferSdp), OfferSdp])),
    Sdp1 = check_crlf(Sdp),

    % imboy_log:error(io_lib:format("whip_handler/publish OfferSdp1 ~p :~p ~n", [is_binary(OfferSdp), OfferSdp1])),
    % WebRTC 中 answer SDP 中 m-line 不能随意增加和删除，顺序不能随意变更，需要和 Offer SDP 中保持一致。
    %% 在这里编写你的逻辑来生成 Answer SDP
    %% 可以使用 ersip_sdp 库来解析和构建 SDP 数据
    ersip_sdp:parse(Sdp1).


% generate_answer_sdp2() ->
%     % CurrentUid = maps:get(current_uid, State),
%     %% 例如：
%     Sdp = [
%         "v=0", ?crlf,
%         "o=- 1234567890 1 IN IP4 192.168.0.144", ?crlf,
%         "s=WebRTC Session", ?crlf,
%         "t=0 0", ?crlf,
%         "a=setup:active", ?crlf,
%         "a=connection:new", ?crlf,
%         "m=audio 50000 RTP/AVP 0", ?crlf,
%         "c=IN IP4 192.168.0.144", ?crlf,
%         "a=rtpmap:0 PCMU/8000", ?crlf
%     ],
%     iolist_to_binary(Sdp).


%% 根据 Offer SDP 生成 Answer SDP
% https://aggresss.blog.csdn.net/article/details/126991583
% 为了降低复杂性，不支持 SDP 重新协商，因此在完成通过 HTTP 的初始 SDP Offer/Answer 后，不能添加或删除任何 track 或 stream 。
generate_answer_sdp(OfferSdp, Setup, _State) ->
    % CurrentUid = maps:get(current_uid, State),
    %% 例如：
    OrigOrigin = ersip_sdp:origin(OfferSdp),

    ExpectedOrigin = ersip_sdp_origin:set_address(ersip_sdp_addr:make(<<"IN IP4 dev.imboy.pub">>), OrigOrigin),
    %% 构建 Answer SDP
    AnswerSdp2 = ersip_sdp:set_origin(ExpectedOrigin, OfferSdp),
    % SDP Offer 应该使用 sendonly 属性，SDP Answer 必须使用 recvonly 属性。
    % The SDP offer SHOULD use the "sendonly" attribute and the SDP answer MUST use the "recvonly" attribute in any case.
    %% 添加其他需要的信息

    %% 返回生成的 Answer SDP
    % ersip_sdp:assemble(AnswerSdp2).
    AnswerSdp3 = ersip_sdp:assemble(AnswerSdp2),

    AnswerSdp4 =
        case Setup of
            "a=setup:passive" ->
                iolist_to_binary(string:replace(AnswerSdp3, <<"a=sendonly">>, <<"a=recvonly">>, all));
            "a=setup:active" ->
                iolist_to_binary(string:replace(AnswerSdp3, <<"a=recvonly">>, <<"a=sendonly">>, all))
        end,

    % CurrentUid = 0,
    % {Username, Credential, _Uris} = user_ds:webrtc_credential(CurrentUid),
    SdpLi = remove_item(AnswerSdp4),
    Append = [[[Setup, ?crlf]]
     % , ["a=connection:new", ?crlf]
     % , ["m=audio 50000 RTP/AVP 0", ?crlf]
     % , ["a=mid:audio", ?crlf]
     % , [["a=ice-ufrag:", Username, ?crlf]]
     % , [["a=ice-pwd:", Credential, ?crlf]]
     % , [["a=ice-lite", ?crlf]]
    ],
    AnswerSdp5 = iolist_to_binary(SdpLi ++ Append),
    % imboy_log:error(io_lib:format("whip_handler/publish AnswerSdp5 ~p :~p ~n", [is_binary(AnswerSdp5), AnswerSdp5])),
    AnswerSdp5.


check_crlf(Sdp) ->
    case string:find(Sdp, ?crlf) of
        nomatch ->
            iolist_to_binary(string:replace(Sdp, <<"\n">>, ?crlf, all));
        _ ->
            Sdp
    end.


remove_item(Sdp) ->
    [ [I, ?crlf]
      || I <- binary:split(Sdp, ?crlf, [global]),
         case I of
             % <<"m=", _T1/binary>> ->
             %     false;
             % <<"a=ice", _T1/binary>> ->
             %     false;
             % a=setup:actpass 既可以是客户端，也可以是服务器
             % a=setup:active 客户端
             % a=setup:passive 服务器
             <<"a=setup:", _T1/binary>> ->
                 false;
             <<"a=mid:", _T1/binary>> ->
                 false;
             <<>> ->
                 false;
             _ ->
                 true
         end ].
