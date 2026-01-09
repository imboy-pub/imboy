-module(msg_c2s_logic).

%%%
%  C2S 消息业务逻辑模块
%%%

-include("log.hrl").

-export([c2s/3]).
-export([c2s_client_ack/3]).
-export([c2s_to_external/5]).
-export([c2s_to_role_chat/3]).

%% ===================================================================
%% API
%% ===================================================================


%% @doc C2S 消息入口
-spec c2s(binary(), integer(), map()) -> ok | {reply, map()}.
c2s(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    case cowboy_bstr:to_lower(To) of
        <<"bot_qian_fan">> ->
            c2s_to_external(MsgId, CurrentUid, <<"bot_qian_fan">>, Data,
                           fun qianfan_api:create_chat/3);
        % 【扩展点】未来添加其他外部服务
        % <<"bot_openai">> ->
        %     c2s_to_external(MsgId, CurrentUid, <<"bot_openai">>, Data,
        %                    fun openai_api:create_chat/3);
        % <<"bot_claude">> ->
        %     c2s_to_external(MsgId, CurrentUid, <<"bot_claude">>, Data,
        %                    fun claude_api:create_chat/3);
        _ ->
            % 不支持的 c2s 消息
            Msg = message_ds:assemble_s2c(MsgId, <<"c2s_unsupported">>, To),
            {reply, Msg}
    end.


%% @doc 客户端确认 C2S 投递消息
-spec c2s_client_ack(binary(), integer(), binary()) -> ok.
c2s_client_ack(MsgId, CurrentUid, _DID) ->
    % C2S 消息的 ACK 确认：直接删除消息（使用完整的 WHERE 语句）
    MsgIdBin = MsgId,
    UidBin = integer_to_binary(CurrentUid),
    Where = <<"msg_id = '", MsgIdBin/binary, "' AND from_id = ", UidBin/binary>>,
    _ = msg_c2s_repo:delete_msg(Where),

    % 【关键修复】标记 staging 表为已处理，清理备份记录
    msg_store_ds:unstage(MsgId),

    ok.


%% ===================================================================
%% 外部服务处理函数
%% ===================================================================


%% @doc C2S 消息发送到外部服务（AI/Bot/第三方 API）
%% @param MsgId 消息ID
%% @param CurrentUid 当前用户ID
%% @param To 外部服务标识（如 <<"bot_qian_fan">>）
%% @param Data 消息数据
%% @param ApiCallback API 调用回调函数，签名为 fun(Uid, Text, Opts) -> RespMap
-spec c2s_to_external(binary(), integer(), binary(), map(), function()) ->
    ok | {reply, map()}.
c2s_to_external(MsgId, CurrentUid, To, Data, ApiCallback) ->
    From = imboy_hashids:encode(CurrentUid),
    Payload = maps:get(<<"payload">>, Data),
    Text = maps:get(<<"text">>, Payload),
    TopicId = maps:get(<<"topic_id">>, Payload, 0),
    TopicTitle = maps:get(<<"topic_title">>, Payload, <<>>),
    CreatedAtRaw = maps:get(<<"created_at">>, Data),
    CreatedAt = imboy_dt:to_rfc3339(CreatedAtRaw),

    % 异步存储主题（带重试）
    imboy_async:async_retry(fun() ->
        msg_c2s_ds:write_topic(<<"C2S">>, TopicId, CurrentUid, To, TopicTitle, CreatedAt)
    end),

    % 【优化】先准备基础 Payload（不含 API 响应）
    % 消息状态：10 服务端收到 11 投递给三方 12 收到三方结果 20 已投递客户端
    Payload0 = Payload#{<<"status">> => 10, <<"to_id_str">> => To},
    Payload0Bin = jsone:encode(Payload0, [native_utf8]),

    % 【关键修复】先备份到 staging 表（同步，检查返回值）
    case msg_store_ds:stage(<<"c2s">>, MsgId, Payload0Bin, CurrentUid, 0,
                              CreatedAt, CreatedAt) of
        ok ->
            % 备份成功，立即响应
            self() ! {reply, #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2S_SERVER_ACK">>,
                <<"server_ts">> => imboy_dt:millisecond()
            }},

            % ① 先入队（异步，非阻塞）
            msg_store_ds:enqueue(c2s, MsgId, #{
                payload => Payload0Bin,
                from_id => CurrentUid,
                to_id => 0,
                status => 10,
                topic_id => TopicId,
                to_id_str => To,
                created_at => CreatedAt
            }),

            % ② 异步调用外部 API + 投递响应（带重试）
            imboy_async:async_retry(fun() ->
                % 使用回调调用外部 API
                RespMap = ApiCallback(CurrentUid, Text, []),
                send_service_response(To, MsgId, CurrentUid, From, Payload0,
                                     RespMap, TopicId, CreatedAt)
            end, 3, 2000),
            ok;
        error ->
            % 备份失败，返回错误
            ok = ?ERROR_LOG("[C2S_STAGE_FAILED] MsgId=~s, Uid=~p, To=~p~n",
                      [MsgId, CurrentUid, To]),
            Msg = message_ds:assemble_s2c(MsgId, <<"internal_error">>, To),
            {reply, Msg}
    end.


%% @doc AI 角色聊天处理（预留接口，需配合 ai_role_ds 使用）
-spec c2s_to_role_chat(binary(), integer(), map()) -> {reply, map()}.
c2s_to_role_chat(MsgId, _CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    _RoleId = maps:get(<<"role_id">>, Payload, <<"doctor">>),

    % TODO: 实现角色聊天逻辑
    % case ai_role_ds:get_role(_RoleId) of
    %     {ok, Role} ->
    %         SystemPrompt = maps:get(<<"system_prompt">>, Role),
    %         ...
    % end.

    Msg = message_ds:assemble_s2c(MsgId, <<"role_chat_not_implemented">>, <<"bot_role_chat">>),
    {reply, Msg}.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%% @doc 发送外部服务响应消息（内部辅助函数）
-spec send_service_response(binary(), binary(), integer(), binary(), map(),
                             map(), integer(), binary()) -> ok.
send_service_response(To, MsgId, CurrentUid, From, Payload0, RespMap, TopicId, CreatedAt) ->
    % 更新消息状态
    _Payload2 = Payload0#{
        <<"bot_response">> => RespMap,
        <<"status">> => 12  % 状态：12 收到三方结果
    },

    % 构建响应消息
    MsgId2 = <<"bot_response", MsgId/binary>>,
    Msg = #{
        <<"id">> => MsgId2,
        <<"type">> => <<"C2S">>,
        <<"topic_id">> => TopicId,
        <<"from">> => To,
        <<"to">> => From,
        <<"payload">> => #{
            <<"msg_type">> => <<"text">>,
            <<"text">> => imboy_str:replace_single_quote(maps:get(<<"result">>, RespMap))
        },
        <<"created_at">> => CreatedAt
    },
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = [0, 5000, 7000, 11000],
    message_ds:send_next(CurrentUid, MsgId2, MsgJson, MsLi).
