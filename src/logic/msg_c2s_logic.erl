-module(msg_c2s_logic).

%%%
%  C2S 消息业务逻辑模块
%%%

-include("log.hrl").

-export([c2s/3]).
-export([c2s_client_ack/3]).
-export([c2s_to_external/5]).
-export([c2s_to_role_chat/3]).
-export([handle_sync/3]).

%% ===================================================================
%% API
%% ===================================================================


%% @doc C2S 消息入口
-spec c2s(binary(), integer(), map()) -> ok | {reply, map()}.
c2s(MsgId, CurrentUid, Data) ->
    To = maps:get(<<"to">>, Data),
    case cowboy_bstr:to_lower(To) of
        <<"sync">> ->
            %% 客户端增量同步：基于 conv_seq 游标拉取缺失消息
            Payload = maps:get(<<"payload">>, Data, #{}),
            Cursors = maps:get(<<"cursors">>, Payload, []),
            Limit = maps:get(<<"limit">>, Payload, 50),
            Result = handle_sync(CurrentUid, Cursors, Limit),
            {reply, #{<<"id">> => MsgId,
                      <<"type">> => <<"C2S">>,
                      <<"action">> => <<"sync_resp">>,
                      <<"payload">> => Result}};
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
            % 检查是否为 E2EE 社交恢复消息
            Payload = maps:get(<<"payload">>, Data, #{}),
            MsgType = maps:get(<<"msg_type">>, Payload, <<>>),
            case MsgType of
                <<"e2ee_social_shard">> ->
                    handle_e2ee_social_shard(MsgId, CurrentUid, Data);
                _ ->
                    % 不支持的 c2s 消息
                    Msg = message_ds:assemble_s2c(MsgId, <<"c2s_unsupported">>, To),
                    {reply, Msg}
            end
    end.


%% @doc 客户端确认 C2S 投递消息
-spec c2s_client_ack(binary(), integer(), binary()) -> ok.
c2s_client_ack(MsgId, CurrentUid, DID) ->
    msg_ack_logic:client_ack(<<"c2s">>, MsgId, CurrentUid, DID).


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
    From = CurrentUid,
    Payload = maps:get(<<"payload">>, Data),
    Text = maps:get(<<"text">>, Payload),
    TopicId = maps:get(<<"topic_id">>, Payload, 0),
    TopicTitle = maps:get(<<"topic_title">>, Payload, <<>>),
    CreatedAtRaw = maps:get(<<"created_at">>, Data),
    CreatedAt = elib_dt:to_rfc3339(CreatedAtRaw),

    % 异步存储主题（带重试）
    elib_async:async_retry(fun() ->
        msg_c2s_ds:write_topic(<<"C2S">>, TopicId, CurrentUid, To, TopicTitle, CreatedAt)
    end),

    % 【优化】先准备基础 Payload（不含 API 响应）
    % 消息状态：10 服务端收到 11 投递给三方 12 收到三方结果 20 已投递客户端
    Payload0 = Payload#{<<"status">> => 10, <<"to_id_str">> => To},
    Payload0Bin = jsone:encode(Payload0, [native_utf8]),

    % 【关键修复】先备份到 staging 表（同步，检查返回值）
    % v2.0: C2S 消息使用 text 类型，无 action 和 e2ee
    case msg_store_ds:stage(<<"c2s">>, MsgId, <<"text">>, <<>>, #{}, Payload0Bin,
                              CurrentUid, 0, CreatedAt, CreatedAt) of
        ok ->
            % 备份成功，立即响应
            self() ! {reply, #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2S_SERVER_ACK">>,
                <<"server_ts">> => elib_dt:millisecond()
            }},

            % ① 先入队（异步，非阻塞）
            msg_store_ds:enqueue(<<"c2s">>, MsgId, #{
                payload => Payload0Bin,
                from_id => CurrentUid,
                to_id => 0,
                status => 10,
                topic_id => TopicId,
                to_id_str => To,
                created_at => CreatedAt
            }),

            % ② 异步调用外部 API + 投递响应（带重试）
            elib_async:async_retry(fun() ->
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


%% @doc AI 角色聊天处理
%% 从 config_ds:env(ai_roles) 读取角色 system_prompt，通过千帆 API 回复
%% ai_roles 配置示例：#{<<"doctor">> => <<"你是一名专业的医生助手..."/utf8>>}
-spec c2s_to_role_chat(binary(), integer(), map()) -> ok | {reply, map()}.
c2s_to_role_chat(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data),
    RoleId = maps:get(<<"role_id">>, Payload, <<"doctor">>),
    % 从配置读取角色 system_prompt；未配置时使用通用助手提示
    Roles = config_ds:env(ai_roles, #{}),
    SystemPrompt = maps:get(RoleId, Roles,
                            <<"你是一个有帮助的AI助手，请专业、友善地回答用户问题。"/utf8>>),
    % 将 system_prompt 注入为开场历史，使千帆 API 持有角色上下文
    RoleCallback = fun(Uid, Content, _Opts) ->
        History = [
            #{<<"role">> => <<"user">>,      <<"content">> => SystemPrompt},
            #{<<"role">> => <<"assistant">>, <<"content">> => <<"好的，我会按照这个角色来回答您的问题。"/utf8>>}
        ],
        qianfan_api:create_chat(Uid, Content, History)
    end,
    c2s_to_external(MsgId, CurrentUid, <<"bot_role_chat">>, Data, RoleCallback).


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

    % 构建响应消息（v2.0 格式）
    MsgId2 = <<"bot_response", MsgId/binary>>,
    %% v2.0: msg_type 在顶层，不在 payload 中
    Msg = #{
        <<"id">> => MsgId2,
        <<"type">> => <<"C2S">>,
        <<"msg_type">> => <<"text">>,
        <<"topic_id">> => TopicId,
        <<"from">> => To,
        <<"to">> => From,
        <<"payload">> => #{
            <<"text">> => elib_str:replace_single_quote(maps:get(<<"result">>, RespMap, <<>>))
        },
        <<"created_at">> => CreatedAt
    },
    MsgJson = jsone:encode(Msg, [native_utf8]),
    MsLi = elib_retry_config:intervals(<<"c2s">>),
    message_ds:send_next(CurrentUid, MsgId2, MsgJson, MsLi).


%% ===================================================================
%% Sync — 客户端增量同步（基于 conv_seq 游标）
%% ===================================================================


%% @doc 处理客户端 sync 请求
%% 客户端传入多个会话的游标 [{conv_key, seq}...]，服务端返回每个会话的增量消息
%% @param CurrentUid 当前用户 ID
%% @param Cursors 游标列表，每项为 #{<<"conv_key">> => Key, <<"seq">> => Seq}
%% @param Limit 每个会话最多返回条数
-spec handle_sync(integer(), list(map()), non_neg_integer()) -> map().
handle_sync(CurrentUid, Cursors, Limit) when is_list(Cursors) ->
    ClampedLimit = erlang:min(erlang:max(Limit, 1), 100),
    Results = lists:filtermap(
        fun(Cursor) ->
            ConvKey = maps:get(<<"conv_key">>, Cursor, <<>>),
            Seq = maps:get(<<"seq">>, Cursor, 0),
            case authorize_conv(CurrentUid, ConvKey) of
                false ->
                    false;
                true ->
                    case msg_archive_ds:history(ConvKey, Seq, ClampedLimit) of
                        {ok, []} ->
                            false;
                        {ok, Rows} ->
                            Messages = [messaging_logic:encode_history_msg(CurrentUid, R)
                                        || R <- Rows],
                            NextSeq = messaging_logic:next_seq_from_rows(Rows, Seq),
                            {true, #{<<"conv_key">> => ConvKey,
                                     <<"messages">> => Messages,
                                     <<"next_seq">> => NextSeq,
                                     <<"has_more">> => length(Rows) >= ClampedLimit}};
                        {error, _} ->
                            false
                    end
            end
        end, Cursors),
    #{<<"results">> => Results};
handle_sync(_CurrentUid, _Cursors, _Limit) ->
    #{<<"results">> => []}.


%% @doc 验证用户是否有权访问该会话
%% conv_key 格式: "c2c:{min_uid}:{max_uid}" 或 "c2g:{group_id}"
-spec authorize_conv(integer(), binary()) -> boolean().
authorize_conv(CurrentUid, <<"c2c:", Rest/binary>>) ->
    case binary:split(Rest, <<":">>) of
        [UidA, UidB] ->
            A = ec_cnv:to_integer(UidA),
            B = ec_cnv:to_integer(UidB),
            CurrentUid =:= A orelse CurrentUid =:= B;
        _ ->
            false
    end;
authorize_conv(CurrentUid, <<"c2g:", GidBin/binary>>) ->
    Gid = ec_cnv:to_integer(GidBin),
    group_ds:is_member(CurrentUid, Gid);
authorize_conv(_CurrentUid, _ConvKey) ->
    false.


%% ===================================================================
%% E2EE 社交恢复 - 零信任架构
%% ===================================================================

%% @doc 处理 E2EE 社交恢复分片消息
%% 零信任架构：处理代理解密分片请求
-spec handle_e2ee_social_shard(binary(), integer(), map()) -> {reply, map()}.
handle_e2ee_social_shard(MsgId, CurrentUid, Data) ->
    Payload = maps:get(<<"payload">>, Data, #{}),
    Action = maps:get(<<"action">>, Payload, <<>>),

    case Action of
        <<"decrypt_shard">> ->
            % 用户向代理请求解密分片
            % 服务端仅作为传输通道，转发请求给代理
            To = maps:get(<<"to">>, Data),
            From = CurrentUid,

            ShardId = maps:get(<<"shard_id">>, Payload, <<>>),
            KeyVersion = maps:get(<<"key_version">>, Payload, <<>>),
            Uid = maps:get(<<"uid">>, Payload, CurrentUid),
            ProxyUid = ec_cnv:to_integer(To),

            % 记录分片解密请求日志（持久化到数据库）
            e2ee_shard_validator:log_shard_transmission(
                shard_decrypted,
                ShardId,
                #{
                    <<"uid">> => Uid,
                    <<"proxy_uid">> => ProxyUid,
                    <<"key_version">> => KeyVersion
                }
            ),

            % 构造转发消息
            Msg = message_ds:assemble_msg(<<"C2C">>, From, To, Payload, MsgId, <<>>, <<"decrypt_shard">>, null),

            % 转发给代理
            MsLi = elib_retry_config:intervals(<<"c2c">>),
            message_ds:send_next(ProxyUid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),

            % 给请求者回复确认
            {reply, Msg};
        _ ->
            % 不支持的 E2EE 社交恢复操作
            Msg = message_ds:assemble_s2c(MsgId, <<"e2ee_social_unsupported_action">>, Action),
            {reply, Msg}
    end.
