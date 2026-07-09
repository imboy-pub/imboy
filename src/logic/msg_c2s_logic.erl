-module(msg_c2s_logic).

%%%
%  C2S 消息业务逻辑模块
%%%

-include("log.hrl").

-export([c2s/3]).
-export([c2s_client_ack/3]).
-export([c2s_to_external/5]).
-export([llm_callback/2]).
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
            {reply, #{
                <<"id">> => MsgId,
                <<"type">> => <<"C2S">>,
                <<"action">> => <<"sync_resp">>,
                <<"in_reply_to">> => MsgId,
                <<"payload">> => Result
            }};
        <<"bot_", _/binary>> = Bot ->
            % bot_* 统一查 imboy_llm_registry 分派（BYO-LLM）
            % 新增 provider 只需实现 imboy_llm behaviour + llm_providers 配置
            c2s_to_llm(MsgId, CurrentUid, Bot, Data);
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

%% @doc bot_* 消息按注册表分派到 LLM provider
%% To → provider 名：bot_qian_fan → qianfan（向后兼容），bot_xxx → xxx
-spec c2s_to_llm(binary(), integer(), binary(), map()) -> ok | {reply, map()}.
c2s_to_llm(MsgId, CurrentUid, To, Data) ->
    case imboy_llm_registry:lookup(provider_name(To)) of
        {ok, #{module := Mod, opts := Opts}} ->
            c2s_to_external(MsgId, CurrentUid, To, Data, llm_callback(Mod, Opts));
        undefined ->
            {reply, message_ds:assemble_s2c(MsgId, <<"c2s_unsupported">>, To)}
    end.

%% @doc 把 imboy_llm:chat/3 桥接为 c2s_to_external/5 的 ApiCallback 契约
%% ApiCallback 契约：fun(Uid, Text, Opts) -> RespMap（裸 map，含 result 键）
%% 桥接：Text 包成单条 user 消息；{ok, RespMap} → RespMap；
%% {error, _} → 抛异常触发 c2s_to_external 的 async_retry 重试（与原 qianfan
%% crash-then-retry 一致），避免吞成空 result 给用户发空气泡。
-spec llm_callback(module(), map()) -> fun((integer(), binary(), list()) -> map()).
llm_callback(Mod, Opts) ->
    fun(Uid, Text, _CallbackOpts) ->
        Messages = [#{<<"role">> => <<"user">>, <<"content">> => Text}],
        case Mod:chat(Uid, Messages, Opts) of
            {ok, RespMap} ->
                RespMap;
            {error, Reason} ->
                ok = ?ERROR_LOG(
                    "[C2S_LLM_FAILED] provider=~p reason=~p~n",
                    [Mod, Reason]
                ),
                error({llm_failed, Reason})
        end
    end.

provider_name(<<"bot_qian_fan">>) -> <<"qianfan">>;
provider_name(<<"bot_", Rest/binary>>) -> Rest.

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
    case
        msg_store_ds:stage(
            <<"c2s">>,
            MsgId,
            <<"text">>,
            <<>>,
            #{},
            Payload0Bin,
            CurrentUid,
            0,
            CreatedAt,
            CreatedAt
        )
    of
        {ok, duplicate} ->
            % 客户端重发：只补发 ACK，跳过外部 API 重复调用
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2S_SERVER_ACK">>,
                    <<"in_reply_to">> => MsgId,
                    <<"server_ts">> => elib_dt:millisecond()
                }},
            ok;
        {ok, new} ->
            % 备份成功，立即响应
            self() !
                {reply, #{
                    <<"id">> => MsgId,
                    <<"type">> => <<"C2S_SERVER_ACK">>,
                    <<"in_reply_to">> => MsgId,
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
            elib_async:async_retry(
                fun() ->
                    % 使用回调调用外部 API
                    RespMap = ApiCallback(CurrentUid, Text, []),
                    send_service_response(
                        To,
                        MsgId,
                        CurrentUid,
                        From,
                        Payload0,
                        RespMap,
                        TopicId,
                        CreatedAt
                    )
                end,
                3,
                2000
            ),
            ok;
        error ->
            % 备份失败，返回错误
            ok = ?ERROR_LOG(
                "[C2S_STAGE_FAILED] MsgId=~s, Uid=~p, To=~p~n",
                [MsgId, CurrentUid, To]
            ),
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
    SystemPrompt = maps:get(
        RoleId,
        Roles,
        <<"你是一个有帮助的AI助手，请专业、友善地回答用户问题。"/utf8>>
    ),
    % 将 system_prompt 注入为开场历史，使千帆 API 持有角色上下文
    RoleCallback = fun(Uid, Content, _Opts) ->
        History = [
            #{<<"role">> => <<"user">>, <<"content">> => SystemPrompt},
            #{<<"role">> => <<"assistant">>, <<"content">> => <<"好的，我会按照这个角色来回答您的问题。"/utf8>>}
        ],
        qianfan_api:create_chat(Uid, Content, History)
    end,
    c2s_to_external(MsgId, CurrentUid, <<"bot_role_chat">>, Data, RoleCallback).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 发送外部服务响应消息（内部辅助函数）
-spec send_service_response(
    binary(),
    binary(),
    integer(),
    integer(),
    map(),
    map(),
    integer(),
    binary()
) -> ok.
send_service_response(To, MsgId, CurrentUid, From, Payload0, RespMap, TopicId, CreatedAt) ->
    % 更新消息状态
    _Payload2 = Payload0#{
        <<"bot_response">> => RespMap,
        % 状态：12 收到三方结果
        <<"status">> => 12
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
    %% 1. 先鉴权过滤，得到授权会话的 {ConvKey, Seq}
    Authed = lists:filtermap(
        fun(Cursor) ->
            ConvKey = maps:get(<<"conv_key">>, Cursor, <<>>),
            Seq = maps:get(<<"seq">>, Cursor, 0),
            case authorize_conv(CurrentUid, ConvKey) of
                true -> {true, {ConvKey, Seq}};
                false -> false
            end
        end,
        Cursors
    ),
    %% 2. 一次 LATERAL 批量查所有授权会话增量，再按 conv_key 分组(消除逐会话 N+1)
    Grouped =
        case msg_archive_ds:history_batch(Authed, ClampedLimit) of
            {ok, AllRows} ->
                lists:foldl(
                    fun(Row, Acc) ->
                        CK = maps:get(<<"conv_key">>, Row),
                        Acc#{CK => [Row | maps:get(CK, Acc, [])]}
                    end,
                    #{},
                    AllRows
                );
            {error, _} ->
                #{}
        end,
    %% 3. 按原授权顺序组装每个会话的结果(空会话过滤，语义同原逐条 {ok,[]}->false)
    Results = lists:filtermap(
        fun({ConvKey, Seq}) ->
            case maps:get(ConvKey, Grouped, []) of
                [] ->
                    false;
                RevRows ->
                    %% 分组时前插致倒序，还原为 conv_seq ASC
                    Rows = lists:reverse(RevRows),
                    Messages = [
                        messaging_logic:encode_history_msg(CurrentUid, R)
                     || R <- Rows
                    ],
                    NextSeq = messaging_logic:next_seq_from_rows(Rows, Seq),
                    {true, #{
                        <<"conv_key">> => ConvKey,
                        <<"messages">> => Messages,
                        <<"next_seq">> => NextSeq,
                        <<"has_more">> => length(Rows) >= ClampedLimit
                    }}
            end
        end,
        Authed
    ),
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
            ProxyUid = ec_cnv:to_integer(To),

            % 归属校验：shard_id 必须属于请求者本人，且 To 必须是该分片的代理，
            % 防止客户端可控 shard_id/to 伪造解密请求骚扰任意用户
            case verify_shard_relay(CurrentUid, ShardId, ProxyUid) of
                ok ->
                    % 记录分片解密请求日志（uid 取鉴权身份）
                    e2ee_shard_validator:log_shard_transmission(
                        shard_decrypted,
                        ShardId,
                        #{
                            <<"uid">> => CurrentUid,
                            <<"proxy_uid">> => ProxyUid,
                            <<"key_version">> => KeyVersion
                        }
                    ),

                    % 构造转发消息
                    Msg = message_ds:assemble_msg(
                        <<"C2C">>, From, To, Payload, MsgId, <<>>, <<"decrypt_shard">>, null
                    ),

                    % 转发给代理
                    MsLi = elib_retry_config:intervals(<<"c2c">>),
                    message_ds:send_next(ProxyUid, MsgId, jsone:encode(Msg, [native_utf8]), MsLi),

                    % 给请求者回复确认
                    {reply, Msg};
                {error, Reason} ->
                    _ = ?WARN_LOG({decrypt_shard_relay_rejected, CurrentUid, ShardId, Reason}),
                    {reply, message_ds:assemble_s2c(MsgId, <<"shard_not_owned">>, To)}
            end;
        _ ->
            % 不支持的 E2EE 社交恢复操作
            Msg = message_ds:assemble_s2c(MsgId, <<"e2ee_social_unsupported_action">>, Action),
            {reply, Msg}
    end.

%% @doc 校验分片转发请求：分片必须属于请求者本人，且 To 为该分片登记的代理
-spec verify_shard_relay(integer(), binary(), integer()) -> ok | {error, term()}.
verify_shard_relay(_Uid, <<>>, _ProxyUid) ->
    {error, missing_shard_id};
verify_shard_relay(Uid, ShardId, ProxyUid) ->
    case e2ee_social_ds:get_shard_by_id(Uid, ShardId) of
        {ok, Shard} ->
            case maps:get(<<"proxy_uid">>, Shard) =:= ProxyUid of
                true -> ok;
                false -> {error, proxy_mismatch}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
