-module(moya_wechat_msg_logic).

%%%===================================================================
%%% @doc 墨芽小程序「消息推送」业务逻辑 / WeChat mini-program message push
%%%
%%% 微信在「开发管理 → 消息推送」保存配置时会先发一次 **GET** 校验：
%%%   ?signature=&timestamp=&nonce=&echostr=
%%% 验签通过后**原样返回 echostr** 才算接入成功（否则后台报 Token 验证失败）。
%%% 之后用户与小程序**客服**产生交互（发消息、进入会话）时，微信会 POST 事件
%%% 到同一 URL，5 秒内无响应会重试 3 次。
%%%
%%% 本模块只做「验签 → 解密 → 记录 →（可选）被动回复」：
%%%   - **不做任何同步的外部调用**（LLM / 对象存储 / 其它 HTTP）—— 那会把
%%%     5 秒预算花光并触发重试风暴。需要重活时应当在此落库或入队后立即返回，
%%%     与 `moya_ai_worker` 的「入队 + ecron 消费」同款。
%%%   - **不落原始 openid**：日志只记 sha256 前 12 位指纹。微信侧 openid 是
%%%     可跨会话追踪的个人标识，属于 PII，日志留存本身就是风险面。
%%%
%%% 安全约束：
%%%   - Token / EncodingAESKey 只从服务端 config 读
%%%     （wechat_mini_msg_push_token / wechat_mini_msg_push_aes_key），
%%%     任一缺失 → provider_unconfigured（fail-closed，不静默降级为「不验签」）
%%%   - 密文必须验 `msg_signature` 且解密后的 appid 与本环境 wechat_mini_appid
%%%     一致。appid 这一关不能省：EncodingAESKey 一旦在多应用间复用（或泄漏），
%%%     别人发来的报文结构同样合法，只有 appid 能区分「这条本来发给谁」
%%%   - 被动回复默认**关闭**（wechat_mini_msg_push_reply 为空即回空串）。
%%%     回复体形状一旦不对，微信会当作没收到而重试，反而放大日志；
%%%     需要时再显式开
%%%
%%% ⚠️ 明文模式下的取舍：若请求体没有 `Encrypt`（即后台选了「明文模式」），
%%% 微信**不提供任何签名**，因此这一支无法验签 —— 这是该模式自身的取舍，不是
%%% 本模块的疏漏。当前生产选的是「兼容模式」，请求带 Encrypt，走严格验签分支。
%%% @end
%%%===================================================================

-export([verify_url/1, handle_push/2]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 微信首次保存配置时的 GET 校验。
%% 出参 {ok, EchoStr} 表示验签通过、应把 EchoStr 原样回给微信。
-spec verify_url(map()) -> {ok, binary()} | {error, atom()}.
verify_url(Query) ->
    case provider_config() of
        {error, _} = E ->
            E;
        {ok, #{token := Token}} ->
            Signature = qget(Query, <<"signature">>),
            Timestamp = qget(Query, <<"timestamp">>),
            Nonce = qget(Query, <<"nonce">>),
            EchoStr = qget(Query, <<"echostr">>),
            case elib_wechat_msg:verify(Token, Timestamp, Nonce, Signature) of
                true when EchoStr =/= <<>> ->
                    {ok, EchoStr};
                true ->
                    {error, missing_echostr};
                false ->
                    {error, bad_signature}
            end
    end.

%% @doc 处理微信推送的事件（已解码的 JSON body）。
%% 出参 {ok, Reply}：Reply 是要回给微信的响应体，空串表示「收到，不回复」。
-spec handle_push(map(), map()) -> {ok, binary()} | {error, atom()}.
handle_push(Query, Body) when is_map(Body) ->
    case provider_config() of
        {error, _} = E ->
            E;
        {ok, #{token := Token, aes_key := AesKey}} ->
            case decode_event(Token, AesKey, Query, Body) of
                {error, _} = E ->
                    E;
                {ok, Event, Mode} ->
                    audit(Event, Mode),
                    {ok, reply(Event)}
            end
    end;
handle_push(_Query, _Body) ->
    {error, malformed_event}.

%%%===================================================================
%%% Internal：配置
%%%===================================================================

-spec provider_config() -> {ok, map()} | {error, provider_unconfigured}.
provider_config() ->
    Token = safe_binary(config_ds:env(wechat_mini_msg_push_token, <<>>)),
    AesKey = safe_binary(config_ds:env(wechat_mini_msg_push_aes_key, <<>>)),
    case {Token, AesKey} of
        {<<>>, _} -> {error, provider_unconfigured};
        {_, <<>>} -> {error, provider_unconfigured};
        _ -> {ok, #{token => Token, aes_key => AesKey}}
    end.

-spec expected_appid() -> binary().
expected_appid() ->
    safe_binary(config_ds:env(wechat_mini_appid, <<>>)).

-spec reply_text() -> binary().
reply_text() ->
    safe_binary(config_ds:env(wechat_mini_msg_push_reply, <<>>)).

%%%===================================================================
%%% Internal：解包
%%%===================================================================

-spec decode_event(binary(), binary(), map(), map()) ->
    {ok, map(), atom()} | {error, atom()}.
decode_event(Token, AesKey, Query, Body) ->
    case maps:get(<<"Encrypt">>, Body, <<>>) of
        <<>> ->
            %% 明文模式：微信不提供签名，无可校验项（见模块头注的取舍说明）
            {ok, Body, plain};
        Encrypt ->
            Timestamp = qget(Query, <<"timestamp">>),
            Nonce = qget(Query, <<"nonce">>),
            MsgSignature = qget(Query, <<"msg_signature">>),
            case elib_wechat_msg:verify(Token, Timestamp, Nonce, Encrypt, MsgSignature) of
                false ->
                    {error, bad_msg_signature};
                true ->
                    decrypt_event(Encrypt, AesKey)
            end
    end.

-spec decrypt_event(binary(), binary()) -> {ok, map(), atom()} | {error, atom()}.
decrypt_event(Encrypt, AesKey) ->
    case elib_wechat_msg:decrypt(Encrypt, AesKey) of
        {error, Reason} ->
            ?LOG_ERROR("moya_wechat_msg decrypt failed ~p", [Reason]),
            {error, decrypt_failed};
        {ok, #{msg := Msg, appid := AppId}} ->
            case expected_appid() of
                AppId ->
                    decode_payload(Msg);
                _ ->
                    %% 不把拿到的 appid 写进日志：它可能是别人的应用标识
                    ?LOG_ERROR("moya_wechat_msg appid mismatch"),
                    {error, appid_mismatch}
            end
    end.

-spec decode_payload(binary()) -> {ok, map(), atom()} | {error, atom()}.
decode_payload(Msg) ->
    try jsx:decode(Msg, [return_maps]) of
        Event when is_map(Event) -> {ok, Event, encrypted};
        _NotObject -> {error, malformed_event}
    catch
        _:_ -> {error, malformed_event}
    end.

%%%===================================================================
%%% Internal：记录与回复
%%%===================================================================

%% @doc 结构化审计：只落 openid 指纹，不落原始 openid（PII）。
-spec audit(map(), atom()) -> ok.
audit(Event, Mode) ->
    ?LOG_INFO(
        "moya_wechat_msg recv mode=~p msg_type=~ts event=~ts from=~ts to=~ts",
        [
            Mode,
            maps:get(<<"MsgType">>, Event, <<>>),
            maps:get(<<"Event">>, Event, <<>>),
            fingerprint(maps:get(<<"FromUserName">>, Event, <<>>)),
            maps:get(<<"ToUserName">>, Event, <<>>)
        ]
    ),
    ok.

-spec fingerprint(binary()) -> binary().
fingerprint(<<>>) ->
    <<"-">>;
fingerprint(Bin) ->
    <<Head:12/binary, _/binary>> = binary:encode_hex(crypto:hash(sha256, Bin), lowercase),
    Head.

%% @doc 被动回复：仅当显式配置了文案、且来的是文本客服消息时才回。
%% 其它形态（事件、图片、语音…）一律回空串 —— 微信视空串为「已收到，无需回复」。
-spec reply(map()) -> binary().
reply(Event) ->
    case {reply_text(), maps:get(<<"MsgType">>, Event, <<>>)} of
        {<<>>, _} ->
            <<>>;
        {Text, <<"text">>} ->
            encode_reply(Event, Text);
        {_Text, _Other} ->
            <<>>
    end.

-spec encode_reply(map(), binary()) -> binary().
encode_reply(Event, Text) ->
    %% ToUserName / FromUserName 要**互换**：回复的收件人是发消息的用户
    jsx:encode(#{
        <<"ToUserName">> => maps:get(<<"FromUserName">>, Event, <<>>),
        <<"FromUserName">> => maps:get(<<"ToUserName">>, Event, <<>>),
        <<"CreateTime">> => erlang:system_time(second),
        <<"MsgType">> => <<"text">>,
        <<"Content">> => Text
    }).

%%%===================================================================
%%% Internal：工具
%%%===================================================================

-spec qget(map(), binary()) -> binary().
qget(Query, Key) ->
    safe_binary(maps:get(Key, Query, <<>>)).

-spec safe_binary(term()) -> binary().
safe_binary(V) ->
    elib_cnv:safe_to_binary(V).
