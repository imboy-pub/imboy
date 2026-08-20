-module(index_handler).

-include("log.hrl").

-behavior(cowboy_rest).

-export([init/2]).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            init ->
                api_init(Req0);
            help ->
                get_help(Req0);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% @doc API 初始化端点
%% 返回API初始化配置信息，包括WebSocket URL、上传配置等
%%
%% @param Req0 Cowboy请求对象，包含版本和设备信息
%% @return 返回包含配置信息的响应
%% @end
-spec api_init(cowboy_req:req()) -> cowboy_req:req().
api_init(Req0) ->
    % 'sign': EncrypterService.sha512("$deviceId|$appVsn|$cos|$packageName", key)
    % Did = cowboy_req:header(<<"did">>, Req0, <<>>),
    Vsn = cowboy_req:header(<<"vsn">>, Req0, <<>>),
    DType = cowboy_req:header(<<"cos">>, Req0, <<>>),
    Pkg = cowboy_req:header(<<"pkg">>, Req0, <<>>),
    SignKeyVsn = cowboy_req:header(<<"sk">>, Req0, Vsn),

    SolKey = config_ds:env(solidified_key),
    SignKey =
        case app_version_logic:sign_key(DType, SignKeyVsn, Pkg) of
            <<>> ->
                SolKey;
            SK when is_binary(SK) ->
                SK
        end,
    Data =
        #{
            <<"ws_url">> => config_ds:env(ws_url, <<>>),
            %% 旧字段保留空值，供旧版 Flutter 客户端过渡期使用
            <<"upload_url">> => config_ds:env(upload_url, <<"https://s3.imboy.pub">>),
            <<"upload_key">> => <<>>,
            <<"upload_scene">> => <<>>,
            %% 新附件直传接口（Garage S3 presigned URL）
            %% 2026-07-07 43224c1f/4cc20e81 硬切换 /api 前缀后此处漏改，
            %% 真实路由是 /api/v1/attachment/presign（见 imboy_router.erl），
            %% 客户端拿到的旧值会 404。
            <<"attach_presign_endpoint">> => <<"/api/v1/attachment/presign">>,
            %% 公开资源（scope=public，如头像）直读基址，客户端直拼 object_key（见 resource-access-control.md §9）
            <<"public_base_url">> => elib_oss:public_base_url(),
            %% #100 契约修复：客户端与服务端的加解密开关只认 <<"1">>（见
            %% passport_handler ?RSA_ENCRYPT_YES 与客户端 rsaEncrypt == "1"
            %% 判定），配置值却是 on/off——配置为 on 时加密分支静默失效。
            %% 此处把配置归一为线协议 1/0，旧客户端（同样判 "1"）一并打通。
            <<"login_pwd_rsa_encrypt">> =>
                case config_ds:env(login_pwd_rsa_encrypt, <<"off">>) of
                    <<"on">> -> <<"1">>;
                    <<"1">> -> <<"1">>;
                    _ -> <<"0">>
                end,
            <<"login_rsa_pub_key">> => config_ds:env(login_rsa_pub_key)
        },
    % ?DEBUG_LOG([DType, Vsn, Pkg, SignKey, Data]),
    % elib_response:success(Req0, Data, "success.").
    IV = config_ds:env(solidified_key_iv),
    Key = elib_hasher:md5(SignKey),
    LegacyCbc = ec_cnv:to_binary(config_ds:env(init_config_legacy_cbc, <<"on">>)),
    %% Key 32 字节是 GCM 与 CBC 的共同要求；**IV 只有 legacy CBC 用得上**。
    %% 关掉 legacy 之后 solidified_key_iv 已无用途，不该再因为它缺失/长度不对
    %% 而崩掉 /api/v1/init —— 否则 #94 的收尾（把开关置 off）会当场打断客户端
    %% 初始化。配置缺失时仍 fail fast，避免 crypto_init 崩到 cowboy stream。
    case {LegacyCbc, byte_size(IV), iolist_size(Key)} of
        {<<"on">>, 16, 32} ->
            ok;
        {<<"on">>, IvLen, KeyLen} ->
            ?ERROR_LOG(
                "index_handler api_init: bad crypto params, key_len=~p iv_len=~p (expect key=32, iv=16). "
                "Set {solidified_key, <<32-bytes>>} and {solidified_key_iv, <<16-bytes>>} in sys.config.",
                [KeyLen, IvLen]
            ),
            erlang:error({bad_crypto_config, #{key_len => KeyLen, iv_len => IvLen}});
        {_, _, 32} ->
            ok;
        {_, _, KeyLen2} ->
            ?ERROR_LOG(
                "index_handler api_init: bad crypto params, key_len=~p (expect 32). "
                "Set {solidified_key, <<32-bytes>>} in sys.config.",
                [KeyLen2]
            ),
            erlang:error({bad_crypto_config, #{key_len => KeyLen2}})
    end,
    Json = jsone:encode(Data),
    %% res_v2：AES-256-GCM（AEAD 自带完整性 + 每次随机 IV），自包含格式
    %% base64(Salt16 ++ IV12 ++ CT ++ Tag16)，AAD=Salt。
    %%
    %% 原 res 是 AES-256-CBC + 固定 IV（solidified_key_iv）且**无认证标签**：
    %% 客户端无法分辨密文是否被篡改，攻击者可在无 TLS 保护的链路上重定向
    %% ws_url / upload_url / login_rsa_pub_key。elib_cipher:aes_gcm_encrypt/2
    %% 的注释已写明"新代码一律用本函数"（审计 #26），此处对齐。
    %%
    %% 注意：这不解决"密钥编译期 baked 进 APK"—— 对称密钥内嵌在客户端里，
    %% 逆向即可取得，加 MAC 也挡不住持有该密钥的人自行构造密文。彻底解决
    %% 需服务端私钥签名 + 客户端公钥验签，属独立改造。
    ResV2 =
        case elib_cipher:aes_gcm_encrypt(Json, Key) of
            {ok, B64} ->
                B64;
            {error, Reason} ->
                ?ERROR_LOG(
                    "index_handler api_init: aes_gcm_encrypt failed ~p", [Reason]
                ),
                <<>>
        end,
    %% 过渡期同时下发 CBC 密文供存量客户端使用。客户端全量升级到读 res_v2 后，
    %% 把 init_config_legacy_cbc 置 off 关掉降级路径 —— 只要 res 还在，
    %% 攻击者就能直接走旧路径，本次加固不算真正闭合。
    Payload =
        case LegacyCbc of
            <<"on">> ->
                #{
                    res => elib_cipher:aes_encrypt(aes_256_cbc, Json, Key, IV),
                    res_v2 => ResV2
                };
            _ ->
                #{res_v2 => ResV2}
        end,
    Test =
        case elib_pg:query(<<"SELECT to_tsquery('jiebacfg', '软件中国')"/utf8>>, []) of
            {ok, [Row]} -> Row;
            _ -> #{}
        end,
    elib_response:success(Req0, Payload#{test => Test}, "success.").

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 获取API帮助页面
%% 返回HTML格式的API列表页面
%%
%% @param Req0 Cowboy请求对象
%% @return 返回HTML响应
%% @end
-spec get_help(cowboy_req:req()) -> cowboy_req:req().
get_help(Req0) ->
    Body =
        "\n"
        "        <meta charset=\"utf-8\"/>\n"
        "        <meta http-equiv=\"Content-Language\" content=\"zh-CN\">\n"
        "        <h1>API列表</h1>\n"
        "        <ol>\n"
        "            <li><a href=\"/api/v1/init\" target=\"_blank\">/api/v1/init  GET</a></li>\n"
        "            <li><a href=\"/api/v1/conversation/online\" target=\"_blank\">/api/v1/conversation/online  GET</a></li>\n"
        "        </ol>\n"
        "    ",
    cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        unicode:characters_to_binary(Body, utf8),
        Req0
    ).
