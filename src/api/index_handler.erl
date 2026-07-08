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
            <<"login_pwd_rsa_encrypt">> => config_ds:env(login_pwd_rsa_encrypt, <<"off">>),
            <<"login_rsa_pub_key">> => config_ds:env(login_rsa_pub_key)
        },
    % ?DEBUG_LOG([DType, Vsn, Pkg, SignKey, Data]),
    % elib_response:success(Req0, Data, "success.").
    IV = config_ds:env(solidified_key_iv),
    Key = elib_hasher:md5(SignKey),
    %% AES-256-CBC 强约束：Key 必须 32 字节，IV 必须 16 字节
    %% 配置缺失时 fail fast，避免 crypto_init 崩到 cowboy stream（Bad iv size）
    case {byte_size(IV), iolist_size(Key)} of
        {16, 32} ->
            ok;
        {IvLen, KeyLen} ->
            ?ERROR_LOG(
                "index_handler api_init: bad crypto params, key_len=~p iv_len=~p (expect key=32, iv=16). "
                "Set {solidified_key, <<32-bytes>>} and {solidified_key_iv, <<16-bytes>>} in sys.config.",
                [KeyLen, IvLen]
            ),
            erlang:error({bad_crypto_config, #{key_len => KeyLen, iv_len => IvLen}})
    end,
    Bin = elib_cipher:aes_encrypt(aes_256_cbc, jsone:encode(Data), Key, IV),
    Test =
        case elib_pg:query(<<"SELECT to_tsquery('jiebacfg', '软件中国')"/utf8>>, []) of
            {ok, [Row]} -> Row;
            _ -> #{}
        end,
    elib_response:success(Req0, #{test => Test, res => Bin}, "success.").

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
