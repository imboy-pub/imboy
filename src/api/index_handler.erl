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

    SolKey = config_ds:get(<<"solidified_key">>),
    SignKey =
        case app_version_ds:sign_key(DType, SignKeyVsn, Pkg) of
            <<>> ->
                SolKey;
            SK when is_binary(SK) ->
                SK
        end,
    Data =
        #{<<"ws_url">> => config_ds:get(<<"ws_url">>),
          <<"upload_url">> => config_ds:get(<<"upload_url">>),
          <<"upload_key">> => config_ds:get(<<"upload_key">>),
          <<"upload_scene">> => config_ds:get(<<"upload_scene">>),
          <<"login_pwd_rsa_encrypt">> => config_ds:get(<<"login_pwd_rsa_encrypt">>),
          <<"login_rsa_pub_key">> => config_ds:get(<<"login_rsa_pub_key">>)},
    % ?DEBUG_LOG([DType, Vsn, Pkg, SignKey, Data]),
    % elib_response:success(Req0, Data, "success.").
    IV = config_ds:get(<<"solidified_key_iv">>),
    Key = elib_hasher:md5(SignKey),
    %% AES-256-CBC 强约束：Key 必须 32 字节，IV 必须 16 字节
    %% 配置缺失时 fail fast，避免 crypto_init 崩到 cowboy stream（Bad iv size）
    case {byte_size(IV), iolist_size(Key)} of
        {16, 32} -> ok;
        {IvLen, KeyLen} ->
            ?ERROR_LOG("index_handler api_init: bad crypto params, key_len=~p iv_len=~p (expect key=32, iv=16). "
                       "Run config_ds:set(<<\"solidified_key\">>, <<32-bytes>>) and "
                       "config_ds:set(<<\"solidified_key_iv\">>, <<16-bytes>>).",
                       [KeyLen, IvLen]),
            erlang:error({bad_crypto_config, #{key_len => KeyLen, iv_len => IvLen}})
    end,
    Bin = elib_cipher:aes_encrypt(aes_256_cbc, jsone:encode(Data), Key, IV),
    Test = case elib_pg:query(<<"SELECT to_tsquery('jiebacfg', '软件中国')"/utf8>>, []) of
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
        "\n        <meta charset=\"utf-8\"/>\n        <meta http-equiv=\"Cont"
        "ent-Language\" content=\"zh-CN\">\n        <h1>API列表</h1>\n "
        "       <ol>\n            <li><a href=\"/init\" target=\"_blank\">/in"
        "it  GET</a></li>\n            <li><a href=\"/conversation/online\" "
        "target=\"_blank\">\n                /conversation/online  GET</a></l"
        "i>\n        </ol>\n    ",
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"text/html">>},
                     unicode:characters_to_binary(Body, utf8),
                     Req0).
