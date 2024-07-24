-module(index_handler).

-include_lib("imlib/include/log.hrl").

-behavior(cowboy_rest).

-export([init/2]).

%% ===================================================================
%% API
%% ===================================================================


init(Req0, State0) ->
    % ?LOG(State),
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


api_init(Req0) ->
    % 'sign': EncrypterService.sha512("$deviceId|$appVsn|$cos|$packageName", key)
    % Did = cowboy_req:header(<<"did">>, Req0, <<>>),
    Vsn = cowboy_req:header(<<"vsn">>, Req0, <<>>),
    DType = cowboy_req:header(<<"cos">>, Req0, <<>>),
    Pkg = cowboy_req:header(<<"pkg">>, Req0, <<>>),

    SolKey = config_ds:get(solidified_key),
    SignKey = case app_version_ds:sign_key(DType, Vsn, Pkg) of
        undefined ->
            SolKey;
        SK ->
            SK
    end,
    Data = #{
        <<"ws_url">> => config_ds:get("ws_url"),
        <<"upload_url">> => config_ds:get("upload_url"),
        <<"upload_key">> => config_ds:get("upload_key"),
        <<"upload_scene">> => config_ds:get("upload_scene"),

        <<"login_pwd_rsa_encrypt">> => config_ds:get("login_pwd_rsa_encrypt"),
        <<"login_rsa_pub_key">> => config_ds:get("login_rsa_pub_key")
     },
    % imboy_response:success(Req0, Data, "success.").
    % ?LOG([DType, Vsn, Pkg, SignKey, Data]),
    % imboy_response:success(Req0, Data, "success.").
    IV = config_ds:get(solidified_key_iv),
    Key = imboy_hasher:md5(SignKey),
    Bin = imboy_cipher:aes_encrypt(aes_256_cbc, jsone:encode(Data), Key, IV),
    imboy_response:success(Req0, #{res => Bin}, "success.").


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================



get_help(Req0) ->
    Body = "
        <meta charset=\"utf-8\"/>
        <meta http-equiv=\"Content-Language\" content=\"zh-CN\">
        <h1>API列表</h1>
        <ol>
            <li><a href=\"/init\" target=\"_blank\">/init  GET</a></li>
            <li><a href=\"/conversation/online\" target=\"_blank\">
                /conversation/online  GET</a></li>
        </ol>
    ",
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, unicode:characters_to_binary(Body, utf8), Req0).
