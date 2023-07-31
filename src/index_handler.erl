-module(index_handler).

-include_lib("imboy/include/log.hrl").

-behavior(cowboy_rest).

-export([init/2]).

%% ===================================================================
%% API
%% ===================================================================

init(Req0, State0) ->
    % ?LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = case Action of
        init ->
            api_init(Req0);
        help ->
            get_help(Req0);
        false ->
            Req0
    end,
    {ok, Req1, State}.


api_init(Req0) ->
    Vsn = cowboy_req:header(<<"vsn">>, Req0, <<"0.1.1">>),
    [X,Y, _Z] = binary:split(Vsn, <<".">>, [global]),
    VsnXY = iolist_to_binary([X, ".", Y]),

    Data = init_transfer(),
    % imboy_response:success(Req0, Data, "success.").
    AuthKeys = config_ds:env(auth_keys),
    case proplists:get_value(VsnXY, AuthKeys) of
        Key when is_binary(Key); is_list(Key) ->
            IV = config_ds:env(solidified_key_iv),
            Bin = imboy_cipher:aes_encrypt(
                aes_256_cbc
                , jsone:encode(Data)
                , Key
                , IV),
            imboy_response:success(Req0, #{
                res => Bin
            }, "success.");
        _ ->
            imboy_response:success(Req0, #{vsn => Vsn}, "success.")
        end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

init_transfer() ->
    #{
        <<"login_pwd_rsa_encrypt">> => config_ds:get("login_pwd_rsa_encrypt"),
        <<"login_rsa_pub_key">> => config_ds:get("login_rsa_pub_key")
    }.

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
    cowboy_req:reply(
        200
        , #{<<"content-type">> => <<"text/html">>}
        , unicode:characters_to_binary(Body, utf8)
        , Req0
    ).
