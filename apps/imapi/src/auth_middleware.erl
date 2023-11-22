-module(auth_middleware).
-behaviour(cowboy_middleware).

-include_lib("imlib/include/log.hrl").

-export([execute/2]).
-export([remove_last_forward_slash/1]).


%% 这个是回调函数
execute(Req, Env) ->
    Path = remove_last_forward_slash(cowboy_req:path(Req)),
    case Path of
        <<"/static/", _Tail/binary>> ->
            {ok, Req, Env};
        <<"/webrtc/", _Tail/binary>> ->
            {ok, Req, Env};
        % 为了调试方便，先不鉴权
        <<"/live_room/", _Tail/binary>> ->
            {ok, Req, Env};
        <<"/whip/", _Tail/binary>> ->
            {ok, Req, Env};
        _ ->
            OpenLi = imboy_router:open(),
            OptionLi = imboy_router:option(),
            InOpenLi = lists:member(Path, OpenLi),
            InOptionLi = lists:member(Path, OptionLi),
            Switch = config_ds:env(api_auth_switch),
            Passport = string:sub_string(binary_to_list(Path), 1, 10),
            Res1 =
                if
                    Path == <<"/ws">>, Switch == on ->
                        verify_sign(Req, Env);
                    Path == <<"/init">>, Switch == on ->
                        verify_sign(Req, Env);
                    Path == <<"/refreshtoken">>, Switch == on ->
                        verify_sign(Req, Env);
                    Passport == "/passport/", Switch == on ->
                        verify_sign(Req, Env);
                    InOpenLi == false, Switch == on ->
                        verify_sign(Req, Env);
                    true ->
                        {ok, Req, Env}
                end,
            case Res1 of
                {ok, Req, Env} ->
                    Authorization = cowboy_req:header(<<"authorization">>, Req),
                    condition(InOptionLi, InOpenLi, Authorization, Req, Env);
                Res2 ->
                    Res2
            end
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
verify_sign(Req, Env) ->
    % app version 1.0.0
    Vsn = cowboy_req:header(<<"vsn">>, Req, <<"0.1.1">>),
    Did = cowboy_req:header(<<"did">>, Req, <<>>),
    % /// A string representing the operating system or platform.
    % ///
    % /// Possible values include:
    % ///   "android"
    % ///   "fuchsia"
    % ///   "ios"
    % ///   "linux"
    % ///   "macos"
    % ///   "windows"
    % ///
    % /// Note that this list may change over time so platform-specific logic
    % /// should be guarded by the appropriate boolean getter e.g. [isMacOS].
    _ClientOS = cowboy_req:header(<<"cos">>, Req),

    %A string representing the version of the operating system or platform.
    %
    %The format of this string will vary by operating system, platform and
    %version and is not suitable for parsing. For example:
    %  "Linux 5.11.0-1018-gcp \
    %   #20~20.04.2-Ubuntu SMP Fri Sep 3 01:01:37 UTC 2021"
    %  "Version 14.5 (Build 18E182)"
    %  '"Windows 10 Pro" 10.0 (Build 19043)'
    _ClientOSVsn = cowboy_req:header(<<"cosv">>, Req),

    % 签名结果
    % sign =
    Sign = cowboy_req:header(<<"sign">>, Req),
    Method = cowboy_req:header(<<"method">>, Req),
    [X, Y, _Z] = binary:split(Vsn, <<".">>, [global]),
    VsnXY = iolist_to_binary([X, ".", Y]),
    PlainText = iolist_to_binary([Did, "|", VsnXY]),
    case do_verify_sign(Sign, PlainText, VsnXY, Method) of
        true ->
            {ok, Req, Env};
        false ->
            Req1 = imboy_response:error(
                     % 签名错误，需要下载最新版本APP
                     Req,
                     "Failed to verify the signature",
                     707),
            {stop, Req1}
    end.


do_verify_sign(undefined, _, _VsnXY, _Method) ->
    false;
do_verify_sign(_Sign, _, undefined, _Method) ->
    false;

do_verify_sign(Sign, PlainText, VsnXY, <<"sha256">>) ->
    AuthKeys = config_ds:env(auth_keys),
    Key = proplists:get_value(VsnXY, AuthKeys),
    imboy_hasher:hmac_sha256(PlainText, Key) == Sign;
do_verify_sign(Sign, PlainText, VsnXY, <<"sha512">>) ->
    AuthKeys = config_ds:env(auth_keys),
    Key = proplists:get_value(VsnXY, AuthKeys),
    imboy_hasher:hmac_sha512(PlainText, Key) == Sign;
do_verify_sign(Sign, PlainText, _VsnXY, <<"md5">>) ->
    imboy_hasher:md5(PlainText) == Sign;
do_verify_sign(_, _, _, _) ->
    false.


condition(true, _, undefined, Req, Env) ->
    {ok, Req, Env};
condition(true, _, Authorization, Req, Env) ->
    do_authorization(Authorization, Req, Env);
condition(_, true, _, Req, Env) ->
    {ok, Req, Env};
condition(_, _, Authorization, Req, Env) ->
    do_authorization(Authorization, Req, Env).


do_authorization(undefined, Req, _Env) ->
    {stop, Req};
do_authorization(Authorization, Req, Env) ->
    % ?LOG(['Authorization', Authorization]),
    case token_ds:decrypt_token(Authorization) of
        {ok, Id, _ExpireAt, <<"tk">>} when is_integer(Id) ->
            #{handler_opts := HandlerOpts} = Env,
            Env2 = Env#{handler_opts := HandlerOpts#{current_uid => Id}},
            {ok, Req, Env2};
        {ok, _Id, _ExpireAt, <<"rtk">>} ->
            Err = "Does not support refreshtoken",
            Req1 = imboy_response:error(Req, Err, 1),
            {stop, Req1};
        {error, Code, Msg, _Map} ->
            Req1 = imboy_response:error(Req, Msg, Code),
            {stop, Req1}
    end.


%% Remove the last forward slash
%% 删除最后一个正斜杠
%% auth_middleware:remove_last_forward_slash(<<"/abc/">>).
%%  will be echo <<"/abc">>
-spec remove_last_forward_slash(binary()) -> binary().
remove_last_forward_slash(<<"">>) ->
    <<"/">>;
remove_last_forward_slash(<<"/">>) ->
    <<"/">>;
remove_last_forward_slash(Path) ->
    case binary:last(Path) of
        47 ->
            binary:part(Path, 0, byte_size(Path) - 1);
        _ ->
            Path
    end.
