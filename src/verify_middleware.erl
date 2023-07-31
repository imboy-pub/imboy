-module(verify_middleware).

-include_lib("imboy/include/log.hrl").

-behaviour(cowboy_middleware).

-export([execute/2]).


%% 这个是回调函数
execute(Req, Env) ->
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
    [X,Y, _Z] = binary:split(Vsn, <<".">>, [global]),
    VsnXY = iolist_to_binary([X, ".", Y]),
    PlainText = iolist_to_binary([Did, "|", VsnXY]),
    case verify_sign(Sign, PlainText, VsnXY, Method) of
        true ->
            {ok, Req, Env};
        false ->
            Req1 = imboy_response:error(
                % 签名错误，需要下载最新版本APP
                Req, "Failed to verify the signature", 707
            ),
            {stop, Req1}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

verify_sign(undefined, _, _VsnXY, _Method) ->
    false;
verify_sign(_Sign, _, undefined, _Method) ->
    false;

verify_sign(Sign, PlainText, VsnXY, <<"sha256">>) ->
    AuthKeys = config_ds:env(auth_keys),
    Key = proplists:get_value(VsnXY, AuthKeys),
    imboy_hasher:hmac_sha256(PlainText, Key) == Sign;
verify_sign(Sign, PlainText, VsnXY, <<"sha512">>) ->
    AuthKeys = config_ds:env(auth_keys),
    Key = proplists:get_value(VsnXY, AuthKeys),
    imboy_hasher:hmac_sha512(PlainText, Key) == Sign;
verify_sign(Sign, PlainText, _VsnXY, <<"md5">>) ->
    imboy_hasher:md5(PlainText) == Sign;
verify_sign(_, _, _, _) ->
    false.
