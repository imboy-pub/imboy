-module(verify_middleware).

-include_lib("imboy/include/log.hrl").

-behaviour(cowboy_middleware).

-export([execute/2]).


%% 这个是回调函数
execute(Req, Env) ->
    % app version 1.0.0
    Vsn = cowboy_req:header(<<"vsn">>, Req),
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
    case verify_sign(Sign, Vsn) of
        true ->
            {ok, Req, Env};
        false ->
            Req1 = imboy_response:error(Req,
                                       "Failed to verify the signature",
                                       1),
            {stop, Req1}
    end.


% 内部方法
verify_sign(undefined, _Vsn) ->
    false;
verify_sign(_Sign, undefined) ->
    false;
verify_sign(Sign, Vsn) ->
    {ok, AuthKeys} = application:get_env(imboy, auth_keys),
    Key = proplists:get_value(Vsn, AuthKeys),
    Str = Key ++ binary_to_list(Vsn),
    imboy_hasher:md5(Str) == Sign.
