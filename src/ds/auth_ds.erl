-module(auth_ds).

%%%
% auth 领域服务模块
% auth domain service 缩写
%%%

-include("error_code.hrl").

-export([current_uid/1]).

%% 认证相关导出函数
-export([verify_sign/2]).
-export([do_verify_sign/4]).
-export([verify_token/1]).
-export([parse_authorization_header/1]).
-export([remove_last_forward_slash/1]).
-export([strip_version_prefix/2]).
-export([condition/5]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取资源服务访问token
%% 生成用于访问资源服务的认证令牌。
%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% ===================================================================
%% 认证相关函数
%% ===================================================================

%% @doc 验证请求签名
%% 验证客户端请求的签名是否有效
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return {ok, Req, Env} | {stop, Req}
-spec verify_sign(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
verify_sign(Req, Env) ->
    % app version 1.0.0
    Vsn = cowboy_req:header(<<"vsn">>, Req, <<"0.1.1">>),
    Pkg = cowboy_req:header(<<"pkg">>, Req, <<"pub.imboy.apk">>),
    Did = cowboy_req:header(<<"did">>, Req, <<>>),
    ClientOS = cowboy_req:header(<<"cos">>, Req, <<>>),
    Sign = cowboy_req:header(<<"sign">>, Req),
    Method = cowboy_req:header(<<"method">>, Req),

    PlainText = iolist_to_binary([Did, "|", Vsn, "|", ClientOS, "|", Pkg]),
    SignKeyVsn = cowboy_req:header(<<"sk">>, Req, Vsn),
    Key = app_version_ds:sign_key(ClientOS, SignKeyVsn, Pkg),

    case do_verify_sign(Sign, PlainText, Key, Method) of
        true ->
            {ok, Req, Env};
        false ->
            Req1 = elib_response:error(Req, <<"签名验证失败，请更新客户端"/utf8>>, ?ERR_SIGNATURE_INVALID),
            {stop, Req1}
    end.

%% @doc 执行签名验证的底层实现
-spec do_verify_sign(binary() | undefined, binary(), binary() | undefined, binary() | undefined) ->
    boolean().
do_verify_sign(undefined, _, _, _Method) ->
    false;
do_verify_sign(_Sign, _, undefined, _Method) ->
    false;
do_verify_sign(Sign, PlainText, Key, <<"sha256">>) ->
    % 常数时间比较，防止逐字节比对泄露时序信息（HMAC 签名校验覆盖几乎所有API请求）
    crypto:hash_equals(elib_hasher:hmac_sha256(PlainText, Key), Sign);
do_verify_sign(Sign, PlainText, Key, <<"sha512">>) ->
    crypto:hash_equals(elib_hasher:hmac_sha512(PlainText, Key), Sign);
do_verify_sign(_, _, _, _) ->
    false.

%% @doc 验证 Token
%% 验证并解析 Authorization 头中的 Token
%%
%% @param Authorization Authorization 头的值
%% @return {ok, UserId} | {error, Code, Msg}
-spec verify_token(binary()) -> {ok, integer()} | {error, integer(), binary()}.
verify_token(Authorization) ->
    Token = parse_authorization_header(Authorization),
    case token_ds:decrypt_token(Token) of
        {ok, Id, _ExpireDAt, <<"tk">>} when is_integer(Id) ->
            {ok, Id};
        {ok, _Id, _ExpireDAt, <<"rtk">>} ->
            {error, ?ERR_TOKEN_REFRESH_NOT_ALLOWED, <<"TOKEN REFRESH NOT ALLOWED"/utf8>>};
        {error, Code, Msg, _Map} ->
            {error, Code, Msg}
    end.

%% @doc 解析 Authorization 头
%% 支持 Bearer 开头的 token，自动截断前缀
%% auth_ds:parse_authorization_header(Auth).
%% @param Authorization Authorization 头的值
%% @return Token 二进制字符串
-spec parse_authorization_header(binary()) -> binary().
parse_authorization_header(Authorization) when is_binary(Authorization) ->
    case Authorization of
        <<"Bearer ", Rest/binary>> ->
            Rest;
        _ ->
            Authorization
    end;
parse_authorization_header(_) ->
    <<>>.

%% @doc 移除路径末尾的正斜杠
%% 删除最后一个正斜杠
%%
%% @param Path 路径
%% @return 处理后的路径
%% @example auth_ds:remove_last_forward_slash(<<"/abc/">>). => <<"/abc">>
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

%% @doc 移除路径中的版本前缀
%% 从路径中移除指定的版本前缀
%%
%% @param Path 原始路径
%% @param Prefix 要移除的前缀（如 <<"/v1">>）
%% @return 移除前缀后的路径
%% @example auth_ds:strip_version_prefix(<<"/v1/user/info">>, <<"/v1">>). => <<"/user/info">>
-spec strip_version_prefix(binary(), binary()) -> binary().
strip_version_prefix(Path, Prefix) ->
    PrefixSize = byte_size(Prefix),
    case Path of
        <<Prefix:PrefixSize/binary, Rest/binary>> ->
            Rest;
        _ ->
            Path
    end.

%% @doc 条件判断逻辑
%% 根据路由类型和 Authorization 头决定是否需要验证 Token
%%
%% @param InOptionLi 是否在 option 列表中
%% @param InOpenLi 是否在 open 列表中
%% @param Authorization Authorization 头的值
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return {ok, Req, Env} | {stop, Req}
-spec condition(boolean(), boolean(), binary() | undefined, cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
condition(true, _, undefined, Req, Env) ->
    {ok, Req, Env};
condition(true, _, <<>>, Req, Env) ->
    {ok, Req, Env};
condition(true, _, Authorization, Req, Env) ->
    do_authorization(Authorization, Req, Env);
condition(_, true, _, Req, Env) ->
    {ok, Req, Env};
condition(_, _, Authorization, Req, Env) ->
    do_authorization(Authorization, Req, Env).

%% @doc 执行 Token 授权验证
%%
%% @param Authorization Authorization 头的值
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return {ok, Req, Env} | {stop, Req}
-spec do_authorization(binary() | undefined, cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
do_authorization(undefined, Req, _Env) ->
    {stop, Req};
do_authorization(Authorization, Req, Env) ->
    case verify_token(Authorization) of
        {ok, UserId} ->
            #{handler_opts := HandlerOpts} = Env,
            Env2 = Env#{handler_opts := HandlerOpts#{current_uid => UserId}},
            {ok, Req, Env2};
        {error, Code, Msg} ->
            Req1 = elib_response:error(Req, Msg, Code),
            {stop, Req1}
    end.

% auth_ds:current_uid(State)
-spec current_uid(map()) -> integer().
current_uid(State) ->
    maps:get(current_uid, State, 0).

%% ===================================================================
%% EUnit tests.
%% ===================================================================
