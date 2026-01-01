-module(token_ds).
%%%
% token_ds 是 token domain service 缩写
%%%

-export([encrypt_token/1]).
-export([encrypt_refreshtoken/1]).
-export([decrypt_token/1]).
% -export ([get_uid/1]).

-include("include/common.hrl").
-include("log.hrl").

-type token_type() :: rtk | tk.
-type user_id() :: integer() | binary().
-type token() :: binary().
-type expires_at() :: integer().
-type token_subject() :: integer().
-type error_code() :: integer().
-type error_msg() :: binary() | string().
-type error_info() :: map().
-type token_result() :: {ok, user_id(), expires_at(), token_subject()} |
                        {error, error_code(), error_msg(), error_info()}.


%% @doc 生成refresh token
%% 生成用于刷新访问令牌的长效令牌，有效期由?REFRESHTOKEN_VALID定义。
%% @param ID 用户ID或标识符
%% @returns 编码后的JWT refresh token
-spec encrypt_refreshtoken(user_id()) -> token().
encrypt_refreshtoken(ID) ->
    encrypt_token(ID, ?REFRESHTOKEN_VALID, rtk).


%% @doc 生成访问token
%% 生成用于用户认证的访问令牌，有效期由?TOKEN_VALID定义。
%% 使用HS256算法和配置的JWT密钥进行签名。
%%
%% 使用示例：
%% io:format("~s~n", [token_ds:encrypt_token(1)]).
%% @param ID 用户ID或标识符
%% @returns 编码后的JWT access token
-spec encrypt_token(user_id()) -> token().
encrypt_token(ID) ->
    encrypt_token(ID, ?TOKEN_VALID, tk).


%% @doc 解析token
%% 验证并解析JWT token，提取用户ID、过期时间和主题信息。
%% 支持5分钟的时钟偏差容错，验证token签名和有效性。
%% @param Token JWT token字符串
%% @returns 解析结果：成功时返回用户ID、过期时间和主题；失败时返回错误信息
-spec decrypt_token(token()) -> token_result().
decrypt_token(Token) ->
    % io:format("Token: ~p, ~n", [Token]),
    Opts = #{exp_leeway => 300},  % 容忍 5 分钟时钟偏差
    JwtKey = config_ds:env(jwt_key, <<>>),
    try jwerl:verify(Token, hs256, JwtKey, #{}, Opts) of
        {ok, Payload} ->
            Uid = maps:get(uid, Payload, 0),
            ID = imboy_hashids:decode(Uid),
            ExpireDAt = maps:get(exp, Payload, <<>>),
            Sub = maps:get(sub, Payload, 0),
            Now = imboy_dt:utc(second),
            if
                ExpireDAt > Now ->
                    {ok, ID, ExpireDAt, Sub};
                true ->
                    {error, 705, "Please refresh token", #{uid => ID, expired_at => ExpireDAt}}
            end;
        JWT_ERR ->
            ?DEBUG_LOG(['JWT_ERR', JWT_ERR]),
            {error, 706, "Invalid token", #{err => JWT_ERR}}
    catch
        Class:Reason:Stacktrace ->
            % 异常处理代码
            io:format("Class: ~p, Reason: ~p, Stacktrace ~p~n", [Class, Reason, Stacktrace]),
            {error, 706, "Invalid token.", #{}}
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%% @doc 内部token生成函数
%% 根据用户ID、有效时间和主题类型生成JWT token。
%% 用户ID会通过hashids编码后放入payload中。
%% @param ID 用户ID或标识符
%% @param Second token有效期（秒）
%% @param Sub token主题类型（tk表示access token，rtk表示refresh token）
%% @returns 编码后的JWT token
-spec encrypt_token(user_id(), integer(), token_type()) -> token().
encrypt_token(ID, Second, Sub) ->
    ExpireDAt = erlang:system_time(second) + Second,
    Data = #{
         % iss => imboy  % iss (issuer)：签发人
         % , nbf => Now + 1 % nbf (Not Before)：生效时间
         % , iat => Now % iat (Issued At)：签发时间
         sub => Sub  % sub (subject)：主题
         , exp => ExpireDAt  % exp (expiration time)：过期时间
         , uid => imboy_hashids:encode(ID)
    },
    JwtKey = config_ds:env(jwt_key, <<>>),
    jwerl:sign(Data, hs256, JwtKey).
