-module(token_ds).
%%%
% token_ds 是 token domain service 缩写
%%%

-export([encrypt_token/1]).
-export([encrypt_refreshtoken/1]).
-export([decrypt_token/1]).
% -export ([get_uid/1]).

-include_lib("imlib/include/common.hrl").
-include_lib("imlib/include/log.hrl").

-type token_type() :: rtk | tk.


%% 生成refreshtoken
-spec encrypt_refreshtoken(iodata()) -> any().
encrypt_refreshtoken(ID) ->
    encrypt_token(ID, ?REFRESHTOKEN_VALID, rtk).


%% 生成token
% io:format("~s~n", [token_ds:encrypt_token(1)]).
-spec encrypt_token(iodata()) -> any().
encrypt_token(ID) ->
    encrypt_token(ID, ?TOKEN_VALID, tk).


%% 解析token
decrypt_token(Token) ->
    % io:format("Token: ~p, ~n", [Token]),
    Opts = #{exp_leeway => 300},  % 容忍 5 分钟时钟偏差
    try jwerl:verify(Token, hs256, config_ds:get(jwt_key), #{}, Opts) of
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


%% 生成token
-spec encrypt_token(iodata(), integer(), token_type()) -> any().
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
    jwerl:sign(Data, hs256, config_ds:get(jwt_key)).
