-module(token_ds).
%%%
% token_ds 是 token domain service 缩写
%%%

-export([encrypt_token/1]).
-export([encrypt_refreshtoken/1]).
-export([decrypt_token/1]).
% -export ([get_uid/1]).

-include_lib("imlib/include/common.hrl").

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
    try jwerl:verify(Token, hs256, config_ds:env(jwt_key)) of
        {ok, Payload} ->
            Uid = maps:get(uid, Payload, 0),
            ID = imboy_hashids:uid_decode(Uid),
            ExpireAt = maps:get(exp, Payload, 0),
            Sub = maps:get(sub, Payload, 0),
            Now = imboy_dt:utc(millisecond),
            if
                (ExpireAt - Now) > 0 ->
                    {ok, ID, ExpireAt, Sub};
                true ->
                    {error, 705, "Please refresh token", #{uid => ID, expire_at => ExpireAt}}
            end;
        _JWT_ERR ->
            {error, 706, "Invalid token", #{}}
    catch
        _:_ ->
            {error, 706, "Invalid token.", #{}}
    end.


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================


%% 生成token
-spec encrypt_token(iodata(), integer(), token_type()) -> any().
encrypt_token(ID, Millisecond, Sub) ->
    ExpireAt = imboy_dt:utc(millisecond) + Millisecond,
    Data = #{
             % iss => imboy  % iss (issuer)：签发人
             % , nbf => Now + 1 % nbf (Not Before)：生效时间
             % , iat => Now % iat (Issued At)：签发时间
             sub => Sub  % sub (subject)：主题
             ,
             exp => ExpireAt  % exp (expiration time)：过期时间
             ,
             uid => imboy_hashids:uid_encode(ID)
            },
    jwerl:sign(Data, hs256, config_ds:env(jwt_key)).
