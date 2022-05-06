-module(ds_token).
%%%
% ds_token 是 token domain service 缩写
%%%

-export([encrypt_token/1]).
-export([encrypt_refreshtoken/1]).
-export([decrypt_token/1]).
% -export ([get_uid/1]).

-include("common.hrl").

-type token_type() :: rtk | tk.


%% 生成refreshtoken
encrypt_refreshtoken(ID) ->
    encrypt_token(ID, ?REFRESHTOKEN_VALID, rtk).


%% 生成token
encrypt_token(ID) ->
    encrypt_token(ID, ?TOKEN_VALID, tk).


%% 解析token
decrypt_token(Token) ->
    try
        jwerl:verify(Token, hs256, ?JWT_KEY)
    of
        {ok, Payload} ->
            Uid = maps:get(uid, Payload, 0),
            ID = hashids_translator:uid_decode(Uid),
            ExpireAt = maps:get(exp, Payload, 0),
            Sub = maps:get(sub, Payload, 0),
            {ok, ID, ExpireAt, Sub};
        {error, _JWT_ERR} ->
            {error, 705, "请刷新token", []};
        _JWT_ERR ->
            {error, 706, "token无效", []}
    catch
        _:_ ->
            {error, 706, "token无效", []}
    end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% 生成token
-spec encrypt_token(iodata(), integer(), token_type()) -> any().
encrypt_token(ID, Millisecond, Sub) when is_integer(ID) ->
    ID2 = integer_to_binary(ID),
    encrypt_token(ID2, Millisecond, Sub);
encrypt_token(ID, Millisecond, Sub) ->
    % Now = os:system_time(seconds),
    % ExpireAt = Now + Millisecond / 1000,
    ExpireAt = util_dt:milliseconds() + Millisecond,
    Data = #{
             % iss => imboy  % iss (issuer)：签发人
             % , nbf => Now + 1 % nbf (Not Before)：生效时间
             % , iat => Now % iat (Issued At)：签发时间
             sub => Sub  % sub (subject)：主题
             ,
             exp => ExpireAt  % exp (expiration time)：过期时间
             ,
             uid => hashids_translator:uid_encode(ID)},
    jwerl:sign(Data, hs256, ?JWT_KEY).
