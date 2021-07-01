-module (token_ds).
%%%
% token_ds 是 token domain service 缩写
%%%
-export ([encrypt_token/1, encrypt_refreshtoken/1, decrypt_token/1]).
% -export ([get_uid/1]).

-include("common.hrl").

-type token_type() :: rtk | tk.

%% 生成refreshtoken
encrypt_refreshtoken(Id) ->
    encrypt_token(Id, ?REFRESHTOKEN_VALID, rtk).

%% 生成token
encrypt_token(Id) ->
    encrypt_token(Id, ?TOKEN_VALID, tk).

%% 解析token
decrypt_token(Token) ->
    try
        jwerl:verify(Token, hs256, ?JWT_KEY)
    of
        {ok, Payload} ->
            Uid = maps:get(uid, Payload, 0),
            Id = hashids_translator:uid_decode(Uid),
            ExpireAt = maps:get(exp, Payload, 0),
            Sub = maps:get(sub, Payload, 0),
            {ok, Id, ExpireAt, Sub};
        {error, JWT_ERR} ->
            ?LOG([expire,JWT_ERR]),
            {error, 705, "请刷新token", []};
        JWT_ERR ->
            ?LOG([decrypterror, JWT_ERR]),
            {error, 706, "token无效", []}
    catch _:_ ->
        ?LOG(['catch', Token]),
        {error, 706, "token无效", []}
    end.
% decrypt_token(Token) ->
%     try
%         Token2 = imboy_cipher:aes_decrypt(Token),
%         binary:split(Token2, <<$,>>, [global, trim])
%     of
%         [Id | [ExpireAt | [Sub | _]]] ->
%             % {ok, Id, ExpireAt, Sub};
%             Now = dt_util:milliseconds(),
%             Expire = list_to_integer(binary_to_list(ExpireAt)) - Now,
%             if
%                 Expire > 0 ->
%                     {ok, Id, ExpireAt, Sub};
%                 true ->
%                     ?LOG([expire, Id, ExpireAt, Now, Expire]),
%                     {error, 705, "请刷新token", [Id, ExpireAt]}
%             end;
%         _ ->
%             ?LOG([decrypterror, '']),
%             {error, 706, "token无效", []}
%     catch _:_ ->
%         ?LOG(['catch', Token]),
%         {error, 706, "token无效", []}
%     end.

%% Internal.

%% 生成token
-spec encrypt_token(iodata(), integer(), token_type()) -> any().
encrypt_token(Id, Millisecond, Sub) when is_integer(Id)  ->
    Id2 = integer_to_binary(Id),
    encrypt_token(Id2, Millisecond, Sub);
encrypt_token(Id, Millisecond, Sub) ->
    % Now = os:system_time(seconds),
    % ExpireAt = Now + Millisecond / 1000,
    ExpireAt = dt_util:milliseconds() + Millisecond,
    Data = #{
        % iss => imboy  % iss (issuer)：签发人
        % , nbf => Now + 1 % nbf (Not Before)：生效时间
        % , iat => Now % iat (Issued At)：签发时间
        sub => Sub % sub (subject)：主题
        , exp => ExpireAt % exp (expiration time)：过期时间
        , uid => hashids_translator:uid_encode(Id)
    },
    % ?LOG(jwerl:sign(Data, hs256, ?JWT_KEY)),
    jwerl:sign(Data, hs256, ?JWT_KEY).
