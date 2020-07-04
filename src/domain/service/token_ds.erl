-module (token_ds).
%%%
% token_ds 是 token domain service 缩写
%%%
-export ([encrypt_token/1, encrypt_refreshtoken/1, decrypt_token/1]).
% -export ([get_uid/1]).

-include("imboy.hrl").

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
        Token2 = imboy_cipher:aes_decrypt(Token),
        binary:split(Token2, <<$,>>, [global, trim])
    of
        [Id | [ExpireAt | [Type | _]]] ->
            % {ok, Id, ExpireAt, Type};
            Now = imboy_func:milliseconds(),
            Expire = list_to_integer(binary_to_list(ExpireAt)) - Now,
            if
                Expire > 0 ->
                    {ok, Id, ExpireAt, Type};
                true ->
                    % ?LOG([Id, ExpireAt, Now, Expire]),
                    {error, 707, "请刷新token", [Id, ExpireAt]}
            end;
        _ ->
            {error, 706, "token无效", []}
    catch _:_ ->
        % ?LOG(Token),
        {error, 706, "token无效", []}
    end.


%% Internal.

%% 生成token
-spec encrypt_token(iodata(), integer(), token_type()) -> any().
encrypt_token(Id, Millisecond, Type) when is_integer(Id)  ->
    Id2 = integer_to_binary(Id),
    encrypt_token(Id2, Millisecond, Type);
encrypt_token(Id, Millisecond, Type) ->
    ExpireAt = integer_to_binary(imboy_func:milliseconds() + Millisecond),
    imboy_cipher:aes_encrypt(list_to_binary([Id, ",", ExpireAt, ",", atom_to_binary(Type, utf8) ])).
