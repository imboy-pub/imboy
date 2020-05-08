-module (token_ds).
%%%
% token_ds 是 token domain service 缩写
%%%
-export ([encrypt_token/1, encrypt_refreshtoken/1, decrypt_token/1]).
% -export ([get_uid/1]).

-include("imboy.hrl").

%% 生成refreshtoken
encrypt_refreshtoken(Id) ->
    encrypt_token(Id, ?REFRESHTOKEN_VALID).

%% 生成token
encrypt_token(Id) ->
    encrypt_token(Id, ?TOKEN_VALID).

%% 解析token
decrypt_token(Token) ->
    try
        Token2 = imboy_cipher:aes_decrypt(Token),
        binary:split(Token2, <<",">>, [])
    of
        [Id | [ExpireAt | _]] ->
            Now = imboy_func:milliseconds(),
            Expire = list_to_integer(binary_to_list(ExpireAt)) - Now,
            if
                Expire > 0 ->
                    {ok, Id, ExpireAt};
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

% get_uid()
%%%%%%%%%%% 下面私有方法，上面为公有方法 %%%%%%%%%%%

%% 生成token
encrypt_token(Id, Millisecond) when is_integer(Id)  ->
    Id2 = integer_to_binary(Id),
    encrypt_token(Id2, Millisecond);
encrypt_token(Id, Millisecond) ->
    ExpireAt = integer_to_binary(imboy_func:milliseconds() + Millisecond),
    imboy_cipher:aes_encrypt(list_to_binary([Id, ",", ExpireAt])).
