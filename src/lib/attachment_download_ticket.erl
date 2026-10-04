-module(attachment_download_ticket).
-moduledoc "附件下载票据 —— 下载能力使用 purpose-derived key，绝不复用 Human 访问令牌。".
-export([issue/3, verify/2]).

%% Download capabilities use a purpose-derived key, never a Human access token.
-define(TTL, 600).

issue(Uid, ObjectKey, Secret) when
    is_integer(Uid),
    Uid > 0,
    is_binary(ObjectKey),
    byte_size(ObjectKey) > 0,
    byte_size(ObjectKey) =< 2048,
    is_binary(Secret),
    byte_size(Secret) > 0
->
    Now = erlang:system_time(second),
    {ok,
        imboy_jwt:sign(
            #{
                <<"sub">> => <<"attachment_download">>,
                <<"uid">> => Uid,
                <<"object_key">> => ObjectKey,
                <<"iat">> => Now,
                <<"exp">> => Now + ?TTL
            },
            key(Secret)
        )};
issue(_, _, _) ->
    {error, invalid}.

verify(Ticket, Secret) when
    is_binary(Ticket),
    byte_size(Ticket) =< 8192,
    is_binary(Secret),
    byte_size(Secret) > 0
->
    case imboy_jwt:verify(Ticket, key(Secret)) of
        {ok, #{
            <<"sub">> := <<"attachment_download">>,
            <<"uid">> := Uid,
            <<"object_key">> := ObjectKey,
            <<"iat">> := Issued,
            <<"exp">> := Expires
        }} when
            is_integer(Uid),
            Uid > 0,
            is_binary(ObjectKey),
            byte_size(ObjectKey) > 0,
            byte_size(ObjectKey) =< 2048,
            is_integer(Issued),
            is_integer(Expires),
            Expires > Issued,
            Expires =< Issued + ?TTL
        ->
            {ok, Uid, ObjectKey};
        _ ->
            {error, invalid}
    end;
verify(_, _) ->
    {error, invalid}.

key(Secret) ->
    crypto:mac(hmac, sha256, Secret, <<"imboy:attachment-download:v1">>).
