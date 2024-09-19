-module(user_ds).
%%%
% user 领域服务模块
% user domain service 缩写
%%%

-export([webrtc_credential/1]).
-export([title/1]).
-export([title/2]).
-export([auth_webrtc_credential/2]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("imlib/include/log.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("imlib/include/common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% user_ds:title(Uid).
-spec title(integer()) -> binary().
title(Uid) ->
    U = user_repo:find_by_id(Uid, <<"account,nickname">>),
    #{<<"account">> := Account, <<"nickname">> := Nickname} = U,
    case {Account, Nickname} of
        {_, <<>>} ->
            Account;
        _ ->
            Nickname
    end.

title(Uid, 2) ->
    U = user_repo:find_by_id(Uid, <<"account,nickname">>),
    #{<<"account">> := Account, <<"nickname">> := Nickname} = U,
    Title = case {Account, Nickname} of
        {_, <<>>} ->
            Account;
        _ ->
            Nickname
    end,
    {Title, Nickname}.

%%% demo方法描述
-spec webrtc_credential(Uid :: integer()) -> {binary(), binary()}.
webrtc_credential(Uid) ->
    Secret = config_ds:get(<<"eturnal_secret">>),
    TurnUrls = config_ds:get(<<"turn_urls">>),
    StunUrls = config_ds:get(<<"stun_urls">>),
    UidBin = imboy_hashids:encode(Uid),
    TmBin = integer_to_binary(imboy_dt:utc(second) + 86400),
    Username = <<TmBin/binary, ":", UidBin/binary>>,
    Credential = base64:encode(crypto:mac(hmac, sha, Secret, Username)),
    #{
        <<"ttl">> => 86400,
        <<"turn_urls">> => TurnUrls,
        <<"stun_urls">> => StunUrls,
        <<"username">> => Username,
        <<"credential">> => Credential
    }.

% user_ds:auth_webrtc_credential(<<"1726238658">>, <<"38NAnjTI0NnBA4xQSm5PiUKd1B4=">>).
auth_webrtc_credential(Username, Credential) ->
    % Secret = config_ds:env(eturnal_secret),
    Secret = config_ds:get(<<"eturnal_secret">>),
    Credential == base64:encode(crypto:mac(hmac, sha, Secret, Username)).


%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
%addr_test_() ->
%    [?_assert(is_public_addr(?PUBLIC_IPV4ADDR)),
%     ?_assert(is_public_addr(?PUBLIC_IPV6ADDR)),
%     ?_test(my_if_addr(inet)),
%     ?_test(my_if_addr(inet6))].
-endif.
