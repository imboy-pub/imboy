-module(user_ds).
%%%
% user 领域服务模块
% user domain service 缩写
%%%

-export([webrtc_credential/1]).
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

%%% demo方法描述
-spec webrtc_credential(Uid :: integer()) -> {binary(), binary()}.
webrtc_credential(Uid) ->
    Uris = config_ds:env(eturnal_uris),
    Secret = config_ds:env(eturnal_secret),

    UidBin = imboy_hashids:uid_encode(Uid),
    TmBin = integer_to_binary(imboy_dt:second() + 86400),
    Username = <<TmBin/binary, ":", UidBin/binary>>,
    Credential = base64:encode(crypto:mac(hmac, sha, Secret, Username)),
    {Username, Credential, Uris}.


auth_webrtc_credential(Username, Credential) ->
    Secret = config_ds:env(eturnal_secret),
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
