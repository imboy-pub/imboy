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

%% Types
-type user_id() :: integer().
-type nickname() :: binary().
-type user_title() :: binary().
-type username() :: binary().
-type credential() :: binary().
-type webrtc_info() :: #{
    ttl := non_neg_integer(),
    turn_urls := binary(),
    stun_urls := binary(),
    username := username(),
    credential := credential()
}.
-type title_with_nickname() :: {user_title(), nickname()}.

%% ===================================================================
%% API
%% ===================================================================

%% @doc 获取用户显示名称
%% 根据用户ID获取用户的显示名称。如果用户设置了昵称则返回昵称，
%% 否则返回用户账号。
%% @param Uid 用户ID
%% @returns 用户显示名称（昵称优先，否则返回账号）
-spec title(user_id()) -> user_title().
title(Uid) ->
    U = user_repo:find_by_id(Uid, <<"account,nickname">>),
    #{<<"account">> := Account, <<"nickname">> := Nickname} = U,
    case {Account, Nickname} of
        {_, <<>>} ->
            Account;
        _ ->
            Nickname
    end.

%% @doc 获取用户显示名称和昵称（模式2）
%% 返回用户的显示名称和昵称的组合。显示名称的规则与title/1相同，
%% 但额外返回昵称信息供调用方使用。
%% @param Uid 用户ID
%% @param Mode 模式参数，当前只支持2
%% @returns {显示名称, 昵称}的元组
-spec title(user_id(), 2) -> title_with_nickname().
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

%% @doc 生成WebRTC认证凭据
%% 为指定用户生成WebRTC连接所需的认证信息，包括TURN/STUN服务器配置。
%% 生成的凭据有效期为24小时，使用HMAC-SHA算法进行签名。
%% @param Uid 用户ID
%% @returns 包含WebRTC连接信息的map，包含ttl、服务器地址、用户名和凭据
-spec webrtc_credential(user_id()) -> webrtc_info().
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

%% @doc 验证WebRTC凭据
%% 验证用户提供的WebRTC凭据是否有效。
%% 通过重新计算HMAC并与提供的凭据比较来验证身份。
%%
%% 使用示例：
%% user_ds:auth_webrtc_credential(<<"1728601800:p25vd5">>, <<"B9pddqnbi55R4Mn4JC85Qk1l7T0=">>).
%% @param Username WebRTC用户名（格式：时间戳:编码的用户ID）
%% @param Credential Base64编码的HMAC凭据
%% @returns 验证结果：true表示凭据有效，false表示无效
-spec auth_webrtc_credential(username(), credential()) -> boolean().
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
