-module(sso_identity_ds).

%%%
% SSO 身份映射数据服务模块（薄封装 sso_identity_repo）
% SSO identity mapping data service (thin wrapper over sso_identity_repo)
%%%

-export([find_uid/2, bind/4]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("common.hrl").

%% @doc 按 (provider, subject) 查已绑定的 uid
-spec find_uid(binary(), binary()) -> {ok, integer()} | not_found | {error, term()}.
find_uid(Provider, Subject) ->
    case sso_identity_repo:find_by_subject(Provider, Subject) of
        {ok, [#{<<"uid">> := Uid} | _]} ->
            {ok, Uid};
        {ok, _} ->
            not_found;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 绑定/更新身份映射（幂等 upsert）
-spec bind(binary(), binary(), integer(), binary()) -> ok | {error, term()}.
bind(Provider, Subject, Uid, Email) ->
    case sso_identity_repo:upsert(Provider, Subject, Uid, Email) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("sso_identity_ds:bind provider=~ts error ~p", [Provider, Reason]),
            {error, Reason}
    end.
