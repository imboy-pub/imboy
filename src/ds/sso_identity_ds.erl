-module(sso_identity_ds).

-moduledoc "SSO 身份映射数据服务（薄封装 sso_identity_repo）。".
%%%
% SSO 身份映射数据服务模块（薄封装 sso_identity_repo）
% SSO identity mapping data service (thin wrapper over sso_identity_repo)
%%%

-export([find_uid/2, find_uid_tx/3, bind/4, bind_tx/5]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").
-include("common.hrl").

%% @doc 按 (provider, subject) 查已绑定的 uid
-spec find_uid(binary(), binary()) -> {ok, integer()} | not_found | {error, term()}.
find_uid(Provider, Subject) ->
    to_find_result(sso_identity_repo:find_by_subject(Provider, Subject)).

%% @doc 事务内按 (provider, subject) 查已绑定的 uid（Conn 由 with_tx 提供）
-spec find_uid_tx(any(), binary(), binary()) -> {ok, integer()} | not_found | {error, term()}.
find_uid_tx(Conn, Provider, Subject) ->
    to_find_result(sso_identity_repo:find_by_subject_tx(Conn, Provider, Subject)).

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

%% @doc 事务内绑定身份映射（与建 user 行共用同一连接，保证原子）
-spec bind_tx(any(), binary(), binary(), integer(), binary()) -> ok | {error, term()}.
bind_tx(Conn, Provider, Subject, Uid, Email) ->
    case sso_identity_repo:upsert_tx(Conn, Provider, Subject, Uid, Email) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("sso_identity_ds:bind_tx provider=~ts error ~p", [Provider, Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

%% repo 返回 {ok, Rows}；空行集与「有行无 uid」都归一为 not_found，
%% 让调用方的匹配式在 pool / tx 两条路径上完全一致。
%% 注意：此处刻意不加 is_integer 守卫 —— find_uid/2 是既有共享函数，
%% 保持其返回值形态与重构前逐字节一致（uid 类型守卫由调用方负责）。
-spec to_find_result({ok, [map()]} | {error, term()}) ->
    {ok, integer()} | not_found | {error, term()}.
to_find_result({ok, [#{<<"uid">> := Uid} | _]}) ->
    {ok, Uid};
to_find_result({ok, _}) ->
    not_found;
to_find_result({error, Reason}) ->
    {error, Reason}.
