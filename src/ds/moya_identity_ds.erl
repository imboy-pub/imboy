-module(moya_identity_ds).
%%%
% 墨芽小程序「首登自动开户」事务编排层（2026-09-20 试点方案 B）
% WeChat mini-program first-login auto-provisioning (atomic user + sso_identity)
%
% 为什么需要这个模块：
%   方案 A（老师邀请码）之前，家长首次扫码进小程序时，服务端手里**只有
%   openid**，没有任何可用来写 sso_identity 的既有 uid —— 而 openid 又
%   永远拿不到（jscode2session 的服务端不落 openid，log_redact 也把 openid
%   列为脱敏键），机构侧因此**不存在**可执行的人工绑定入口。唯一可行解是
%   在拿到 openid 的这一次请求里一并把账号开出来。
%
% 设计要点：
%   - provision_and_bind/3 生产入口：先分配 account，再 elib_pg:with_tx 包
%     provision_and_bind_in_tx/4。任一失败 → throw({rollback, _}) → 整事务
%     回滚，不留「有 user 行却没有身份映射」的孤儿账号。
%   - ⚠ 事务契约：elib_pg:with_tx **原样透传** fun 的返回值，返回 {error, _}
%     会照常 COMMIT。因此本模块一律用 throw({rollback, Reason}) 触发真回滚
%     （与 user_logic:apply_logout/2、钱路径同款）；切勿改成返回 {error, _}。
%   - provision_and_bind_in_tx/4 导出给真库集成测试直调（与生产同代码路径）。
%   - 并发：pg_advisory_xact_lock(hashtext(provider:subject)) 让同一 openid 的
%     并发首登串行化，锁内复查映射 → 后来者复用已存在的 uid，不会重复建号。
%   - openid 只作为 subject 写入 sso_identity，绝不出现在日志/返回值/错误里。
%%%

-export([provision_and_bind/3, provision_and_bind_in_tx/4]).

-include("log.hrl").
-include_lib("kernel/include/logger.hrl").
-include("common.hrl").

%% 来源标记：运营侧据此区分「微信小程序自动开户的家长」与普通 IM 用户。
%% 与 auth_oidc_logic 写 <<"oidc">> 同一惯例。
-define(SOURCE, <<"moya_wechat_mini">>).
%% 家长账号在复审通过前的默认昵称（可空，仅为列表可读性）
-define(DEFAULT_NICKNAME, <<"微信家长"/utf8>>).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc 首登自动开户：为 (Provider, Subject) 建 user 行并绑定 sso_identity。
%% Opts：#{ip => binary(), nickname => binary()}（均可缺省）
%% 返回 {ok, Uid} | {error, account_unavailable | db_error}
%%
%% 幂等：若映射已存在则直接返回既有 uid，**不建号、不消耗 account 号段**。
-spec provision_and_bind(binary(), binary(), map()) -> {ok, integer()} | {error, atom()}.
provision_and_bind(Provider, Subject, Opts) ->
    %% account 号在事务外先取：account_ds:allocate/0 走缓存 + 自己的池连接，
    %% 放在事务里会变成「事务持有一连接的同时再借一连接」，池饿死时自锁。
    case allocate_account() of
        {ok, Account} ->
            Tx = fun(Conn) ->
                provision_and_bind_in_tx(Conn, Provider, Subject, Opts#{account => Account})
            end,
            case elib_pg:with_tx(Tx) of
                {ok, Uid} when is_integer(Uid) ->
                    {ok, Uid};
                {rollback, Reason} ->
                    ?LOG_ERROR("moya_identity_ds provision rollback ~p", [Reason]),
                    {error, reason_atom(Reason)};
                {error, Reason} ->
                    ?LOG_ERROR("moya_identity_ds provision db error ~p", [Reason]),
                    {error, db_error}
            end;
        {error, _Why} ->
            ?LOG_ERROR("moya_identity_ds account allocate failed"),
            {error, account_unavailable}
    end.

%% @doc 事务体（集成测试直调；业务失败一律 throw({rollback, Reason})）
-spec provision_and_bind_in_tx(any(), binary(), binary(), map()) -> {ok, integer()}.
provision_and_bind_in_tx(Conn, Provider, Subject, Opts) ->
    %% 1. 同一 (provider, subject) 的事务级 advisory 锁：并发首登串行化。
    %%    与 trust_audit_repo / adm_setup_logic 同款用法。
    lock_subject(Conn, Provider, Subject),
    %% 2. 锁内复查：持锁后若有映射（并发的第一次已提交），直接复用，不重复建号。
    case sso_identity_ds:find_uid_tx(Conn, Provider, Subject) of
        {ok, Uid} when is_integer(Uid) ->
            {ok, Uid};
        not_found ->
            create_user_and_bind(Conn, Provider, Subject, Opts);
        {error, Reason} ->
            ?LOG_ERROR("moya_identity_ds find_uid_tx failed ~p", [Reason]),
            throw({rollback, db_error})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc 事务级 advisory 锁 key 由 (provider, subject) 派生。
%% 只用于串行化、不入表、不落日志（Subject 是 openid，禁止外泄）。
-spec lock_subject(any(), binary(), binary()) -> ok.
lock_subject(Conn, Provider, Subject) ->
    LockKey = <<Provider/binary, ":", Subject/binary>>,
    case elib_pg:query(Conn, <<"SELECT pg_advisory_xact_lock(hashtext($1))">>, [LockKey]) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("moya_identity_ds advisory lock failed ~p", [Reason]),
            throw({rollback, db_error})
    end.

-spec create_user_and_bind(any(), binary(), binary(), map()) -> {ok, integer()}.
create_user_and_bind(Conn, Provider, Subject, Opts) ->
    Data = #{
        %% 这些键必须是 **atom**：user_repo:normalize_legacy_create_data/1 产出的
        %% 是固定 atom 键集，同名字段的 binary 键会与之并存 → 拼出重复列 → PG 42701。
        account => maps:get(account, Opts),
        nickname => maps:get(nickname, Opts, ?DEFAULT_NICKNAME),
        %% SSO 用户不走密码登录：置强随机密码，绝不复用
        %% normalize 的 "password123" 兜底（那在生产是不设防的口令）
        password => elib_password:generate(binary:encode_hex(crypto:strong_rand_bytes(32))),
        reg_ip => maps:get(ip, Opts, <<>>),
        source => ?SOURCE
    },
    case user_repo:create_tx(Conn, Data) of
        {ok, Uid} when is_integer(Uid), Uid > 0 ->
            bind_identity(Conn, Provider, Subject, Uid);
        {error, {error, error, <<"23505">>, unique_violation, _Msg, _Detail}} ->
            %% account 号撞车（号段缓存并发/外部占用）：换号重试需要新号，
            %% 交由上层折叠为 5401 —— 家长再点一次登录即可拿到新号，不会半途留脏数据
            throw({rollback, account_conflict});
        {error, Reason} ->
            ?LOG_ERROR("moya_identity_ds create user failed ~p", [Reason]),
            throw({rollback, db_error})
    end.

-spec bind_identity(any(), binary(), binary(), integer()) -> {ok, integer()}.
bind_identity(Conn, Provider, Subject, Uid) ->
    %% email 传空串：小程序登录拿不到邮箱，不占位不猜测
    case sso_identity_ds:bind_tx(Conn, Provider, Subject, Uid, <<>>) of
        ok ->
            %% 成功路径只记 uid 与来源，绝不记 subject（openid）
            ?LOG_INFO("[moya_identity] auto_provisioned uid=~p source=~ts", [Uid, ?SOURCE]),
            {ok, Uid};
        {error, Reason} ->
            ?LOG_ERROR("moya_identity_ds bind_tx failed ~p", [Reason]),
            throw({rollback, db_error})
    end.

-spec allocate_account() -> {ok, non_neg_integer()} | {error, term()}.
allocate_account() ->
    case account_ds:allocate() of
        N when is_integer(N), N > 0 -> {ok, N};
        Other -> {error, Other}
    end.

%% rollback 原因折叠为对外的有限集合：不把内部 term 透给 HTTP 层
-spec reason_atom(term()) -> account_unavailable | db_error.
reason_atom(account_conflict) -> account_unavailable;
reason_atom(_) -> db_error.
