%%%-------------------------------------------------------------------
%%% @doc qr_login_session 会话状态机（waiting→scanned→confirmed）数据服务，
%%%      与 qr_login_event_ds 平行（event=事件流，session=状态机）。
%%%      薄转发 qr_login_session_repo，无缓存无编排（分层门：API 禁直调 Repo）。
%%% @end
%%%-------------------------------------------------------------------
-module(qr_login_session_ds).

-export([create/1]).
-export([find_by_session_token/1, find_by_qr_token/1]).
-export([transition_scan/2, transition_confirm/3, transition_cancel/1]).
-export([claim_login_token/1]).

%% @doc 创建登录会话（waiting 初始态）
-spec create(map()) -> ok | {error, term()}.
create(Params) ->
    qr_login_session_repo:create(Params).

-spec find_by_session_token(binary()) -> map() | undefined | {error, term()}.
find_by_session_token(SessionToken) ->
    qr_login_session_repo:find_by_session_token(SessionToken).

-spec find_by_qr_token(binary()) -> map() | undefined | {error, term()}.
find_by_qr_token(QRToken) ->
    qr_login_session_repo:find_by_qr_token(QRToken).

%% @doc scan CAS：waiting → scanned
-spec transition_scan(binary(), integer()) -> {ok, map()} | {error, not_transitioned}.
transition_scan(SessionToken, Uid) ->
    qr_login_session_repo:transition_scan(SessionToken, Uid).

%% @doc confirm CAS：scanned → confirmed + 原子写入 login_token
-spec transition_confirm(binary(), integer(), binary()) ->
    {ok, map()} | {error, not_transitioned}.
transition_confirm(SessionToken, Uid, LoginToken) ->
    qr_login_session_repo:transition_confirm(SessionToken, Uid, LoginToken).

%% @doc cancel CAS：waiting|scanned → cancelled（幂等）
-spec transition_cancel(binary()) -> ok | {error, not_transitioned}.
transition_cancel(SessionToken) ->
    qr_login_session_repo:transition_cancel(SessionToken).

%% @doc 原子领取 login_token（恰好一个消费者）
-spec claim_login_token(binary()) ->
    {ok, #{login_token := binary(), scanned_by := integer()}} | {error, not_claimable}.
claim_login_token(SessionToken) ->
    qr_login_session_repo:claim_login_token(SessionToken).
