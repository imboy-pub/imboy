-module(enterprise_friend_request_repo).

-moduledoc "企业好友申请（只发起，EPGZ-03 INT-11）仓储层。".
%%%
% enterprise_friend_request_repo 是 EPGZ-03（INT-11）好友申请（只发起）的
% tx 仓储层。现有好友申请的持久化真源是 user_friend 表的 pending 行
% （status=0，uk_fromuid_touid 唯一），人工审批流（accept/reject）由
% friend_logic:confirm_friend / reject_friend 消费同一真源，零改动。
%
% 本模块把 friend_ds / friend_repo 中池化（无 Conn）的三个读/写做成
% 事务内形态，供 logic 在 A2 幂等 begin/complete 事务中组合：
%   * pending_status_tx   镜像 friend_ds:pending_status/2 +
%                         friend_ds:check_relationship/2（去缓存、单 SQL）；
%   * insert_pending_tx   镜像 friend_repo:insert_pending/4（ON CONFLICT
%                         DO NOTHING 幂等，setting 列过滤同源）。
%%%

-export([
    next_friend_id/0,
    pending_status_tx/3,
    insert_pending_tx/5
]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc user_friend TSID（friend 命名空间；惰性注册，镜像 A1 口径）。
-spec next_friend_id() -> pos_integer().
next_friend_id() ->
    ensure_tsid(friend),
    elib_tsid:generate(friend).

%% @doc 事务内派生 from→to 申请状态：blocked > friends > pending > none
%% （语义逐字对齐 friend_ds:pending_status/2；黑名单/好友/申请三查合一，
%% 镜像 friend_ds:check_relationship 的 SQL，去 memo 缓存保证同事务可见）。
-spec pending_status_tx(any(), integer(), integer()) ->
    none | pending | friends | blocked | {error, term()}.
pending_status_tx(Conn, FromUid, ToUid) when
    is_integer(FromUid), is_integer(ToUid)
->
    UserDTable = elib_pg_sql:public_tablename(<<"user_denylist">>),
    Sql =
        <<"SELECT ", "EXISTS(SELECT 1 FROM user_friend ",
            " WHERE ((from_user_id = $1 AND to_user_id = $2 AND status = 1) OR ",
            "(from_user_id = $2 AND to_user_id = $1 AND status = 1))) AS is_friend, ",
            "EXISTS(SELECT 1 FROM ", UserDTable/binary,
            " WHERE user_id = $1 AND denied_user_id = $2) AS in_denylist, ",
            "EXISTS(SELECT 1 FROM user_friend ",
            " WHERE from_user_id = $1 AND to_user_id = $2 AND status = 0) AS is_pending">>,
    case elib_pg:query(Conn, Sql, [FromUid, ToUid]) of
        {ok, [
            #{
                <<"in_denylist">> := InDenylist,
                <<"is_friend">> := IsFriend,
                <<"is_pending">> := IsPending
            }
        ]} ->
            if
                InDenylist ->
                    blocked;
                IsFriend ->
                    friends;
                IsPending ->
                    pending;
                true ->
                    none
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内插入 pending 申请行（status=0，单向 from→to）。
%% 已存在 (from,to) 行（任意 status）则 DO NOTHING（幂等，uk_fromuid_touid），
%% 返回 {ok, existing}；新插入返回 {ok, inserted}。
%% setting 列与现有流程同构：存申请 payload 的 from/msg 等安全子集。
-spec insert_pending_tx(any(), integer(), integer(), map(), binary()) ->
    {ok, inserted | existing} | {error, term()}.
insert_pending_tx(Conn, FromUid, ToUid, Setting, NowTs) when
    is_integer(FromUid), is_integer(ToUid), is_map(Setting), is_binary(NowTs)
->
    Tb = elib_pg_sql:public_tablename(<<"user_friend">>),
    Id = next_friend_id(),
    SettingJson = jsone:encode(Setting, [native_utf8]),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, from_user_id, to_user_id, status, category_id, setting, created_at)",
            " VALUES ($1, $2, $3, 0, 0, $4, $5)",
            " ON CONFLICT (from_user_id, to_user_id) DO NOTHING", " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [Id, FromUid, ToUid, SettingJson, NowTs]) of
        {ok, [_ | _]} ->
            {ok, inserted};
        {ok, []} ->
            {ok, existing};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec ensure_tsid(atom()) -> ok.
ensure_tsid(Name) ->
    case lists:member(Name, elib_tsid:registered()) of
        true ->
            ok;
        false ->
            elib_tsid:register(Name)
    end.
