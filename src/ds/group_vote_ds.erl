-module(group_vote_ds).

%%%
% group_vote_ds — 群投票数据服务层（2026-04 G2-a + G3 治理）
%
% 封装 group_vote_repo（投票主表 / 选项子表 / 记录子表）对 Logic 层的访问。
% Pass-through wrapper over group_vote_repo (vote / option / record tables),
% enforcing Handler→Logic→DS→Repo boundary.
%
% T7 归档写守卫（P0 后续批）：全部内容写路径（建投票/批量选项/投票/改票/
% 撤票/关闭）经 workspace_guard:write_tx 与守卫同事务提交
% （{group,Gid} / {group_vote,VoteId} / {group_vote_record,RecordId}
% → workspace 行锁），归档后拒绝（稳定错误码 980）；查询路径不加守卫。
%%%

-export([
    find_by_vote_id/1
]).

%% G3：投票主表 pass-through
-export([
    insert_vote/1,
    update_vote_status/2,
    list_votes_by_group_id/3,
    count_votes_by_group_id/1
]).

%% G3：选项子表 pass-through
-export([
    insert_options_batch/1,
    list_options_by_vote_id/1,
    count_votes_by_option_id/1,
    count_total_votes_by_vote_id/1,
    count_votes_grouped_by_vote_id/1
]).

%% G3：记录子表 pass-through
-export([
    insert_record/1,
    update_record/2,
    delete_record/1,
    find_record_by_vote_and_user/2
]).

%% @doc 按对外 vote_id 查投票
find_by_vote_id(VoteId) -> group_vote_repo:find_by_vote_id(VoteId).

%% ===================================================================
%% vote main table
%% ===================================================================

insert_vote(Data) ->
    %% T7 归档写守卫：{group, Gid} 行锁与写入同事务（Data 自带 group_id）；
    %% group_id 缺失/非法时走 repo 自动提交版保留必填字段校验错误契约。
    case maps:get(group_id, Data, undefined) of
        Gid when is_integer(Gid), Gid > 0 ->
            workspace_guard:write_tx({group, Gid}, fun(Conn) ->
                group_vote_repo:insert_vote_tx(Conn, Data)
            end);
        _ ->
            group_vote_repo:insert_vote(Data)
    end.

update_vote_status(VoteId, Status) ->
    %% T7 归档写守卫：{group_vote, VoteId}（vote_id → group → workspace）；
    %% 用户 close_vote 与 adm 治理 close 共用本入口。
    workspace_guard:write_tx({group_vote, VoteId}, fun(Conn) ->
        group_vote_repo:update_vote_status_tx(Conn, VoteId, Status)
    end).

list_votes_by_group_id(Gid, Page, Size) ->
    group_vote_repo:list_votes_by_group_id(Gid, Page, Size).

count_votes_by_group_id(Gid) -> group_vote_repo:count_votes_by_group_id(Gid).

%% ===================================================================
%% options
%% ===================================================================

insert_options_batch([]) ->
    {error, invalid_param};
insert_options_batch(Options) ->
    %% T7 归档写守卫：整批选项与守卫同事务（vote_id → group → workspace）
    VoteId = maps:get(vote_id, hd(Options), undefined),
    workspace_guard:write_tx({group_vote, VoteId}, fun(Conn) ->
        group_vote_repo:insert_options_batch_tx(Conn, Options)
    end).

list_options_by_vote_id(VoteId) -> group_vote_repo:list_options_by_vote_id(VoteId).

count_votes_by_option_id(OptionId) ->
    group_vote_repo:count_votes_by_option_id(OptionId).

count_total_votes_by_vote_id(VoteId) ->
    group_vote_repo:count_total_votes_by_vote_id(VoteId).

count_votes_grouped_by_vote_id(VoteId) ->
    group_vote_repo:count_votes_grouped_by_vote_id(VoteId).

%% ===================================================================
%% records
%% ===================================================================

insert_record(Data) ->
    %% T7 归档写守卫：{group_vote, VoteId}（Data 自带 vote_id）；
    %% vote_id 缺失/非法时走 repo 自动提交版保留必填字段校验错误契约。
    case maps:get(vote_id, Data, undefined) of
        VoteId when is_binary(VoteId), VoteId =/= <<>> ->
            workspace_guard:write_tx({group_vote, VoteId}, fun(Conn) ->
                group_vote_repo:insert_record_tx(Conn, Data)
            end);
        _ ->
            group_vote_repo:insert_record(Data)
    end.

update_record(RecordId, Data) ->
    %% T7 归档写守卫：{group_vote_record, RecordId}（记录 → 投票 → 群）
    workspace_guard:write_tx({group_vote_record, RecordId}, fun(Conn) ->
        group_vote_repo:update_record_tx(Conn, RecordId, Data)
    end).

delete_record(RecordId) ->
    %% T7 归档写守卫：{group_vote_record, RecordId}
    workspace_guard:write_tx({group_vote_record, RecordId}, fun(Conn) ->
        group_vote_repo:delete_record_tx(Conn, RecordId)
    end).

find_record_by_vote_and_user(VoteId, UserId) ->
    group_vote_repo:find_record_by_vote_and_user(VoteId, UserId).
