-module(group_vote_ds).

%%%
% group_vote_ds — 群投票数据服务层（2026-04 G2-a + G3 治理）
%
% 封装 group_vote_repo（投票主表 / 选项子表 / 记录子表）对 Logic 层的访问。
% Pass-through wrapper over group_vote_repo (vote / option / record tables),
% enforcing Handler→Logic→DS→Repo boundary.
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

insert_vote(Data) -> group_vote_repo:insert_vote(Data).

update_vote_status(VoteId, Status) ->
    group_vote_repo:update_vote_status(VoteId, Status).

list_votes_by_group_id(Gid, Page, Size) ->
    group_vote_repo:list_votes_by_group_id(Gid, Page, Size).

count_votes_by_group_id(Gid) -> group_vote_repo:count_votes_by_group_id(Gid).

%% ===================================================================
%% options
%% ===================================================================

insert_options_batch(Options) -> group_vote_repo:insert_options_batch(Options).

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

insert_record(Data) -> group_vote_repo:insert_record(Data).

update_record(RecordId, Data) -> group_vote_repo:update_record(RecordId, Data).

delete_record(RecordId) -> group_vote_repo:delete_record(RecordId).

find_record_by_vote_and_user(VoteId, UserId) ->
    group_vote_repo:find_record_by_vote_and_user(VoteId, UserId).
