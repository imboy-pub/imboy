-module(agent_task_ds).

%%%
% Agent Task 领域服务（DATA-01）：授权判据 + 事务组合。
% 授权以服务端权威数据为准（group_ds 现查成员列表），绝不信任事件/请求携带的
% 成员列表；审批人不得是任务所属 agent（防自我审批架空人工闸门）。
% 仲裁事务：CAS 状态迁移 + 决定插入同事务提交，DB 故障即整体回滚（fail-closed）。
%%%

-export([is_group_member/2, is_authorized_approver/3]).
-export([with_tx/1]).

%% @doc 群成员判定（服务端权威数据，现查 group_ds，不信任事件携带列表）。
%% 故障时异常向上传播，由调用方容错（审批 fail-closed）。
-spec is_group_member(integer(), integer()) -> boolean().
is_group_member(Uid, GroupId) ->
    lists:member(Uid, group_ds:member_uids(GroupId)).

%% @doc 审批授权：群成员 且 非任务所属 agent 本人。
%% 注意：group_ds 故障时异常向上传播（logic decide 容错为 internal_error）——
%% 审批在授权数据不可用时 fail-closed 拒绝，绝不放行。
-spec is_authorized_approver(integer(), integer(), integer()) -> boolean().
is_authorized_approver(ApproverUid, GroupId, AgentUid) ->
    ApproverUid =/= AgentUid andalso is_group_member(ApproverUid, GroupId).

%% @doc 事务包装：elib_pg:with_tx 直接返回 fun 的值 R；异常/回滚归一为
%% {error, Reason}（调用方按 error 容错，fail-closed）。
-spec with_tx(fun((any()) -> term())) -> term().
with_tx(Fun) ->
    case elib_pg:with_tx(Fun) of
        {rollback, Reason} -> {error, Reason};
        R -> R
    end.
