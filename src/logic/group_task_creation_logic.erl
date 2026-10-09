-module(group_task_creation_logic).
-export([finish/5]).

%% undefined 表示未指定执行人；显式空列表不扩大分配范围。
-spec finish(integer(), integer(), integer(), binary(), undefined | [integer()]) ->
    {ok, map()} | {error, binary(), integer()}.
finish(GroupId, CreatorId, TaskId, Title, Assignees0) ->
    Assignees =
        case Assignees0 of
            undefined -> group_ds:member_uids(GroupId);
            _ -> Assignees0
        end,
    case assign(TaskId, Assignees, CreatorId) of
        ok -> publish(GroupId, CreatorId, TaskId, Title);
        Error -> Error
    end.

assign(_TaskId, [], _CreatorId) -> ok;
assign(TaskId, Assignees, CreatorId) -> group_task_logic:assign(TaskId, Assignees, CreatorId).

publish(GroupId, CreatorId, TaskId, Title) ->
    Result = #{<<"task_id">> => TaskId},
    Payload = #{
        <<"id">> => integer_to_binary(TaskId),
        <<"group_id">> => integer_to_binary(GroupId),
        <<"title">> => Title,
        <<"content">> => Title
    },
    case
        group_collab_message_logic:publish(
            GroupId, CreatorId, <<"groupTask">>, integer_to_binary(TaskId), Payload
        )
    of
        {ok, Message} -> {ok, Result#{<<"chat_message">> => Message}};
        {error, _} -> {ok, Result#{<<"message_status">> => <<"failed">>}}
    end.
