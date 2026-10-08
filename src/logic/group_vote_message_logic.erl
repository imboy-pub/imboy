-module(group_vote_message_logic).
%% 创建后复用 C2G 的持久化、成员权限、禁言与加密策略，禁止直接广播绕过消息闸门。
-export([publish/1]).

publish(#{
    <<"vote_id">> := VoteId,
    <<"group_id">> := Gid,
    <<"creator_id">> := Creator,
    <<"title">> := Title
}) ->
    group_collab_message_logic:publish(
        Gid,
        Creator,
        <<"groupVote">>,
        VoteId,
        #{
            <<"vote_id">> => VoteId,
            <<"group_id">> => integer_to_binary(Gid),
            <<"title">> => Title,
            <<"content">> => Title
        }
    ).
