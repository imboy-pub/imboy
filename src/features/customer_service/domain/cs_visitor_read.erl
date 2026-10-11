%%% 逐消息阅读事实只接受明确可见的正 TSID 字符串集合。
-module(cs_visitor_read).
-export([message_ids/1]).
message_ids(Ids) when is_list(Ids), length(Ids) >= 1, length(Ids) =< 200 ->
    case lists:all(fun valid_id/1, Ids) of
        true -> {ok, lists:usort([binary_to_integer(Id) || Id <- Ids])};
        false -> {error, {invalid_argument, message_ids}}
    end;
message_ids(_) ->
    {error, {invalid_argument, message_ids}}.
valid_id(Id) when is_binary(Id), byte_size(Id) >= 1, byte_size(Id) =< 19 ->
    case re:run(Id, <<"^[1-9][0-9]*$">>, [{capture, none}]) of
        match -> binary_to_integer(Id) =< 9223372036854775807;
        nomatch -> false
    end;
valid_id(_) ->
    false.
