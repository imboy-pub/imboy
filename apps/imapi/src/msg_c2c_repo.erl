-module(msg_c2c_repo).
%%%
% msg_c2c_repo 是 msg_c2c repository 缩写
%%%

-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([read_msg/4]).
-export([write_msg/6]).
-export([delete_msg/1]).
-export([count_by_to_id/1]).
-export([delete_overflow_msg/2]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"msg_c2c">>).


read_msg(Where, Vals, Column, Limit) ->
    Tb = tablename(),
    ValsLen = length(Vals),
    LimitIndex = integer_to_binary(ValsLen + 1),
    % use index i_ToId
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " ", Where/binary, " ORDER BY id ASC LIMIT $",
            LimitIndex/binary>>,
    % logger:error("msg_c2c_repo:read_msg/4 ~s~n", [Sql]),
    imboy_db:query(Sql, Vals ++ [Limit]).


% msg_c2c_repo:write_msg(imboy_dt:millisecond(), <<"ciik13p2888j8hhi437g">>, <<"{\"msg_type\":\"text\",\"text\":\"ddd的点点滴滴\"},\"created_at\":1688551567306}">>, 1, 2, imboy_dt:millisecond()).
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(CreatedAt, Id, Payload, FromId2, ToId, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) when is_integer(ToId) ->
    ToId2 = list_to_binary(integer_to_list(ToId)),
    write_msg(CreatedAt, Id, Payload, FromId, ToId2, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) ->
    % ?LOG([CreatedAt, Id, Payload, FromId, ToId, ServerTS]),
    Tb = tablename(),
    Column = <<"(payload, from_id, to_id,
        created_at, server_ts, msg_id)">>,
    CreatedAt2 = integer_to_binary(CreatedAt),
    ServerTS2 = integer_to_binary(ServerTS),
    Payload2 = imboy_hasher:encoded_val(Payload),
    Value = <<"(", Payload2/binary, ", '", FromId/binary, "', '", ToId/binary, "', '", CreatedAt2/binary, "', '",
              ServerTS2/binary, "', '", Id/binary, "')">>,
    imboy_db:insert_into(Tb, Column, Value).


delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE id = $1">>,
    delete_msg(Where, Id);
delete_msg(Id) ->
    % use index uk_c2c_MsgId
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, Id).


delete_msg(Where, Val) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    imboy_db:execute(Sql, [Val]).


% msg_c2c_repo:count_by_to_id(1).
count_by_to_id(ToUid) ->
    ToUid2 = integer_to_binary(ToUid),
    % use index i_c2c_ToId
    imboy_db:pluck(tablename(), <<"to_id = ", ToUid2/binary>>, <<"count(*) as count">>, 0).


delete_overflow_msg(ToUid, Limit) ->
    Tb = tablename(),
    Where = <<" WHERE to_id = $1 ORDER BY id ASC LIMIT $2">>,
    Sql = <<"SELECT id FROM ", Tb/binary, Where/binary>>,
    case imboy_db:query(Sql, [ToUid, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            [ delete_msg(Id) || {Id} <- Rows ],
            ok
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
