-module(msg_c2c_repo).
%%%
% msg_c2c_repo 是 msg_c2c repository 缩写
%%%

-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([read_msg/3]).
-export([write_msg/6]).
-export([delete_msg/1]).
-export([delete_msg/2]).
-export([count_by_to_id/1]).
-export([delete_overflow_msg/2]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"msg_c2c">>).


read_msg(Where, Column, Limit) ->
    Tb = tablename(),
    % use index i_ToId
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " "
        , Where/binary
        , " ORDER BY id ASC LIMIT "
        , (ec_cnv:to_binary(Limit))/binary>>,
    % logger:error("msg_c2c_repo:read_msg/3 ~s~n", [Sql]),
    imboy_db:query(Sql).


% msg_c2c_repo:write_msg(imboy_dt:now(), <<"ciik13p2888j8hhi437g">>, <<"{\"msg_type\":\"text\",\"text\":\"ddd的点点滴滴\"},\"created_at\":1688551567306}">>, 1, 2, imboy_dt:now()).
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) when is_integer(FromId) ->
    FromId2 = list_to_binary(integer_to_list(FromId)),
    write_msg(CreatedAt, Id, Payload, FromId2, ToId, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) when is_integer(ToId) ->
    ToId2 = list_to_binary(integer_to_list(ToId)),
    write_msg(CreatedAt, Id, Payload, FromId, ToId2, ServerTS);
write_msg(CreatedAt, Id, Payload, FromId, ToId, ServerTS) ->
    % ?LOG([CreatedAt, Id, Payload, FromId, ToId, ServerTS]),
    Tb = tablename(),
    imboy_db:insert_into(Tb, #{
        payload => {raw, imboy_hasher:encoded_val(Payload)},
        from_id => FromId,
        to_id => ToId,
        created_at => CreatedAt,
        server_ts => ServerTS,
        msg_id => Id
        }).


delete_msg(Id) when is_integer(Id) ->
    Where = <<"WHERE id = $1">>,
    delete_msg(Where, [Id]);
delete_msg(Id) ->
    % use index uk_c2c_MsgId
    Where = <<"WHERE msg_id = $1">>,
    delete_msg(Where, [Id]).


delete_msg(Where, Params) when is_list(Params) ->
    Tb = tablename(),
    Sql = <<"DELETE FROM ", Tb/binary, " ", Where/binary>>,
    ?LOG(['delete_msg', Params, Sql]),
    imboy_db:execute(Sql, Params).


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
