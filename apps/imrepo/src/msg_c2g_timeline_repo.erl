-module(msg_c2g_timeline_repo).
%%%
% msg_c2g_timeline_repo 是 msg_c2g_timeline repository 缩写
%%%

-include_lib("imlib/include/log.hrl").

-export([tablename/0]).
-export([client_ack/2]).
-export([delete_timeline/2]).
-export([list_by_uid/2, list_by_uid/3]).
-export([check_msg/1]).
-export([count_by_to_id/1]).
-export([delete_overflow_timeline/2]).

%% ===================================================================
%% API
%% ===================================================================


tablename() ->
    imboy_db:public_tablename(<<"msg_c2g_timeline">>).


% msg_c2g_timeline_repo:list_by_uid(2, <<"msg_id">>, 10).
list_by_uid(Uid, Column) ->
    list_by_uid(Uid, Column, 10000000).


list_by_uid(Uid, Column, Limit) ->
    Tb = tablename(),
    % use index idx_c2g_timeline_ToUid_ClientAck
    Where = <<" WHERE to_uid = $1 AND client_ack = 0 LIMIT $2">>,
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, Where/binary>>,
    imboy_db:query(Sql, [Uid, Limit]).

% msg_c2g_timeline_repo:client_ack(109, <<"cor1aup1a20rgjtl5t8g">>).
client_ack(ToUid, MsgId) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    Where = <<"to_uid = ", (ec_cnv:to_binary(ToUid))/binary," AND  msg_id = '", MsgId/binary, "'">>,
    imboy_db:update(Tb, Where, #{client_ack => 1}).

% msg_c2g_timeline_repo:delete_timeline(6).
delete_timeline(ToUid, MsgId) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    Where = <<" WHERE to_uid = $1 AND  msg_id = $2">>,
    Sql = <<"DELETE FROM ", Tb/binary, Where/binary>>,
    imboy_db:execute(Sql, [ToUid, MsgId]).


% msg_c2g_timeline_repo:check_msg(1).
check_msg(MsgId) ->
    % use index uk_c2g_timeline_MsgId
    imboy_db:pluck(tablename(), <<"msg_id = ", MsgId/binary>>, <<"count(*) as count">>, 0).


% msg_c2g_timeline_repo:count_by_to_id(1).
count_by_to_id(ToUid) ->
    ToUid2 = integer_to_binary(ToUid),
    % use index uk_c2g_timeline_ToUid_MsgId
    imboy_db:pluck(tablename(), <<"to_uid = ", ToUid2/binary>>, <<"count(*) as count">>, 0).


% msg_c2g_timeline_repo:delete_overflow_timeline(1, 100).
delete_overflow_timeline(ToUid, Limit) ->
    Tb = tablename(),
    % use index uk_c2g_timeline_ToUid_MsgId
    Where = <<" WHERE to_uid = $1 ORDER BY created_at ASC LIMIT $2">>,
    Sql = <<"SELECT msg_id FROM ", Tb/binary, Where/binary>>,
    % ?LOG(Sql),
    case imboy_db:query(Sql, [ToUid, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            [ delete_timeline(ToUid, MsgId) || {MsgId} <- Rows ],
            {msg_ids, [ MsgId || {MsgId} <- Rows ]}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================
