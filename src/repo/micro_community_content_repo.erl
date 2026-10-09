-module(micro_community_content_repo).
-moduledoc "Scoped channel content persistence; all external values are SQL parameters.".
-export([context_tx/3, message_tx/3, search_tx/4, essence_tx/4, tags_tx/4]).

context_tx(Conn, Uid, ChannelId) ->
    case
        rows(
            Conn,
            <<
                "SELECT id,creator_uid,status,scope,workspace_id,visibility,access_type "
                "FROM channel WHERE id=$1 FOR UPDATE"
            >>,
            [ChannelId]
        )
    of
        [] ->
            {error, not_found};
        [Channel] ->
            Role = role_tx(Conn, Uid, Channel),
            Workspace = workspace_access_tx(Conn, Uid, Channel),
            Subscribed = rows(
                Conn,
                <<
                    "SELECT id FROM channel_subscription "
                    "WHERE channel_id=$1 AND user_id=$2 AND status=1 FOR SHARE"
                >>,
                [ChannelId, Uid]
            ),
            %% Lock first, then re-evaluate wall-clock entitlement after any lock wait.
            _ = rows(
                Conn,
                <<
                    "SELECT id FROM channel_order WHERE channel_id=$1 "
                    "AND user_id=$2 AND status=1 FOR SHARE"
                >>,
                [ChannelId, Uid]
            ),
            Purchased = rows(
                Conn,
                <<
                    "SELECT id FROM channel_order WHERE channel_id=$1 "
                    "AND user_id=$2 AND status=1 AND (subscription_end_at IS NULL "
                    "OR subscription_end_at>clock_timestamp())"
                >>,
                [ChannelId, Uid]
            ),
            {ok, Channel#{
                role => Role,
                workspace_allowed => Workspace,
                subscribed => Subscribed =/= [],
                purchased => Purchased =/= []
            }}
    end.

role_tx(Conn, Uid, Channel) ->
    case
        rows(
            Conn,
            <<
                "SELECT role FROM channel_admin WHERE channel_id=$1 "
                "AND user_id=$2 FOR SHARE"
            >>,
            [maps:get(<<"id">>, Channel), Uid]
        )
    of
        [#{<<"role">> := Role}] ->
            Role;
        [] ->
            case maps:get(<<"creator_uid">>, Channel) =:= Uid of
                true -> 3;
                false -> 0
            end
    end.

workspace_access_tx(_Conn, _Uid, #{<<"scope">> := <<"personal">>}) ->
    true;
workspace_access_tx(Conn, Uid, #{<<"scope">> := <<"workspace">>, <<"workspace_id">> := WsId}) ->
    Members = rows(
        Conn,
        <<
            "SELECT wm.user_id FROM workspace_member wm "
            "WHERE wm.workspace_id=$1 AND wm.user_id=$2 AND wm.status='active' FOR SHARE"
        >>,
        [WsId, Uid]
    ),
    case Members of
        [_] ->
            true;
        [] ->
            rows(
                Conn,
                <<
                    "SELECT om.user_id FROM workspace w "
                    "JOIN organization o ON o.id=w.organization_id AND o.status='active' "
                    "JOIN organization_member om ON om.organization_id=o.id "
                    "WHERE w.id=$1 AND om.user_id=$2 AND om.status='active' "
                    "AND om.role IN ('owner','admin') FOR SHARE OF om,o"
                >>,
                [WsId, Uid]
            ) =/= []
    end;
workspace_access_tx(_, _, _) ->
    false.

message_tx(Conn, ChannelId, MessageId) ->
    case
        rows(
            Conn,
            <<
                "SELECT id,channel_id,author_id,payload,created_at FROM channel_message "
                "WHERE channel_id=$1 AND id=$2 AND status=1 AND NOT revoked FOR UPDATE"
            >>,
            [ChannelId, MessageId]
        )
    of
        [Message] -> {ok, Message};
        [] -> {error, not_found}
    end.

search_tx(Conn, ChannelId, Filters, {Page, Size}) ->
    Params = [ChannelId, maps:get(q, Filters), maps:get(tag, Filters), maps:get(essence, Filters)],
    Where = <<
        " WHERE channel_id=$1 AND status=1 AND NOT revoked "
        "AND ($2::text='' OR to_tsvector('jiebacfg'::regconfig,coalesce(content,'')) "
        "@@ plainto_tsquery('jiebacfg'::regconfig,$2::text)) "
        "AND ($3::text='' OR (payload->'tags') @> jsonb_build_array($3::text)) "
        "AND ($4::boolean IS NULL OR is_essence=$4::boolean)"
    >>,
    [#{<<"total">> := Total}] = rows(
        Conn, <<"SELECT count(*) AS total FROM channel_message", Where/binary>>, Params
    ),
    List = rows(
        Conn,
        <<
            "SELECT id,channel_id,author_id,author_name,author_avatar,content,"
            "msg_type,payload,is_pinned,is_essence,created_at,updated_at FROM channel_message",
            Where/binary,
            " ORDER BY id DESC LIMIT $5 OFFSET $6"
        >>,
        Params ++ [Size, (Page - 1) * Size]
    ),
    {ok, #{
        list => [decode_payload(Row) || Row <- List], total => Total, page => Page, size => Size
    }}.

essence_tx(Conn, ChannelId, MessageId, Value) ->
    update(
        Conn,
        <<
            "UPDATE channel_message SET is_essence=$3,updated_at=CURRENT_TIMESTAMP "
            "WHERE channel_id=$1 AND id=$2 AND status=1 AND NOT revoked RETURNING id,is_essence,is_pinned"
        >>,
        [ChannelId, MessageId, Value]
    ).

tags_tx(Conn, ChannelId, MessageId, Tags) ->
    %% jsonb_set modifies only tags; attachments and every other payload key survive.
    case
        update(
            Conn,
            <<
                "UPDATE channel_message SET payload=jsonb_set(payload,'{tags}',$3::jsonb,true),"
                "edited_at=CURRENT_TIMESTAMP,updated_at=CURRENT_TIMESTAMP "
                "WHERE channel_id=$1 AND id=$2 AND status=1 AND NOT revoked RETURNING id,payload"
            >>,
            [ChannelId, MessageId, jsone:encode(Tags)]
        )
    of
        {ok, Row} -> {ok, decode_payload(Row)};
        Error -> Error
    end.

decode_payload(Row) ->
    Decoded = elib_response:json_decode_field(Row, <<"payload">>),
    case maps:get(<<"payload">>, Decoded, undefined) of
        Payload when is_map(Payload) -> Decoded;
        _ -> throw({abort_tx, {503, <<"内容服务暂不可用"/utf8>>}})
    end.

update(Conn, Sql, Params) ->
    case rows(Conn, Sql, Params) of
        [Row] -> {ok, Row};
        [] -> {error, not_found}
    end.

rows(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, Result} when is_list(Result) -> Result;
        _ -> throw({abort_tx, {503, <<"内容服务暂不可用"/utf8>>}})
    end.
