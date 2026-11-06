-module(talk_record_export_download_ref_tests).
-moduledoc "导出内容流与边缘引用共用鉴权链的纯 mock 回归，不依赖数据库或对象存储。".

-include_lib("eunit/include/eunit.hrl").

-define(SECRET, <<"export-reference-test-secret-32-bytes">>).

reference_and_content_share_authorization_test() ->
    with_fixture(fun() ->
        Ticket = ticket(),
        {ok, Ref} = talk_record_export_download:verify_ticket_ref(Ticket, 7, 1, 9),
        ?assertEqual(21, maps:get(<<"attachment_id">>, Ref)),
        ?assertEqual(false, maps:is_key(<<"content">>, Ref)),
        assert_no_byte_io(),
        %% 内容读取契约已退役（V3-18 边缘承载：ref 零字节、字节经 presigned
        %% URL 流转），此处只保留鉴权链与零字节断言。
        ?assertEqual(1, meck:num_calls(talk_record_export_logic, authorize_tx, [conn, 7, 1]))
    end).

edge_target_uses_reference_without_reading_artifact_test() ->
    with_fixture(fun() ->
        ?assertEqual(
            {ok, {<<"https://storage.invalid/export">>, <<"talk-record-export.md">>}},
            talk_record_export_download:edge_target(ticket())
        ),
        ?assertEqual(0, meck:num_calls(talk_record_export_artifact, read, '_')),
        ?assertEqual(1, meck:num_calls(attachment_edge_redirect, presign_attachment, [21, 600])),
        ?assertEqual(1, meck:num_calls(talk_record_export_logic, authorize_tx, [conn, 7, 1]))
    end).

current_guards_reject_before_byte_io_test_() ->
    Access = {ok, #{<<"acl_revision">> => 4}},
    Row = ready_row(),
    Cases = [
        {"revoked", {error, not_found}, Row, not_found},
        {"ACL advanced", {ok, #{<<"acl_revision">> => 5}}, Row, acl_stale},
        {"ACL lookup failed", {error, db_error}, Row, db_error},
        {"other requester", Access, Row#{<<"requester_id">> => 8}, export_ticket_invalid},
        {"queued", Access, Row#{<<"state">> => <<"queued">>}, export_not_ready},
        {"failed", Access, Row#{<<"state">> => <<"failed">>}, export_failed},
        {"expired", Access, Row#{<<"expires_at">> => 0}, export_expired},
        {"missing expiry", Access, Row#{<<"expires_at">> => null}, export_expired}
    ],
    [
        {Name, fun() ->
            with_fixture(fun() ->
                meck:expect(talk_record_export_logic, authorize_tx, fun(conn, 7, 1) ->
                    AuthResult
                end),
                meck:expect(talk_record_export_repo, find_tx, fun(conn, 1, 9) ->
                    {ok, ExportRow}
                end),
                Ticket = ticket(),
                ?assertEqual(
                    {error, Reason}, talk_record_export_download:verify_ticket_ref(Ticket)
                ),
                ?assertEqual({error, Reason}, talk_record_export_download:edge_target(Ticket)),
                assert_no_byte_io()
            end)
        end}
     || {Name, AuthResult, ExportRow, Reason} <- Cases
    ].

bound_identity_checked_before_acl_or_byte_io_test_() ->
    [
        {Name, fun() ->
            with_fixture(fun() ->
                Ticket = ticket(),
                ?assertEqual(
                    {error, export_ticket_invalid},
                    talk_record_export_download:verify_ticket_ref(Ticket, Uid, Rid, Eid)
                ),
                ?assertEqual(0, meck:num_calls(talk_record_export_logic, authorize_tx, '_')),
                assert_no_byte_io()
            end)
        end}
     || {Name, Uid, Rid, Eid} <- [
            {"actor", 8, 1, 9}, {"record", 7, 2, 9}, {"export", 7, 1, 10}
        ]
    ].

presign_failure_is_preserved_test() ->
    %% 工件字节不再内联读取（V3-18）；等价的失败透传点是 edge_target 的
    %% presign 错误——不得被吞掉或误报成其它错误。
    with_fixture(fun() ->
        meck:expect(attachment_edge_redirect, presign_attachment, fun(21, 600) ->
            {error, presign_unavailable}
        end),
        ?assertEqual(
            {error, presign_unavailable}, talk_record_export_download:edge_target(ticket())
        )
    end).

expired_export_cannot_issue_ticket_test_() ->
    [
        {Name, fun() ->
            with_fixture(fun() ->
                ?assertEqual(
                    {error, export_expired},
                    talk_record_export_download:issue(1, 9, 7, ExpiresAt, 4)
                )
            end)
        end}
     || {Name, ExpiresAt} <- [{"missing", null}, {"expired", 0}]
    ].

ticket() ->
    {ok, #{<<"ticket">> := Ticket}} =
        talk_record_export_download:issue(1, 9, 7, os:system_time(second) + 3600, 4),
    Ticket.

ready_row() ->
    #{
        <<"id">> => 9,
        <<"state">> => <<"ready">>,
        <<"requester_id">> => 7,
        <<"expires_at">> => os:system_time(second) + 3600,
        <<"attachment_id">> => 21,
        <<"format">> => <<"md">>
    }.

assert_no_byte_io() ->
    ?assertEqual(0, meck:num_calls(talk_record_export_artifact, read, '_')),
    ?assertEqual(0, meck:num_calls(attachment_edge_redirect, presign_attachment, '_')).

with_fixture(F) ->
    SavedKey = application:get_env(imboy, jwt_key),
    Mods = [
        elib_pg,
        talk_record_export_logic,
        talk_record_export_repo,
        talk_record_export_artifact,
        attachment_edge_redirect
    ],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Mods),
    try
        application:set_env(imboy, jwt_key, ?SECRET),
        meck:expect(elib_pg, with_tx, fun(Fn, _) -> Fn(conn) end),
        meck:expect(elib_pg, with_conn, fun(Fn) -> Fn(conn) end),
        meck:expect(talk_record_export_logic, authorize_tx, fun(conn, 7, 1) ->
            {ok, #{<<"acl_revision">> => 4}}
        end),
        meck:expect(talk_record_export_repo, find_tx, fun(conn, 1, 9) -> {ok, ready_row()} end),
        meck:expect(talk_record_export_repo, expire_tx, fun(conn, 9) -> ok end),
        meck:expect(talk_record_export_artifact, read, fun(_) -> {ok, <<"export content">>} end),
        meck:expect(attachment_edge_redirect, presign_attachment, fun(21, 600) ->
            {ok, <<"https://storage.invalid/export">>}
        end),
        F()
    after
        lists:foreach(fun meck:unload/1, Mods),
        case SavedKey of
            {ok, Key} -> application:set_env(imboy, jwt_key, Key);
            undefined -> application:unset_env(imboy, jwt_key)
        end
    end.
