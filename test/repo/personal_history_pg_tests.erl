-module(personal_history_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% MH-02 个人消息历史域真库验证（迁移 276-279，本地隔离库）。
%%%
%%% 覆盖合同（mh01-personal-history-contract v1.1）锚点：
%%%   §1/§2.1 默认 OFF 拒绝上传与 status 四态
%%%   §2.2 opt-in 幂等（同代次同状态）与 purge 中 409
%%%   §2.3/W1 upload 幂等（同键二次返回原结果不覆盖）+ 参与关系 fail-closed
%%%         + action_version 终态推进/迟到旧版本 409
%%%   §10.4 nonce 重用 409
%%%   §2.4/W2 changes 单调发放 + ack-cursor 水位推进（≤ 已发放最大值，幂等）
%%%   §2.4/MSG-H15 page conv_seq DESC 分页 + tombstone（deleted_at）
%%%   §2.5 密钥包版本冲突 409 / malformed 422 / 未开启 403
%%%   §2.6/W3/W4 disable 原子代次提升 + 删账 + 旧代次写 410 + 新代次重开
%%%
%%% DB 不可用时按 TEST_WITH_DB 惯例 skip。
%%%===================================================================

-define(TEST_ACCOUNTS_MIN, 98600100).
-define(TEST_ACCOUNTS_MAX, 98600199).
-define(A1, 98600101).
-define(A2, 98600102).
-define(OTHER1, 98600103).
-define(OTHER2, 98600104).

%%%-------------------------------------------------------------------
%%% Helpers
%%%-------------------------------------------------------------------

cleanup_test_rows() ->
    Min = integer_to_binary(?TEST_ACCOUNTS_MIN),
    Max = integer_to_binary(?TEST_ACCOUNTS_MAX),
    Tables = [
        <<"public.personal_history_archive">>,
        <<"public.personal_history_authorization">>,
        <<"public.personal_history_cursor">>,
        <<"public.personal_history_key_package">>
    ],
    lists:foreach(
        fun(Tb) ->
            Sql =
                <<"DELETE FROM ", Tb/binary, " WHERE account_id BETWEEN ", Min/binary, " AND ",
                    Max/binary>>,
            {ok, _} = elib_pg:execute(Sql, [])
        end,
        Tables
    ),
    ok.

domain() ->
    personal_history_ds:domain().

conv(A, B) ->
    Min = erlang:min(A, B),
    Max = erlang:max(A, B),
    <<"c2c:", (integer_to_binary(Min))/binary, ":", (integer_to_binary(Max))/binary>>.

archive_count(AccountId) ->
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(
        <<"SELECT count(*) AS count FROM public.personal_history_archive",
            " WHERE account_id = $1 AND domain = $2">>,
        [AccountId, domain()]
    ),
    Count.

auth_row(AccountId) ->
    {ok, [Row]} = elib_pg:query(
        <<"SELECT state, generation FROM public.personal_history_authorization",
            " WHERE account_id = $1 AND domain = $2">>,
        [AccountId, domain()]
    ),
    Row.

key_package_count(AccountId) ->
    {ok, [#{<<"count">> := Count}]} = elib_pg:query(
        <<"SELECT count(*) AS count FROM public.personal_history_key_package",
            " WHERE account_id = $1 AND domain = $2">>,
        [AccountId, domain()]
    ),
    Count.

opt_in(AccountId) ->
    {ok, Payload} = personal_history_logic:opt_in(AccountId, <<"ph-test-instance">>),
    Payload.

put_key_package(AccountId, KeyVersion) ->
    personal_history_logic:put_key_package(AccountId, #{
        <<"key_version">> => KeyVersion,
        <<"wrapped_key">> => base64:encode(<<"wrapped-v", (integer_to_binary(KeyVersion))/binary>>),
        <<"kdf_meta">> => #{
            <<"algorithm">> => <<"pbkdf2-sha256">>,
            <<"iterations">> => 310000
        },
        <<"integrity_hash">> => <<"aabbccdd", (integer_to_binary(KeyVersion))/binary>>
    }).

upload_params(AccountId, Peer, MsgId, Nonce, Seq) ->
    upload_params(AccountId, Peer, MsgId, Nonce, Seq, 0, undefined).

upload_params(AccountId, Peer, MsgId, Nonce, Seq, ActionVersion, Generation) ->
    Meta0 = #{
        <<"alg">> => <<"aes-256-gcm">>,
        <<"key_version">> => 1,
        <<"nonce">> => base64:encode(Nonce),
        <<"aad">> => base64:encode(<<"aad:", MsgId/binary>>)
    },
    Params = #{
        <<"conversation_id">> => conv(AccountId, Peer),
        <<"msg_id">> => MsgId,
        <<"action_version">> => ActionVersion,
        <<"conv_seq">> => Seq,
        <<"ciphertext">> => base64:encode(<<"ct-", MsgId/binary>>),
        <<"envelope_meta">> => Meta0,
        <<"client_ts">> => 1789600000000
    },
    case Generation of
        undefined -> Params;
        Gen -> Params#{<<"generation">> => Gen}
    end.

%%%-------------------------------------------------------------------
%%% §2.1/§2.3 默认 OFF：无授权 = 零写入，upload/key-package 全拒
%%%-------------------------------------------------------------------

default_off_rejects_upload_and_key_package_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        %% status：默认 OFF / generation 0（MSG-H01 零授权）
        {ok, Status} = personal_history_logic:status(?A1),
        ?assertEqual(<<"OFF">>, maps:get(<<"state">>, Status)),
        ?assertEqual(0, maps:get(<<"generation">>, Status)),
        %% 未开启上传 → 403 not_enabled
        ?assertEqual(
            {error, <<"not_enabled">>, 403},
            personal_history_logic:upload(
                ?A1, upload_params(?A1, ?A2, <<"m1">>, <<"n1">>, 1)
            )
        ),
        %% 未开启密钥包 → 403 not_enabled
        ?assertEqual(
            {error, <<"not_enabled">>, 403},
            put_key_package(?A1, 1)
        ),
        %% 无授权行、无归档行、无密钥包
        ?assertEqual(0, archive_count(?A1)),
        ?assertEqual(0, key_package_count(?A1)),
        {ok, undefined} = personal_history_repo:find_authorization(?A1, domain()),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.2 opt-in 幂等：同代次重复返回同状态
%%%-------------------------------------------------------------------

opt_in_idempotent_same_generation_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        First = opt_in(?A1),
        ?assertEqual(<<"ON">>, maps:get(<<"state">>, First)),
        ?assertEqual(<<"created">>, maps:get(<<"result">>, First)),
        ?assertEqual(1, maps:get(<<"generation">>, First)),
        Second = opt_in(?A1),
        ?assertEqual(<<"idempotent">>, maps:get(<<"result">>, Second)),
        ?assertEqual(maps:get(<<"generation">>, First), maps:get(<<"generation">>, Second)),
        %% 幂等不重置 enabled_at：同代次同状态
        ?assertEqual(
            maps:get(<<"enabled_at">>, First), maps:get(<<"enabled_at">>, Second)
        ),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.3/W1 opt-in 后上传幂等：同键二次返回原结果不覆盖
%%%-------------------------------------------------------------------

upload_idempotent_returns_original_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        {ok, Stored} = personal_history_logic:upload(
            ?A1, upload_params(?A1, ?A2, <<"m-dup">>, <<"nonce-dup">>, 7)
        ),
        ?assertEqual(<<"stored">>, maps:get(<<"result">>, Stored)),
        StoredSeq = maps:get(<<"event_seq">>, Stored),
        %% 同键重试：返回原结果（同 event_seq），不产生第二语义记录
        {ok, Dup} = personal_history_logic:upload(
            ?A1, upload_params(?A1, ?A2, <<"m-dup">>, <<"nonce-dup">>, 7)
        ),
        ?assertEqual(<<"duplicate">>, maps:get(<<"result">>, Dup)),
        ?assertEqual(StoredSeq, maps:get(<<"event_seq">>, Dup)),
        ?assertEqual(1, archive_count(?A1)),
        %% 未排序 conv_key 规范化后同键（与投递链 conv_key 同源）
        Unsorted = (upload_params(?A1, ?A2, <<"m-dup">>, <<"nonce-dup">>, 7))#{
            <<"conversation_id">> => conv(?A2, ?A1)
        },
        {ok, Dup2} = personal_history_logic:upload(?A1, Unsorted),
        ?assertEqual(<<"duplicate">>, maps:get(<<"result">>, Dup2)),
        ?assertEqual(1, archive_count(?A1)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §10.4 nonce 重用 → 409 nonce_reused
%%%-------------------------------------------------------------------

nonce_reuse_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        {ok, _} = personal_history_logic:upload(
            ?A1, upload_params(?A1, ?A2, <<"m-n1">>, <<"shared-nonce">>, 1)
        ),
        %% 不同 msg_id 复用同 (key_version, nonce) → 409，第二行不落库
        ?assertEqual(
            {error, <<"nonce_reused">>, 409},
            personal_history_logic:upload(
                ?A1, upload_params(?A1, ?A2, <<"m-n2">>, <<"shared-nonce">>, 2)
            )
        ),
        ?assertEqual(1, archive_count(?A1)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §1/§2.3 参与关系 fail-closed：非两端会话与域外键同型 403
%%%-------------------------------------------------------------------

not_participant_fail_closed_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        %% 他人会话（OTHER1-OTHER2）→ not_participant
        OtherConv = (upload_params(?A1, ?A2, <<"m-oth">>, <<"n-oth">>, 1))#{
            <<"conversation_id">> => conv(?OTHER1, ?OTHER2)
        },
        ?assertEqual(
            {error, <<"not_participant">>, 403},
            personal_history_logic:upload(?A1, OtherConv)
        ),
        %% 域外/群会话键（c2g 不在本域）→ 同型同时序 403
        C2G = (upload_params(?A1, ?A2, <<"m-c2g">>, <<"n-c2g">>, 1))#{
            <<"conversation_id">> => <<"c2g:990001">>
        },
        ?assertEqual(
            {error, <<"not_participant">>, 403},
            personal_history_logic:upload(?A1, C2G)
        ),
        %% 畸形键 → 同型 403
        Bad = (upload_params(?A1, ?A2, <<"m-bad">>, <<"n-bad">>, 1))#{
            <<"conversation_id">> => <<"c2c:not-a-number:9">>
        },
        ?assertEqual(
            {error, <<"not_participant">>, 403},
            personal_history_logic:upload(?A1, Bad)
        ),
        ?assertEqual(0, archive_count(?A1)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.3 action_version：迟到旧版本 409；更高版本终态推进
%%%-------------------------------------------------------------------

action_version_conflict_and_advance_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        {ok, _} = personal_history_logic:upload(
            ?A1, upload_params(?A1, ?A2, <<"m-ver">>, <<"n-v2">>, 5, 2, undefined)
        ),
        %% 旧版本迟到：不得按重试覆盖已有终态
        ?assertEqual(
            {error, <<"version_conflict">>, 409},
            personal_history_logic:upload(
                ?A1, upload_params(?A1, ?A2, <<"m-ver">>, <<"n-v1">>, 5, 1, undefined)
            )
        ),
        %% 更高版本 = 编辑/撤回终态推进（同一语义记录更新）
        {ok, Advanced} = personal_history_logic:upload(
            ?A1, upload_params(?A1, ?A2, <<"m-ver">>, <<"n-v3">>, 5, 3, undefined)
        ),
        ?assertEqual(<<"advanced">>, maps:get(<<"result">>, Advanced)),
        ?assertEqual(3, maps:get(<<"action_version">>, Advanced)),
        ?assertEqual(1, archive_count(?A1)),
        {ok, [#{<<"action_version">> := 3}]} = elib_pg:query(
            <<"SELECT action_version FROM public.personal_history_archive",
                " WHERE account_id = $1 AND domain = $2">>,
            [?A1, domain()]
        ),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.5 密钥包：版本冲突 409 / malformed 422（迭代下限+PEM 拒收）
%%%-------------------------------------------------------------------

key_package_conflict_and_malformed_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, Saved} = put_key_package(?A1, 1),
        ?assertEqual(1, maps:get(<<"key_version">>, Saved)),
        %% 同版本重复 → 409 version_conflict
        ?assertEqual(
            {error, <<"version_conflict">>, 409},
            put_key_package(?A1, 1)
        ),
        %% 迭代低于 310000 下限 → 422 malformed_package（§10.4）
        ?assertEqual(
            {error, <<"malformed_package">>, 422},
            personal_history_logic:put_key_package(?A1, #{
                <<"key_version">> => 2,
                <<"wrapped_key">> => base64:encode(<<"w2">>),
                <<"kdf_meta">> => #{
                    <<"algorithm">> => <<"pbkdf2-sha256">>, <<"iterations">> => 100000
                },
                <<"integrity_hash">> => <<"hash2">>
            })
        ),
        %% PEM 明文拒收（§10.5：279 沿用拒收下限）
        Pem = <<"-----BEGIN PRIVATE KEY-----\nQUFBQQ==\n-----END PRIVATE KEY-----\n">>,
        ?assertEqual(
            {error, <<"malformed_package">>, 422},
            personal_history_logic:put_key_package(?A1, #{
                <<"key_version">> => 3,
                <<"wrapped_key">> => Pem,
                <<"kdf_meta">> => #{
                    <<"algorithm">> => <<"pbkdf2-sha256">>, <<"iterations">> => 310000
                },
                <<"integrity_hash">> => <<"hash3">>
            })
        ),
        %% 未知 key_version 上传 → 422 key_package_missing（fail-closed）
        Missing = (upload_params(?A1, ?A2, <<"m-nokey">>, <<"n-nokey">>, 1))#{
            <<"envelope_meta">> => #{
                <<"alg">> => <<"aes-256-gcm">>,
                <<"key_version">> => 99,
                <<"nonce">> => base64:encode(<<"n-nokey">>),
                <<"aad">> => base64:encode(<<"aad">>)
            }
        },
        ?assertEqual(
            {error, <<"key_package_missing">>, 422},
            personal_history_logic:upload(?A1, Missing)
        ),
        ?assertEqual(1, key_package_count(?A1)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.4/W2 changes 单调发放 + ack-cursor 水位推进
%%%-------------------------------------------------------------------

changes_issue_and_ack_watermark_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        lists:foreach(
            fun(Seq) ->
                Nonce = <<"n-chg-", (integer_to_binary(Seq))/binary>>,
                MsgId = <<"m-chg-", (integer_to_binary(Seq))/binary>>,
                {ok, _} = personal_history_logic:upload(
                    ?A1, upload_params(?A1, ?A2, MsgId, Nonce, Seq)
                )
            end,
            [1, 2, 3]
        ),
        %% W2：cursor 只见已 commit 归档；单调发放
        {ok, C1} = personal_history_logic:changes(?A1, #{
            cursor => 0, limit => 2, client_instance => <<"inst-1">>
        }),
        Events1 = maps:get(<<"events">>, C1),
        ?assertEqual(2, length(Events1)),
        Next1 = maps:get(<<"next_cursor">>, C1),
        ?assertEqual(false, maps:get(<<"reset_hint">>, C1)),
        Seqs1 = [maps:get(<<"event_seq">>, E) || E <- Events1],
        ?assertEqual(lists:sort(Seqs1), Seqs1),
        %% 续读余量
        {ok, C2} = personal_history_logic:changes(?A1, #{
            cursor => Next1, limit => 50, client_instance => <<"inst-1">>
        }),
        ?assertEqual(1, length(maps:get(<<"events">>, C2))),
        Next2 = maps:get(<<"next_cursor">>, C2),
        %% ack：推进 ≤ 已发放最大值；重复 ack 幂等（W5 不影响历史）
        {ok, Ack1} = personal_history_logic:ack_cursor(?A1, #{
            cursor => Next2, client_instance => <<"inst-1">>
        }),
        ?assertEqual(true, maps:get(<<"accepted">>, Ack1)),
        ?assertEqual(Next2, maps:get(<<"advanced_to">>, Ack1)),
        {ok, Ack2} = personal_history_logic:ack_cursor(?A1, #{
            cursor => Next2, client_instance => <<"inst-1">>
        }),
        ?assertEqual(true, maps:get(<<"accepted">>, Ack2)),
        ?assertEqual(Next2, maps:get(<<"advanced_to">>, Ack2)),
        %% 越界 ack（大于已发放最大值）不推进
        {ok, Ack3} = personal_history_logic:ack_cursor(?A1, #{
            cursor => Next2 + 1000, client_instance => <<"inst-1">>
        }),
        ?assertEqual(false, maps:get(<<"accepted">>, Ack3)),
        ?assertEqual(Next2, maps:get(<<"advanced_to">>, Ack3)),
        %% 水位落库：acked = change_cursor = Next2
        {ok, #{<<"acked_cursor">> := Next2, <<"change_cursor">> := Next2}} =
            personal_history_repo:find_cursor(?A1, domain(), 1, <<"inst-1">>),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.4/§10.2 游标出窗 → reset_hint（有界 change log）
%%%-------------------------------------------------------------------

changes_reset_hint_when_out_of_window_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        {ok, _} = personal_history_logic:upload(
            ?A1, upload_params(?A1, ?A2, <<"m-win">>, <<"n-win">>, 1)
        ),
        %% ds 层直证：游标落后超过窗口 → reset_hint，不发放事件
        {ok, G, Events, NextCursor, ResetHint} = personal_history_ds:issue_changes(
            ?A1, domain(), <<"inst-win">>, -20000, 10
        ),
        ?assertEqual(1, G),
        ?assertEqual([], Events),
        ?assertEqual(undefined, NextCursor),
        ?assertEqual(true, ResetHint),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.4/MSG-H15 page conv_seq DESC 分页 + tombstone（deleted_at）
%%%-------------------------------------------------------------------

page_desc_and_tombstone_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        lists:foreach(
            fun(Seq) ->
                Nonce = <<"n-pg-", (integer_to_binary(Seq))/binary>>,
                MsgId = <<"m-pg-", (integer_to_binary(Seq))/binary>>,
                {ok, _} = personal_history_logic:upload(
                    ?A1, upload_params(?A1, ?A2, MsgId, Nonce, Seq)
                )
            end,
            [1, 2, 3]
        ),
        %% 撤回终态投影：msg2 墓碑化（保留行，deleted_at 置位）
        {ok, 1} = personal_history_ds:mark_deleted(
            ?A1, domain(), conv(?A1, ?A2), <<"m-pg-2">>
        ),
        {ok, Page} = personal_history_logic:page(?A1, #{
            conversation_id => conv(?A1, ?A2), before => 0
        }),
        Msgs = maps:get(<<"messages">>, Page),
        ?assertEqual(3, length(Msgs)),
        ?assertEqual([3, 2, 1], [maps:get(<<"conv_seq">>, M) || M <- Msgs]),
        BySeq = lists:keyfind(2, 1, [{maps:get(<<"conv_seq">>, M), M} || M <- Msgs]),
        ?assertMatch({2, #{<<"deleted">> := true}}, BySeq),
        ?assertEqual(
            [false, true, false], [maps:get(<<"deleted">>, M) || M <- Msgs]
        ),
        %% before 锚点分页：conv_seq < 3 → 2,1
        {ok, Page2} = personal_history_logic:page(?A1, #{
            conversation_id => conv(?A1, ?A2), before => 3
        }),
        ?assertEqual([2, 1], [maps:get(<<"conv_seq">>, M) || M <- maps:get(<<"messages">>, Page2)]),
        %% 非参与方 page → 403
        ?assertEqual(
            {error, <<"not_participant">>, 403},
            personal_history_logic:page(?A1, #{
                conversation_id => conv(?OTHER1, ?OTHER2), before => 0
            })
        ),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §10.2 page_token 校验：代次/时效/链长（数值界）
%%%-------------------------------------------------------------------

page_token_bounds_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 往返：当前代次解码成功
        Token = personal_history_ds:encode_page_token(7, 3, 12345),
        ?assertEqual({ok, 3, 12345}, personal_history_ds:decode_page_token(Token, 7)),
        %% 代次不符 → generation_expired（§2.6 拒绝旧页令牌）
        ?assertEqual(
            {error, generation_expired}, personal_history_ds:decode_page_token(Token, 8)
        ),
        %% 过期 → page_token_expired（TTL 外）
        Old = personal_history_ds:encode_page_token(7, 0, 1),
        Aged = aged_token(7, 0, 1),
        ?assertEqual({ok, 0, 1}, personal_history_ds:decode_page_token(Old, 7)),
        ?assertEqual(
            {error, page_token_expired}, personal_history_ds:decode_page_token(Aged, 7)
        ),
        %% 链长超界 → rescan_bound_exceeded（重扫数值界）
        Deep = hop_token(7, 101, 1),
        ?assertEqual(
            {error, rescan_bound_exceeded}, personal_history_ds:decode_page_token(Deep, 7)
        ),
        %% 畸形 token → page_token_expired
        ?assertEqual(
            {error, page_token_expired},
            personal_history_ds:decode_page_token(<<"not-a-token">>, 7)
        )
    end).

aged_token(Gen, Hop, Before) ->
    Issued = erlang:system_time(millisecond) - 700000,
    token_plain(Gen, Hop, Before, Issued).

hop_token(Gen, Hop, Before) ->
    token_plain(Gen, Hop, Before, erlang:system_time(millisecond)).

token_plain(Gen, Hop, Before, Issued) ->
    Plain =
        <<"v1:", (integer_to_binary(Gen))/binary, ":", (integer_to_binary(Hop))/binary, ":",
            (integer_to_binary(Before))/binary, ":", (integer_to_binary(Issued))/binary>>,
    base64:encode(Plain).

%%%-------------------------------------------------------------------
%%% §2.6/W3/W4 disable：原子代次提升 + 删账 + 旧代次写 410 + 新代次重开
%%%-------------------------------------------------------------------

disable_purges_and_stale_writes_410_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        {ok, _} = personal_history_logic:upload(
            ?A1, upload_params(?A1, ?A2, <<"m-del">>, <<"n-del">>, 1)
        ),
        ?assertEqual(1, archive_count(?A1)),
        %% disable：202 语义载荷（state + purge_ticket + 新代次）
        {ok, Disable} = personal_history_logic:disable(?A1),
        ?assertEqual(<<"OFF_PURGE_PENDING">>, maps:get(<<"state">>, Disable)),
        ?assert(is_binary(maps:get(<<"purge_ticket">>, Disable))),
        ?assertEqual(2, maps:get(<<"generation">>, Disable)),
        %% 删账完成：密文+游标+密钥封装销毁，防复活标记留存（OFF_PURGED g2）
        ?assertEqual(0, archive_count(?A1)),
        ?assertEqual(0, key_package_count(?A1)),
        ?assertMatch(
            #{<<"state">> := <<"OFF_PURGED">>, <<"generation">> := 2}, auth_row(?A1)
        ),
        %% delete-status：online_done + PITR 窗口诚实提示（§10.6）
        {ok, Ds} = personal_history_logic:delete_status(?A1),
        ?assertEqual(<<"online_done">>, maps:get(<<"phase">>, Ds)),
        ?assert(is_binary(maps:get(<<"backup_window_hint">>, Ds))),
        %% W4：旧代次迟到写全部 410（带旧 generation 视角）
        ?assertEqual(
            {error, <<"generation_expired">>, 410},
            personal_history_logic:upload(
                ?A1, upload_params(?A1, ?A2, <<"m-late">>, <<"n-late">>, 2, 0, 1)
            )
        ),
        %% 关闭后无代次视角的新写同样 410（OFF_PURGED 防复活）
        ?assertEqual(
            {error, <<"generation_expired">>, 410},
            personal_history_logic:upload(
                ?A1, upload_params(?A1, ?A2, <<"m-late2">>, <<"n-late2">>, 3)
            )
        ),
        %% 旧代次 page/changes 同样拒绝
        ?assertEqual(
            {error, <<"generation_expired">>, 410},
            personal_history_logic:page(?A1, #{
                conversation_id => conv(?A1, ?A2), before => 0
            })
        ),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.2/W3 purge 中 opt-in 409；OFF_PURGE_PENDING 可重入补齐
%%%-------------------------------------------------------------------

purge_pending_blocks_opt_in_and_reenters_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        %% 只走 disable 第一步（W3 崩溃模拟：删账未执行）
        {ok, _Row, Gen2} = personal_history_ds:disable(?A1, domain()),
        ?assertEqual(2, Gen2),
        ?assertMatch(#{<<"state">> := <<"OFF_PURGE_PENDING">>}, auth_row(?A1)),
        %% purge 中 opt-in → 409 purge_in_progress
        ?assertEqual(
            {error, <<"purge_in_progress">>, 409},
            personal_history_logic:opt_in(?A1, <<"inst-x">>)
        ),
        %% delete-status：pending + retry
        {ok, Pending} = personal_history_logic:delete_status(?A1),
        ?assertEqual(<<"pending">>, maps:get(<<"phase">>, Pending)),
        %% 可重入：purge_online 整段重放完成
        {ok, _} = personal_history_ds:purge_online(?A1, domain(), Gen2),
        ?assertMatch(#{<<"state">> := <<"OFF_PURGED">>}, auth_row(?A1)),
        {ok, Done} = personal_history_logic:delete_status(?A1),
        ?assertEqual(<<"online_done">>, maps:get(<<"phase">>, Done)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.2/§2.6 重启用新代次；防复活（跨代不复活旧数据）
%%%-------------------------------------------------------------------

reenable_uses_new_generation_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        {ok, _} = personal_history_logic:upload(
            ?A1, upload_params(?A1, ?A2, <<"m-old">>, <<"n-old">>, 1)
        ),
        {ok, _} = personal_history_logic:disable(?A1),
        %% 重开：新代次（g3），不回填（旧归档已随 purge 删除）
        Re = opt_in(?A1),
        ?assertEqual(<<"reenabled">>, maps:get(<<"result">>, Re)),
        ?assertEqual(3, maps:get(<<"generation">>, Re)),
        ?assertEqual(0, archive_count(?A1)),
        %% 旧代次（g1）视角写 → 410；新代次（g3）写需新密钥包
        ?assertEqual(
            {error, <<"generation_expired">>, 410},
            personal_history_logic:upload(
                ?A1, upload_params(?A1, ?A2, <<"m-cross">>, <<"n-cross">>, 9, 0, 1)
            )
        ),
        {ok, _} = put_key_package(?A1, 2),
        {ok, Fresh} = personal_history_logic:upload(
            ?A1,
            (upload_params(?A1, ?A2, <<"m-fresh">>, <<"n-fresh">>, 10, 0, 3))#{
                <<"envelope_meta">> => #{
                    <<"alg">> => <<"aes-256-gcm">>,
                    <<"key_version">> => 2,
                    <<"nonce">> => base64:encode(<<"n-fresh">>),
                    <<"aad">> => base64:encode(<<"aad">>)
                }
            }
        ),
        ?assertEqual(<<"stored">>, maps:get(<<"result">>, Fresh)),
        ?assertEqual(3, maps:get(<<"generation">>, Fresh)),
        ?assertEqual(1, archive_count(?A1)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.3 请求形状校验：非法参数显式 422（fail-closed，无静默吞）
%%%----------------------------------------------------------------###

malformed_upload_rejected_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?A1),
        {ok, _} = put_key_package(?A1, 1),
        %% 缺 ciphertext / 非整数 conv_seq / 空 msg_id → 422
        Base = upload_params(?A1, ?A2, <<"m-mal">>, <<"n-mal">>, 1),
        ?assertEqual(
            {error, <<"malformed_request">>, 422},
            personal_history_logic:upload(?A1, maps:remove(<<"ciphertext">>, Base))
        ),
        ?assertEqual(
            {error, <<"malformed_request">>, 422},
            personal_history_logic:upload(?A1, Base#{<<"conv_seq">> => <<"x">>})
        ),
        ?assertEqual(
            {error, <<"malformed_request">>, 422},
            personal_history_logic:upload(?A1, Base#{<<"msg_id">> => <<>>})
        ),
        %% 非 base64 密文 → 422
        ?assertEqual(
            {error, <<"malformed_request">>, 422},
            personal_history_logic:upload(?A1, Base#{<<"ciphertext">> => <<"...">>})
        ),
        %% ack-cursor 非法游标 → 422
        ?assertEqual(
            {error, <<"malformed_request">>, 422},
            personal_history_logic:ack_cursor(?A1, #{
                cursor => -1, client_instance => <<"inst-m">>
            })
        ),
        ?assertEqual(0, archive_count(?A1)),
        cleanup_test_rows()
    end).

%%%-------------------------------------------------------------------
%%% §2.5b GET key-package（mh04-key-package-read-addendum：恢复读取入口）
%%%-------------------------------------------------------------------

get_key_package_roundtrip_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        _ = opt_in(?OTHER1),
        {ok, _} = put_key_package(?OTHER1, 1),
        %% 正常读取：字段沿用 PUT 丰富度（wrapped_key b64 原样、kdf_meta、
        %% integrity_hash、created_at），代次/版本同域。
        {ok, Pkg} = personal_history_logic:get_key_package(?OTHER1, #{
            key_version => <<"1">>, generation => <<"1">>
        }),
        ?assertEqual(1, maps:get(<<"key_version">>, Pkg)),
        ?assertEqual(1, maps:get(<<"generation">>, Pkg)),
        ?assertEqual(
            base64:encode(<<"wrapped-v1">>),
            maps:get(<<"wrapped_key">>, Pkg)
        ),
        ?assertEqual(
            #{<<"algorithm">> => <<"pbkdf2-sha256">>, <<"iterations">> => 310000},
            maps:get(<<"kdf_meta">>, Pkg)
        ),
        ?assertEqual(<<"aabbccdd1">>, maps:get(<<"integrity_hash">>, Pkg)),
        ?assert(maps:is_key(<<"created_at">>, Pkg)),
        %% retired_at 不自动否定历史旧版本可读性（addendum）。
        {ok, _} = elib_pg:execute(
            <<"UPDATE public.personal_history_key_package SET retired_at = now()",
                " WHERE account_id = $1 AND domain = $2 AND key_version = 1">>,
            [?OTHER1, domain()]
        ),
        ?assertMatch(
            {ok, #{<<"key_version">> := 1}},
            personal_history_logic:get_key_package(?OTHER1, #{
                key_version => <<"1">>, generation => <<"1">>
            })
        ),
        cleanup_test_rows()
    end).

get_key_package_param_domain_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        %% 参数无效在授权检查之前（addendum 校验顺序）——OFF 账号同样 422。
        Malformed = [
            {<<"1">>, <<"x">>},
            {<<"1">>, <<>>},
            {<<"01">>, <<"1">>},
            {<<"1e3">>, <<"1">>},
            {<<"-1">>, <<"1">>},
            {<<"+1">>, <<"1">>},
            {<<" 1">>, <<"1">>},
            {<<"0">>, <<"1">>},
            {<<"1">>, <<"-0">>},
            {<<"1">>, <<"9007199254740992">>},
            {<<"9007199254740992">>, <<"1">>},
            {<<>>, <<>>},
            {not_a_binary, <<"1">>},
            {<<"1">>, not_a_binary}
        ],
        lists:foreach(
            fun({Version, Generation}) ->
                ?assertEqual(
                    {error, <<"malformed_package_request">>, 422},
                    personal_history_logic:get_key_package(?OTHER2, #{
                        key_version => Version, generation => Generation
                    })
                )
            end,
            Malformed
        ),
        %% 域上界含端点：版本/代次 9007199254740991 合法（解析层通过）。
        ?assertNotEqual(
            {error, <<"malformed_package_request">>, 422},
            personal_history_logic:get_key_package(?OTHER2, #{
                key_version => <<"9007199254740991">>,
                generation => <<"9007199254740991">>
            })
        ),
        cleanup_test_rows()
    end).

get_key_package_authorization_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        cleanup_test_rows(),
        %% 未开启（默认 OFF）：403 not_enabled。
        ?assertEqual(
            {error, <<"not_enabled">>, 403},
            personal_history_logic:get_key_package(?OTHER2, #{
                key_version => <<"1">>, generation => <<"1">>
            })
        ),
        _ = opt_in(?OTHER2),
        {ok, _} = put_key_package(?OTHER2, 1),
        %% 代次比对当前 ON 授权：旧代次读 410 generation_expired。
        ?assertEqual(
            {error, <<"generation_expired">>, 410},
            personal_history_logic:get_key_package(?OTHER2, #{
                key_version => <<"1">>, generation => <<"0">>
            })
        ),
        %% 指定包缺失（当前代次、未知版本）：422 key_package_missing。
        ?assertEqual(
            {error, <<"key_package_missing">>, 422},
            personal_history_logic:get_key_package(?OTHER2, #{
                key_version => <<"7">>, generation => <<"1">>
            })
        ),
        %% 不能以不含 generation 的唯一索引代替代次条件：手工注入
        %% key_version=9/generation=9 的行，按 (9, 当前 g=1) 查询必须落空。
        {ok, _} = elib_pg:execute(
            <<"INSERT INTO public.personal_history_key_package",
                " (account_id, domain, key_version, generation, wrapped_key, kdf_meta, integrity_hash)",
                " VALUES ($1, $2, 9, 9, $3, '{\"algorithm\":\"pbkdf2-sha256\",\"iterations\":310000}'::jsonb, 'x')">>,
            [?OTHER2, domain(), <<"cross-gen">>]
        ),
        ?assertEqual(
            {error, <<"key_package_missing">>, 422},
            personal_history_logic:get_key_package(?OTHER2, #{
                key_version => <<"9">>, generation => <<"1">>
            })
        ),
        %% 关闭后（OFF_PURGED + 代次提升）：跨代读一律 410。
        {ok, _} = personal_history_logic:disable(?OTHER2),
        ?assertMatch(
            {error, <<"generation_expired">>, 410},
            personal_history_logic:get_key_package(?OTHER2, #{
                key_version => <<"1">>, generation => <<"1">>
            })
        ),
        cleanup_test_rows()
    end).
