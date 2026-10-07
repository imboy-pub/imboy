-module(ac04_s5_readback_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% Q06/AC04 S5 重启回读（独立 eunit VM = 应用重启语义）
%%%
%%% 本模块与 ac04_e2ee_off_archive_db_tests 分属两次独立 eunit 进程：
%%% 前者在 VM-1 完成写入（staging→正式表）并把 msg_id 清单落盘
%%% /tmp/q06_ac04_s5_manifest.term；本模块在 VM-2（全新 Erlang VM +
%%% 全新应用 boot，缓存/进程全空，等价"重启后回读"）读回：
%%%   - history 分页 msg_id 集合与写入侧逐一致（顺序按 conv_seq）
%%%   - FTS 关键词命中同一集合
%%%   - staging 无 pending 残留
%%% 若 manifest 不存在（单独跑本模块）则显式失败提示先跑写入侧。
%%%===================================================================

-define(S5_MANIFEST, "/tmp/q06_ac04_s5_manifest.term").

read_manifest() ->
    {ok, Bin} = file:read_file(?S5_MANIFEST),
    binary_to_term(Bin).

s5_readback_after_restart_test_() ->
    ?TEST_WITH_DB_TIMEOUT(60, fun() ->
        case filelib:is_file(?S5_MANIFEST) of
            false ->
                io:format(
                    "manifest missing (~s): run ac04_e2ee_off_archive_db_tests first~n",
                    [?S5_MANIFEST]
                ),
                ?assert(filelib:is_file(?S5_MANIFEST));
            true ->
                {A, B, Kw, MsgIds} = read_manifest(),
                %% 回读一：history 分页（新 VM，无任何进程内缓存）
                {ok, #{<<"messages">> := Msgs, <<"has_more">> := More}} =
                    messaging_logic:history(A, <<"c2c">>, integer_to_binary(B), 0, 50),
                ?assertEqual(false, More),
                ReadBackIds = [maps:get(<<"msg_id">>, M) || M <- Msgs],
                %% 集合包含写入侧全集（会话内可能有其他套件行，用包含断言）
                ?assertEqual(
                    lists:sort(MsgIds),
                    lists:sort(lists:filter(fun(Id) -> lists:member(Id, MsgIds) end, ReadBackIds))
                ),
                %% 回读二：FTS 命中同一集合
                {ok, Hits} = fts_user_repo:search_c2c_msg(Kw, 50, 0, A),
                HitIds = [maps:get(<<"msg_id">>, M) || M <- Hits],
                lists:foreach(
                    fun(Id) -> ?assert(lists:member(Id, HitIds)) end,
                    MsgIds
                ),
                %% 回读三：staging 无 pending 残留
                {ok, [#{<<"n">> := PendN}]} = elib_pg:query(
                    <<
                        "SELECT count(*) AS n FROM public.msg_store_staging ",
                        "WHERE msg_id = ANY($1) AND processed_at IS NULL"
                    >>,
                    [MsgIds]
                ),
                ?assertEqual(0, PendN)
        end
    end).
