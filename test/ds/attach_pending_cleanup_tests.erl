-module(attach_pending_cleanup_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc #20：待确认 presign 回收
%%%
%%% 缺口：attach_logic:presign/5 签发后不落任何库行，而
%%% attachment_repo:orphan_list_for_delete/1 是
%%% `SELECT ... FROM attachment WHERE status = 1 AND referer_time = 0`
%%% —— 只扫表不扫桶。"PUT 上去但从不 confirm"的对象在库里不存在，
%%% 清理器永远看不见它。
%%%
%%% confirm 路径本身已有兜底（verify_and_save 做 HEAD 核实 + max_file_size
%%% 超限即 delete_object），所以缺口不在"大小无约束"而在"根本不走 confirm"。
%%%
%%% 本测试锁定 attachment_ds:pending_cleanup/1 的三条不变量。
%%%===================================================================

%% 删对象必须带上登记行里的 bucket，不能走 delete_object/1 的默认桶
cleanup_deletes_with_recorded_bucket_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [
                {'list_expired', 1, fun(24) ->
                    {ok, [
                        #{<<"object_key">> => <<"k1">>, <<"bucket">> => <<"b1">>},
                        #{<<"object_key">> => <<"k2">>, <<"bucket">> => <<"b2">>}
                    ]}
                end},
                {'delete_by_keys', 1, fun(Keys) ->
                    put(deleted_keys, lists:sort(Keys)),
                    ok
                end}
            ]},
            {elib_oss, [
                {'delete_object', 2, fun(_Bucket, _Key) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{cleaned => 2, errors => 0}},
                attachment_ds:pending_cleanup(24)
            ),
            ?assertEqual([<<"k1">>, <<"k2">>], erase(deleted_keys)),
            ?assert(meck:called(elib_oss, delete_object, [<<"b1">>, <<"k1">>])),
            ?assert(meck:called(elib_oss, delete_object, [<<"b2">>, <<"k2">>]))
        end
    ).

%% S3 删除失败的键**不得**从登记表里删掉，否则这个对象再也没人管
cleanup_keeps_row_when_object_delete_fails_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [
                {'list_expired', 1, fun(24) ->
                    {ok, [
                        #{<<"object_key">> => <<"ok_key">>, <<"bucket">> => <<"b1">>},
                        #{<<"object_key">> => <<"bad_key">>, <<"bucket">> => <<"b1">>}
                    ]}
                end},
                {'delete_by_keys', 1, fun(Keys) ->
                    put(deleted_keys, Keys),
                    ok
                end}
            ]},
            {elib_oss, [
                {'delete_object', 2, fun
                    (<<"b1">>, <<"bad_key">>) -> {error, timeout};
                    (<<"b1">>, <<"ok_key">>) -> ok
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{cleaned => 1, errors => 1}},
                attachment_ds:pending_cleanup(24)
            ),
            %% 只销 ok_key；bad_key 留在表里等下一轮重试
            ?assertEqual([<<"ok_key">>], erase(deleted_keys))
        end
    ).

%% 无过期登记时不得调用任何删除
cleanup_noop_when_nothing_expired_test_() ->
    ?WITH_MECKS(
        [
            {attach_pending_repo, [
                {'list_expired', 1, fun(24) -> {ok, []} end},
                {'delete_by_keys', 1, fun(_) -> ok end}
            ]},
            {elib_oss, [
                {'delete_object', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{cleaned => 0, errors => 0}},
                attachment_ds:pending_cleanup(24)
            ),
            ?assertNot(meck:called(elib_oss, delete_object, ['_', '_'])),
            ?assertNot(meck:called(attach_pending_repo, delete_by_keys, ['_']))
        end
    ).
