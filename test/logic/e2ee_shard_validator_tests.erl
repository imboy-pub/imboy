-module(e2ee_shard_validator_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc E2EE 分片传输审计日志测试
%%%
%%% 回归背景：本模块曾存在 lib/logic 两份同名源文件，调用方按
%%% (Event, ShardId, Meta) 传参而生效实现按 (FromUid, ToUid, ShardId)
%%% 解参，uid 列被填 atom、INSERT 必败且错误被吞——审计链零记录。
%%% 本测试锁死"调用方契约 → DS 落库字段"的映射关系。
%%%===================================================================

log_shard_transmission_maps_caller_contract_to_db_fields_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_shard_transmission_log_ds, [
                {'insert', 1, fun(Data) ->
                    %% 字段类型与取值必须符合 repo 落库契约
                    ?assertEqual(<<"shard-001">>, maps:get(shard_id, Data)),
                    ?assertEqual(9999, maps:get(uid, Data)),
                    ?assertEqual(1001, maps:get(proxy_uid, Data)),
                    ?assertEqual(<<"shard_created">>, maps:get(action, Data)),
                    ?assertEqual(<<"server_to_proxy">>, maps:get(direction, Data)),
                    ?assertEqual(<<"key-v1">>, maps:get(key_version, Data)),
                    ?assert(is_integer(maps:get(created_at, Data))),
                    %% 额外元数据收进 metadata（JSON binary）
                    Meta = jsone:decode(maps:get(metadata, Data), [{object_format, map}]),
                    ?assertEqual(1, maps:get(<<"shard_index">>, Meta)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            ok = e2ee_shard_validator:log_shard_transmission(
                shard_created,
                <<"shard-001">>,
                #{
                    <<"uid">> => 9999,
                    <<"proxy_uid">> => 1001,
                    <<"key_version">> => <<"key-v1">>,
                    <<"shard_index">> => 1
                }
            ),
            ?assertEqual(1, meck:num_calls(e2ee_shard_transmission_log_ds, insert, 1))
        end
    ).

log_shard_transmission_decrypt_direction_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_shard_transmission_log_ds, [
                {'insert', 1, fun(Data) ->
                    ?assertEqual(<<"proxy_to_user">>, maps:get(direction, Data)),
                    ?assertEqual(<<"shard_decrypted">>, maps:get(action, Data)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            %% ShardId 传 integer（elib_tsid 原始值）也必须被规整为 binary
            ok = e2ee_shard_validator:log_shard_transmission(
                shard_decrypted,
                123456789,
                #{<<"uid">> => <<"9999">>, <<"proxy_uid">> => 1001}
            )
        end
    ).

log_shard_transmission_swallow_db_error_test_() ->
    ?WITH_MECKS(
        [
            {e2ee_shard_transmission_log_ds, [
                {'insert', 1, fun(_) -> {error, closed} end}
            ]}
        ],
        fun() ->
            %% 日志失败不影响主流程
            ?assertEqual(
                ok,
                e2ee_shard_validator:log_shard_transmission(
                    shard_sent,
                    <<"shard-002">>,
                    #{<<"uid">> => 1, <<"proxy_uid">> => 2}
                )
            )
        end
    ).
