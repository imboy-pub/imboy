-module(talk_record_part_upload_direct_tests).
-moduledoc "V3-18 P1：录音分片上传 direct s3_put 化（真 PG + 真 Garage 端到端）".
%%%
%%% 断言（G18 的分片上传面）：
%%%   * prepare：register→declare→grant 返回 s3_put descriptor（transport/
%%%     url/required_headers 含 content-type + checksum）；重试幂等（同 Cid
%%%     同 key、expected 同值不再报冲突）；
%%%   * PUT staging（真 presign URL + required_headers）→ S3 checksum 验证；
%%%   * verify_upload（confirm 面）：finalize 收口 → CONFIRMED；confirmed
%%%     重放 ok；
%%%   * 反例：未 PUT 先 confirm → proof 失败拒绝；错字节（checksum 不符）
%%%     在 PUT 时被 S3 拒绝（非 2xx）。
%%% 前置：A0_ENDPOINT/A0_AK/A0_SK（v3/fixture/garage-v3run1.env）。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(UID, 992010).
-define(RECORD, 992020).

part_upload_direct_test_() ->
    {timeout, 120, fun part_upload_direct/0}.

part_upload_direct() ->
    false = os:getenv("A0_ENDPOINT") =:= false,
    {ok, _} = eunit_runner:eunit_setup_with_db(),
    Ep = list_to_binary(os:getenv("A0_ENDPOINT")),
    Cfg = #{
        endpoint => Ep,
        bucket => <<"imboy">>,
        public_bucket => <<"imboy">>,
        access_key => list_to_binary(os:getenv("A0_AK")),
        secret_key => list_to_binary(os:getenv("A0_SK")),
        region => <<"garage">>
    },
    PrevGarage = application:get_env(imboy, garage),
    PrevRegistry = application:get_env(imboy, attachment_storage_registry),
    application:set_env(imboy, garage, Cfg),
    Base = attachment_storage:registry(),
    application:set_env(imboy, attachment_storage_registry, Base#{
        roots => (maps:get(roots, Base, #{}))#{
            <<"a0-root">> => #{
                driver => <<"garage">>,
                namespace => <<"a0-ns">>,
                endpoint => Ep,
                container => <<"imboy">>,
                access_key => maps:get(access_key, Cfg),
                secret_key => maps:get(secret_key, Cfg),
                region => <<"garage">>
            }
        },
        upload_namespaces => (maps:get(upload_namespaces, Base, #{}))#{
            <<"talk_record">> => <<"a0-ns">>
        }
    }),
    try
        run(Cfg)
    after
        cleanup_fixture(),
        case PrevGarage of
            undefined -> application:unset_env(imboy, garage);
            {ok, V} -> application:set_env(imboy, garage, V)
        end,
        case PrevRegistry of
            undefined -> application:unset_env(imboy, attachment_storage_registry);
            {ok, R} -> application:set_env(imboy, attachment_storage_registry, R)
        end
    end.

run(_Cfg) ->
    Body = <<"G18-PART-AAC-", 0:(24 * 8)>>,
    Sha = binary:encode_hex(crypto:hash(sha256, Body), lowercase),
    Part = #{
        <<"part_id">> => 1,
        <<"codec">> => <<"aac">>,
        <<"sha256">> => Sha,
        <<"byte_size">> => byte_size(Body)
    },
    %% 1) prepare：direct descriptor
    {ok, #{grant := G1, attempt_id := No1, object_key := Key}} =
        talk_record_part_upload:prepare(?UID, ?RECORD, Part),
    ?assertEqual(<<"s3_put">>, maps:get(transport, G1)),
    ?assert(is_binary(maps:get(url, G1)) andalso byte_size(maps:get(url, G1)) > 0),
    Hs = maps:get(required_headers, G1),
    {_, Ct} = lists:keyfind(<<"Content-Type">>, 1, Hs),
    ?assertEqual(<<"audio/aac">>, Ct),
    ?assert(lists:keymember(<<"x-amz-checksum-sha256">>, 1, Hs)),
    io:format("descriptor_ok: attempt=~p ct=~ts key=~ts~n", [No1, Ct, Key]),
    %% 2) 重试幂等：同 part 再 prepare（register 幂等 + expected 同值）
    {ok, #{object_key := Key, grant := _G2}} =
        talk_record_part_upload:prepare(?UID, ?RECORD, Part),
    io:format("prepare_retry_idempotent: ok~n"),
    %% 3) 反例：未 PUT 先 confirm → finalize proof 失败（staging 无对象）
    PartRow = #{<<"part_id">> => 1, <<"sha256">> => Sha, <<"byte_size">> => byte_size(Body)},
    ?assertMatch(
        {error, _},
        talk_record_part_upload:verify_upload(?UID, ?RECORD, PartRow)
    ),
    io:format("confirm_before_put_rejected: ok~n"),
    %% 4) 真 PUT staging（required_headers 逐字）→ S3 checksum 验证
    {ok, 200} = staging_put(G1, Body),
    io:format("staging_put_s3_checksum: ok~n"),
    %% 5) 反例：错字节被 S3 拒绝（新 attempt 的 URL 签名绑原 checksum）
    {ok, #{grant := G3}} =
        talk_record_part_upload:prepare(?UID, ?RECORD, Part),
    Wrong = <<"WRONG-BYTES-", 0:(24 * 8)>>,
    ?assertMatch(
        {error, _},
        staging_put_raw(G3, Wrong)
    ),
    io:format("wrong_bytes_rejected_by_s3: ok~n"),
    %% 5.5) renew 后新 attempt 的 staging 重新 PUT 正确字节（I05：新
    %% attempt 新 target，旧 PUT 作废——恢复语义）
    {ok, 200} = staging_put(G3, Body),
    io:format("renew_reput_recovery: ok~n"),
    %% 6) verify（confirm 面）：finalize 收口 CONFIRMED
    ok = talk_record_part_upload:verify_upload(?UID, ?RECORD, PartRow),
    io:format("verify_finalize_confirmed: ok~n"),
    %% 7) confirmed 重放 ok
    ok = talk_record_part_upload:verify_upload(?UID, ?RECORD, PartRow),
    io:format("verify_replay_ok: ok~n"),
    ok.

staging_put(#{url := Url, required_headers := Headers}, Body) ->
    Hs = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    {_, Ctype} = lists:keyfind("content-type", 1, lowercase(Hs)),
    case
        httpc:request(
            put,
            {binary_to_list(Url), Hs, Ctype, Body},
            [{timeout, 30000}, {autoredirect, false}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, S, _}, _, _}} when S >= 200, S < 300 -> {ok, 200};
        {ok, {{_, S, _}, _, B}} -> {error, {put_status, S, B}};
        {error, R} -> {error, R}
    end.

staging_put_raw(#{url := Url, required_headers := Headers}, Body) ->
    %% 原样头 + 错误字节：S3 checksum 校验应拒绝（非 2xx）
    Hs = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    {_, Ctype} = lists:keyfind("content-type", 1, lowercase(Hs)),
    case
        httpc:request(
            put,
            {binary_to_list(Url), Hs, Ctype, Body},
            [{timeout, 30000}, {autoredirect, false}],
            [{body_format, binary}]
        )
    of
        {ok, {{_, S, _}, _, _}} when S >= 200, S < 300 -> {ok, 200};
        {ok, {{_, S, _}, _, _}} -> {error, {rejected, S}};
        {error, R} -> {error, R}
    end.

lowercase(Hs) ->
    [{string:lowercase(K), V} || {K, V} <- Hs].

cleanup_fixture() ->
    %% 合成段清理（uid 992010 段；V5 守卫下会话/attempt 行；object_key
    %% 形态 u992010/talk_record/<date>/<hash>/part-N.aac 不含 record id，
    %% 按 creator uid 段清理；Garage 对象残留由 run 收口按资源账登记）
    catch elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(Conn, <<"SET LOCAL session_replication_role = replica">>, []),
        {ok, _} = elib_pg:query(
            Conn, <<"DELETE FROM attach_direct_attempt WHERE object_key LIKE 'u992010/%'">>, []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<
                "DELETE FROM attachment WHERE creator_user_id = 992010"
                " AND scope = 'talk_record'"
            >>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM attach_pending WHERE creator_user_id = 992010">>,
            []
        ),
        ok
    end).
