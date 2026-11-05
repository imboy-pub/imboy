-module(attachment_reference_guard_relay_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

reference_guard_test_() ->
    case os:getenv("TALK_OWNED_PG_HOST") of
        H when H =/= false, H =/= "" ->
            {timeout, 60,
                {setup, fun setup/0, fun talk_record_t19_support:teardown_all/1, fun(S) ->
                    ?_test(run(S))
                end}};
        _ ->
            %% 受控让位（talk_record 域统一锚点 TALK_OWNED_PG_HOST，与
            %% T3 映射批/T4 同锚）：本生成器独立直跑（不经 owned fixture
            %% 包装），setup 经 t19→t17→t15→t05 support 直连具名钉库
            %% （默认 55433），全量轨道无钉库必崩计 cancel（run 41409234749）。
            io:format(
                user,
                "REF_GUARD_EUNIT_YIELD missing_env=\"TALK_OWNED_PG_HOST\"~n",
                []
            ),
            []
    end.
setup() ->
    S = talk_record_t19_support:setup_all(),
    C = maps:get(conn, S),
    {ok, [#{<<"base">> := Base}]} = elib_pg:query(
        C, <<"SELECT greatest(coalesce(max(id),0),9000000000000) base FROM attachment">>, []
    ),
    meck:expect(elib_tsid, generate, fun(_) ->
        Base + erlang:unique_integer([positive, monotonic])
    end),
    S.
run(S) ->
    C = maps:get(conn, S),
    Opts = #{age_days => 30},
    {ok, Before} = attachment_repo:orphan_stats(Opts),
    Old = elib_dt:add(elib_dt:now(), {-40 * 86400, second}),
    A = fixture(C, Old, 0),
    Recent = fixture(C, elib_dt:now(), 0),
    Referenced = fixture(C, Old, 1),
    {ok, After} = attachment_repo:orphan_stats(Opts),
    ?assertEqual(maps:get(<<"count">>, Before) + 1, maps:get(<<"count">>, After)),
    ?assertEqual(
        ec_cnv:to_integer(maps:get(<<"total_size">>, Before)) + 7,
        ec_cnv:to_integer(maps:get(<<"total_size">>, After))
    ),
    {ok, Rows} = attachment_repo:orphan_list_for_delete(Opts),
    Ids = [maps:get(<<"id">>, R) || R <- Rows],
    ?assert(lists:member(A, Ids)),
    ?assertNot(lists:member(Recent, Ids)),
    ?assertNot(lists:member(Referenced, Ids)),
    {ok, [#{<<"path">> := Path}]} = attachment_repo:orphan_path_tx(C, A, Opts),
    ?assertEqual({ok, []}, attachment_repo:orphan_path_tx(C, Recent, Opts)),
    ?assertEqual(
        ok, elib_pg:with_tx(fun(X) -> attachment_repo:bind_reference_lock_tx(X, Path) end)
    ),
    ?assertEqual(
        {ok, false}, elib_pg:with_tx(fun(X) -> attachment_repo:refresh_gc_verified_tx(X, Path) end)
    ),
    %% Formal V5 forbids deleting live metadata, even for an old orphan.
    ?assertEqual(
        {rollback, live_delete_blocked},
        elib_pg:with_tx(
            fun(X) ->
                {error, #error{code = <<"23510">>, message = Message}} =
                    attachment_repo:delete_orphan_tx(X, A, Opts),
                true = binary:match(Message, <<"requires committed delete_intent">>) =/= nomatch,
                throw(live_delete_blocked)
            end,
            [{reraise, false}]
        )
    ),
    {ok, [_]} = attachment_repo:orphan_path_tx(C, A, Opts),
    real_garage_intent_rollback(C, Old).

%% Positive evidence uses real bytes and a trusted registry identity. Metadata,
%% GC proof and deletion intent all roll back; no unrelated worker queue is drained.
%% V3-20：disk 驱动退役，探针对象改挂真实 Garage。本套件不在 attv5 车道：
%% garage 身份优先读 talk 车道注入的 TALK_RECORD_PRIVATE_GARAGE_CONFIG
%% （JSON：endpoint/bucket/region/access_key/secret_key/spool_dir），
%% fallback A0_ENDPOINT/A0_A0/A0_SK（桶名取 JSON 缺席时的命名约定值），
%% 两者皆缺 no_go。同时装 imboy garage env（with_snapshot 的签名一致性校验）。
real_garage_intent_rollback(C, Old) ->
    #{endpoint := Endpoint, bucket := Bucket} = garage_env(),
    Saved = application:get_env(imboy, attachment_storage_registry),
    SavedGarage = application:get_env(imboy, garage),
    Ns = <<"reference-guard-private">>,
    ok = application:set_env(imboy, garage, #{
        endpoint => Endpoint,
        bucket => Bucket,
        region => maps:get(region, garage_env(), <<"us-east-1">>),
        access_key => maps:get(access_key, garage_env()),
        secret_key => maps:get(secret_key, garage_env())
    }),
    Root = #{
        root_id => <<"reference_guard">>,
        driver => <<"garage">>,
        namespace => Ns,
        container => Bucket,
        endpoint => Endpoint
    },
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1, roots => #{reference_guard => Root}
    }),
    Key = <<"synthetic/", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    {ok, Identity} = attachment_storage:identity(Ns, Bucket, Key),
    Bytes = <<"synthetic-reference-guard:", Key/binary>>,
    try
        ok = attachment_storage_garage:put(Ns, Bucket, Key, Bytes),
        ?assertEqual({ok, Bytes}, attachment_storage_garage:read(Ns, Bucket, Key)),
        Meta = #{
            <<"path">> => Key,
            <<"name">> => <<"synthetic.txt">>,
            <<"url">> => <<>>,
            <<"mime_type">> => <<"text/plain">>,
            <<"size">> => byte_size(Bytes),
            <<"scope">> => <<"private">>,
            <<"file_hash256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
            <<"storage_driver">> => <<"garage">>,
            <<"storage_namespace">> => Ns,
            <<"storage_container">> => Bucket,
            <<"storage_locator_sha256">> => maps:get(locator_digest, Identity)
        },
        ?assertEqual(
            {rollback, synthetic_intent_rollback},
            elib_pg:with_tx(
                fun(X) ->
                    {ok, Id, true} = attachment_repo:confirm_if_absent_tx(X, Old, 700001, Meta),
                    {ok, 1} = elib_pg:execute(
                        X, <<"UPDATE attachment SET referer_time=0 WHERE id=$1">>, [Id]
                    ),
                    {ok, true} = attachment_repo:refresh_gc_verified_tx(X, Key),
                    ok = attachment_repo:intent_ordinary_delete_tx(
                        X, Key, <<"synthetic orphan test">>
                    ),
                    {ok, [
                        #{
                            <<"lifecycle_state">> := <<"delete_intent">>,
                            <<"job_state">> := <<"ready">>
                        }
                    ]} =
                        elib_pg:query(
                            X,
                            <<
                                "SELECT a.lifecycle_state,q.job_state FROM attachment a"
                                " JOIN attachment_object_deletion q ON q.path=a.path WHERE a.path=$1"
                            >>,
                            [Key]
                        ),
                    throw(synthetic_intent_rollback)
                end,
                [{reraise, false}]
            )
        ),
        ?assertEqual(
            {ok, []}, elib_pg:query(C, <<"SELECT id FROM attachment WHERE path=$1">>, [Key])
        ),
        ?assertEqual(
            {ok, []},
            elib_pg:query(C, <<"SELECT path FROM attachment_object_deletion WHERE path=$1">>, [Key])
        ),
        ?assertEqual({ok, Bytes}, attachment_storage_garage:read(Ns, Bucket, Key))
    after
        attachment_storage_garage:delete(Ns, Bucket, Key),
        case Saved of
            {ok, V} -> application:set_env(imboy, attachment_storage_registry, V);
            undefined -> application:unset_env(imboy, attachment_storage_registry)
        end,
        restore_garage(SavedGarage)
    end.

restore_garage({ok, V}) -> ok = application:set_env(imboy, garage, V);
restore_garage(undefined) -> ok = application:unset_env(imboy, garage).

%% 优先 TALK_RECORD_PRIVATE_GARAGE_CONFIG（JSON 文件，talk 车道契约键集）；
%% fallback A0_ENDPOINT/A0_AK/A0_SK（gate 的 A0 约定，桶名沿用 talk 车道
%% 命名约定 talk-record-relay）；两者皆缺 no_go。
garage_env() ->
    case os:getenv("TALK_RECORD_PRIVATE_GARAGE_CONFIG") of
        Path when is_list(Path), Path =/= "" ->
            {ok, Bin} = file:read_file(Path),
            C = jsone:decode(Bin),
            #{
                endpoint => maps:get(<<"endpoint">>, C),
                bucket => maps:get(<<"bucket">>, C),
                region => maps:get(<<"region">>, C, <<"us-east-1">>),
                access_key => maps:get(<<"access_key">>, C),
                secret_key => maps:get(<<"secret_key">>, C)
            };
        _ ->
            case {os:getenv("A0_ENDPOINT"), os:getenv("A0_AK"), os:getenv("A0_SK")} of
                {EP, AK, SK} when
                    is_list(EP), EP =/= "", is_list(AK), AK =/= "", is_list(SK), SK =/= ""
                ->
                    #{
                        endpoint => list_to_binary(EP),
                        bucket => <<"talk-record-relay">>,
                        region => <<"us-east-1">>,
                        access_key => list_to_binary(AK),
                        secret_key => list_to_binary(SK)
                    };
                _ ->
                    erlang:error({no_go, reference_guard_garage_env_missing})
            end
    end.

fixture(C, Created, Referer) ->
    Path =
        <<"synthetic-orphan-query/",
            (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    {ok, A, true} = attachment_repo:confirm_if_absent_tx(
        C,
        Created,
        700001,
        #{
            <<"path">> => Path,
            <<"name">> => <<"query-only.txt">>,
            <<"url">> => <<>>,
            <<"mime_type">> => <<"text/plain">>,
            <<"size">> => 7,
            <<"scope">> => <<"private">>
        }
    ),
    {ok, 1} = elib_pg:execute(C, <<"UPDATE attachment SET referer_time=$2 WHERE id=$1">>, [
        A, Referer
    ]),
    A.
