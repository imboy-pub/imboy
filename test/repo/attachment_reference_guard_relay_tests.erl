-module(attachment_reference_guard_relay_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

reference_guard_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun talk_record_t19_support:teardown_all/1, fun(S) ->
            ?_test(run(S))
        end}}.
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
    real_disk_intent_rollback(C, Old).

%% Positive evidence uses real bytes and a trusted registry identity. Metadata,
%% GC proof and deletion intent all roll back; no unrelated worker queue is drained.
real_disk_intent_rollback(C, Old) ->
    Base = os:getenv("TALK_UPLOAD_DISK_ROOT"),
    true = is_list(Base) andalso filename:pathtype(Base) =:= absolute,
    Dir = filename:join(
        Base, "reference-guard-" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Saved = application:get_env(imboy, attachment_storage_registry),
    Ns = <<"reference-guard-private">>,
    Bucket = <<"synthetic">>,
    Root = #{
        root_id => <<"reference_guard">>,
        driver => <<"disk">>,
        namespace => Ns,
        container => Bucket,
        endpoint => <<"file://", (list_to_binary(Dir))/binary>>
    },
    application:set_env(imboy, attachment_storage_registry, #{
        version => 1, roots => #{reference_guard => Root}
    }),
    Key = <<"synthetic/", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    {ok, Location} = attachment_storage:resolve(Ns),
    {ok, Identity} = attachment_storage:identity(Ns, Bucket, Key),
    I = Identity#{endpoint => maps:get(endpoint, Location)},
    Bytes = <<"synthetic-reference-guard:", Key/binary>>,
    try
        ok = attachment_storage_disk:put_snapshot(I, Bytes, infinity),
        ?assertEqual({ok, Bytes}, attachment_storage_disk:read_snapshot(I)),
        Meta = #{
            <<"path">> => Key,
            <<"name">> => <<"synthetic.txt">>,
            <<"url">> => <<>>,
            <<"mime_type">> => <<"text/plain">>,
            <<"size">> => byte_size(Bytes),
            <<"scope">> => <<"private">>,
            <<"file_hash256">> => binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
            <<"storage_driver">> => <<"disk">>,
            <<"storage_namespace">> => Ns,
            <<"storage_container">> => Bucket,
            <<"storage_locator_sha256">> => maps:get(locator_digest, I)
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
        ?assertEqual({ok, Bytes}, attachment_storage_disk:read_snapshot(I))
    after
        attachment_storage_disk:delete_snapshot(I),
        case Saved of
            {ok, V} -> application:set_env(imboy, attachment_storage_registry, V);
            undefined -> application:unset_env(imboy, attachment_storage_registry)
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
