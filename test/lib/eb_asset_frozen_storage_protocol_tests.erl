%%% Real PG + Garage object boundary; not seat/widget HTTP authorization acceptance.
%%% V3-20：disk 驱动退役，disk 遍随驱动删除，仅保留 garage 冻结协议面。
-module(eb_asset_frozen_storage_protocol_tests).
-include_lib("eunit/include/eunit.hrl").
protocol_test_() ->
    %% 受控让位（仓库先例：attachment_lifecycle_v5_pg_tests r20_test_/
    %% v5_gate_pinned、w403_gateway_protocol_tests protocol_test_：eunit 无
    %% {skip, ...} 顶层描述符，生成器返回空清单才是等价的静默让位机制）。
    %% 本套件 setup 直调 attachment_lifecycle_v5_w403_gateway_protocol_
    %% tests:setup/0（required("W403_RESOURCE_MANIFEST") fail-closed），是
    %% r12 中该模块 required/1 抛错的三条外部调用链之一；nightly 全量裸跑
    %% （make eunit-local-legacy）不经 gate，缺钉名 env 时在生成器入口
    %% 让位，不进 setup。gate 路径（env 在场）语义与断言零删减。
    case os:getenv("W403_RESOURCE_MANIFEST") of
        V when is_list(V), V =/= "" ->
            protocol_units();
        _ ->
            io:format(user, "EB_FROZEN_EUNIT_YIELD missing_env=\"W403_RESOURCE_MANIFEST\"~n", []),
            []
    end.

protocol_units() ->
    {setup, fun attachment_lifecycle_v5_w403_gateway_protocol_tests:setup/0,
        fun attachment_lifecycle_v5_w403_gateway_protocol_tests:cleanup/1, fun(S) ->
            [
                {
                    atom_to_list(D) ++ "/" ++ binary_to_list(Kind),
                    {timeout, 90, fun() -> scenario(S, D, Kind) end}
                }
             || %% V3-20：disk 驱动退役，disk 遍随驱动删除，garage 侧保留。
                D <- [garage],
                Kind <- [<<"seat">>, <<"visitor_session">>]
            ]
        end}.
scenario(S, Driver, Kind) ->
    {ok, Original} = application:get_env(imboy, attachment_storage_registry),
    %% V3-20：disk 驱动退役，冻结命名空间仅剩 garage 根。
    Ns =
        case Driver of
            garage -> <<"w403-garage">>
        end,
    Registry = Original#{upload_namespaces => #{<<"enterprise">> => Ns}},
    application:set_env(imboy, attachment_storage_registry, Registry),
    application:set_env(imboy, eb_asset_object_store, garage),
    Org = 8000000000000 + binary:decode_unsigned(crypto:strong_rand_bytes(5)),
    Ws = Org + 1,
    Id = Org + 2,
    Uid = maps:get(uid, S),
    Bytes = <<"EB frozen driver actual bytes">>,
    {ok, [_]} = elib_pg:query(
        <<"INSERT INTO organization(id,name,owner_id) VALUES($1,'owned-eb-storage',$2) RETURNING id">>,
        [Org, Uid]
    ),
    {ok, [_]} = elib_pg:query(
        <<"INSERT INTO workspace(id,organization_id,name,owner_id,branding) VALUES($1,$2,'owned-eb-storage',$3,'{}') RETURNING id">>,
        [Ws, Org, Uid]
    ),
    Subject = #{<<"kind">> => Kind, <<"principal">> => integer_to_binary(Uid)},
    Hash = eb_asset_content:sha256_hex(Bytes),
    Now = erlang:system_time(second),
    Cid = elib_uuid:gen_v7(),
    D = #{
        id => Id,
        conversation_id => null,
        uploaded_by_user_id => Uid,
        object_hash => Hash,
        mime => <<"text/plain">>,
        size_bytes => byte_size(Bytes)
    },
    %% Admission validates actual bytes against the frozen presign declaration.
    Registration = #{
        <<"subject">> => Subject,
        <<"declaration">> => #{
            <<"mime">> => maps:get(mime, D),
            <<"object_hash">> => Hash,
            <<"size_bytes">> => byte_size(Bytes),
            <<"conversation_id">> => null,
            <<"file_name">> => null,
            <<"message_id">> => null,
            <<"retain_until">> => null
        }
    },
    {ok, Id} = eb_pg_asset_meta:register_session_v5(Org, Ws, D, Cid, Registration, {
        Now + 3600, Now + 300
    }),
    {ok, Row} = eb_pg_asset_meta:fetch_asset(Org, Ws, Id),
    ?assertEqual(atom_to_binary(Driver), maps:get(storage_driver, Row)),
    ?assertEqual(Ns, maps:get(storage_namespace, Row)),
    %% Same CID reuses the frozen root even after the upload mapping changes.
    application:set_env(imboy, attachment_storage_registry, Registry#{
        upload_namespaces => #{<<"enterprise">> => <<"missing">>}
    }),
    ?assertEqual(
        {ok, Id},
        eb_pg_asset_meta:register_session_v5(Org, Ws, D#{id => Id + 9}, Cid, Registration, {
            Now + 3600, Now + 300
        })
    ),
    application:set_env(imboy, attachment_storage_registry, Registry),
    Ctx = #{
        issued_at => calendar:system_time_to_universal_time(Now, second),
        expires_at => calendar:system_time_to_universal_time(Now + 300, second)
    },
    Put = D#{payload => Bytes, upload_subject => Subject, signature_context => Ctx},
    ?assertMatch({ok, _}, eb_asset_store:put_private(Org, Ws, Put)),
    ?assertEqual({ok, {content_stream, Bytes}}, eb_asset_store:stream_content(Org, Ws, Id)),
    %% Binary compatibility and the HTTP sealed-source path pin the same receipt.
    attachment_upload_spool:with_source(Bytes, fun(Source) ->
        ?assertEqual(byte_size(Bytes), attachment_upload_spool:size(Source)),
        ?assertEqual(Hash, attachment_upload_spool:sha256(Source)),
        ?assertMatch({ok, _}, eb_asset_store:put_private(Org, Ws, Put#{payload => Source}))
    end),
    ?assertEqual({ok, {content_stream, Bytes}}, eb_asset_store:stream_content(Org, Ws, Id)),
    ?assertMatch({error, _}, eb_asset_store:put_private(Org, Ws, Put#{payload => <<"different">>})),
    Roots = maps:map(
        fun(_, R) ->
            case maps:get(namespace, R) of
                Ns -> R#{endpoint => <<"file:///must-not-touch-eb-remap">>};
                _ -> R
            end
        end,
        maps:get(roots, Registry)
    ),
    application:set_env(imboy, attachment_storage_registry, Registry#{roots => Roots}),
    ?assertEqual(
        {error, {object_store, location_changed}}, eb_asset_store:stream_content(Org, Ws, Id)
    ),
    ?assertEqual(
        {error, {object_store, location_changed}}, eb_asset_store:put_private(Org, Ws, Put)
    ),
    application:set_env(imboy, attachment_storage_registry, Registry),
    ?assertMatch({ok, _}, eb_asset_store:confirm_asset(Org, Ws, Id)),
    ?assertEqual({ok, {content_stream, Bytes}}, eb_asset_store:stream_content(Org, Ws, Id)),
    ?assertMatch({error, _}, eb_asset_store:put_private(Org, Ws, Put)),
    %% Cleanup only the unique test object using its validated frozen identity.
    Frozen = maps:from_list([
        {atom_to_binary(K), maps:get(K, Row)}
     || K <- [storage_driver, storage_namespace, storage_container, storage_locator_sha256]
    ]),
    {ok, I} = attachment_storage:frozen_location(maps:get(object_key, Row), Frozen),
    ?assertEqual(ok, attachment_storage:delete_snapshot(I)),
    application:set_env(imboy, attachment_storage_registry, Original).
