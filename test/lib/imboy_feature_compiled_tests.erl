-module(imboy_feature_compiled_tests).

-include_lib("eunit/include/eunit.hrl").
-include("generated/imboy_product_features.hrl").

generated_manifest_metadata_is_exposed_test() ->
    ?assertEqual(?IMBOY_PRODUCT_FEATURE_MANIFEST_HASH, imboy_feature:manifest_hash()),
    ?assertEqual(?IMBOY_PRODUCT_FEATURE_SCHEMA_VERSION, imboy_feature:manifest_schema_version()).

base_only_filters_and_selected_routes_are_annotated_test() ->
    Routes = [
        {"/base", user_handler, #{action => show}},
        {"/channel", channel_handler, #{action => show}},
        {"/discover", channel_discovery_handler, #{action => discover}},
        {"/e2ee", olm_handler, #{action => claim_key}}
    ],
    ?assertEqual(
        [{"/base", user_handler, #{action => show}}],
        imboy_feature:compiled_routes(api, Routes, [core])
    ),
    ?assertEqual(
        [
            {"/base", user_handler, #{action => show}},
            {"/channel", channel_handler, #{action => show, required_feature => channel}}
        ],
        imboy_feature:compiled_routes(api, Routes, [core, channel])
    ).

route_feature_covers_split_handlers_test() ->
    Cases = [
        {api, olm_handler, claim_key, e2ee},
        {api, group_handler, set_e2ee_mode, e2ee},
        {api, location_handler, people_nearby, location},
        {api, moment_handler, feed, moment},
        {api, report_handler, moment_create, moment},
        {api, channel_discovery_handler, search, channel_discover},
        {api, channel_handler_admin, create_invitation, channel_invitation},
        {api, channel_handler_admin, admins, channel},
        {api, channel_handler_order, pay_order, channel_order},
        {api, channel_handler_comment, create_comment, channel},
        {api, channel_webhook_handler, incoming, channel},
        {api, group_vote_handler, create, group_vote},
        {api, group_schedule_handler, create, group_schedule},
        {api, group_task_handler, create, group_task},
        {admin, adm_admin_handler, compliance_key_list, e2ee},
        {admin, adm_channel_handler, invitations, channel_invitation},
        {admin, adm_channel_handler, refund_order, channel_order},
        {admin, adm_channel_handler, list, channel},
        {admin, adm_moment_handler, list, moment},
        {admin, adm_group_vote_handler, vote_list, group_vote},
        {admin, adm_group_schedule_handler, schedule_list, group_schedule},
        {admin, adm_group_task_handler, task_list, group_task},
        {admin, adm_report_handler, channel_list, channel}
    ],
    lists:foreach(
        fun({Surface, Handler, Action, Expected}) ->
            ?assertEqual(Expected, imboy_feature:route_feature(Surface, Handler, Action))
        end,
        Cases
    ),
    ?assertEqual(
        undefined,
        imboy_feature:route_feature(admin, adm_group_schedule_handler, governance_log_list)
    ).

explicit_feature_metadata_is_authoritative_test() ->
    Route = {"/plugin", custom_handler, #{action => run, required_feature => moment}},
    ?assertEqual([], imboy_feature:compiled_routes(api, [Route], [core])),
    ?assertEqual([Route], imboy_feature:compiled_routes(api, [Route], [core, moment])).
