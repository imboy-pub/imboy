-module(web_attachment_logic_tests).
-include_lib("eunit/include/eunit.hrl").

frozen_authorized_web_download_test() ->
    Modules = [attachment_ds, attachment_storage, elib_s3_sign],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Previous = application:get_env(imboy, garage),
    try
        application:set_env(imboy, garage, #{public_endpoint => <<"https://s3.test">>}),
        Rec = #{<<"scope">> => <<"private">>, <<"creator_user_id">> => 1,
                <<"storage_driver">> => <<"garage">>, <<"status">> => 1},
        Key = <<"u1/name%2F.png">>,
        Url = <<"https://s3.test/bucket/u1/name%252F.png?X-Amz-Signature=x">>,
        meck:expect(attachment_ds, find_by_path, fun(_) -> {ok, Rec} end),
        meck:expect(attachment_storage, frozen_location, fun(_, _) ->
            {ok, #{driver => garage, container => <<"bucket">>, key => Key,
                   garage_config => #{public_endpoint => <<"https://s3.test">>}}} end),
        meck:expect(elib_s3_sign, client_endpoint, fun(_) -> {ok, <<"https://s3.test">>} end),
        meck:expect(elib_s3_sign, presign_url_at, fun(_, _, _, _, _, _, _, _) -> Url end),
        ?assertEqual({error, forbidden}, attach_logic:web_view_url(2, Key)),
        ?assertEqual(0, meck:num_calls(attachment_storage, frozen_location, '_')),
        ?assertEqual({ok, Url}, attach_logic:web_view_url(1, Key)),
        ?assertEqual(1, meck:num_calls(attachment_storage, frozen_location, '_')),
        meck:expect(attachment_storage, frozen_location, fun(_, _) -> {error, location_unverified} end),
        ?assertEqual({error, forbidden}, attach_logic:web_view_url(1, Key))
    after
        case Previous of {ok, V} -> application:set_env(imboy, garage, V); undefined -> application:unset_env(imboy, garage) end,
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.

group_web_ticket_acl_test() ->
    Modules = [attachment_ds, attachment_download_ticket, config_ds, attachment_storage, elib_s3_sign, attachment_storage_garage],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Previous = application:get_env(imboy, garage),
    try
        application:set_env(imboy, garage, #{public_endpoint => <<"https://s3.test">>}),
        Rec = #{<<"scope">> => <<"group">>, <<"path">> => <<"u1/g">>, <<"status">> => 1, <<"storage_driver">> => <<"garage">>},
        meck:expect(attachment_ds, find_by_path, fun(_) -> {ok, Rec} end),
        meck:expect(attachment_ds, authorize_group_access, fun(_, Uid) -> Uid =:= 1 end),
        meck:expect(attachment_download_ticket, issue_web, fun(_, _, _) -> {ok, <<"a.b-c">>} end),
        meck:expect(attachment_download_ticket, verify_web, fun(_, _) -> {ok, 1, <<"u1/g">>} end),
        meck:expect(attachment_storage, frozen_location, fun(_, _) ->
            {ok, #{driver => garage, container => <<"bucket">>, key => <<"u1/g">>,
                   garage_config => #{public_endpoint => <<"https://s3.test">>}}} end),
        meck:expect(elib_s3_sign, client_endpoint, fun(_) -> {ok, <<"https://s3.test">>} end),
        meck:expect(attachment_storage_garage, presign_snapshot, fun(_, _) -> {ok, <<"https://internal.test/signed">>} end),
        meck:expect(config_ds, env, fun(base_url, _) -> <<"https://api.test/">>;
                                     (edge_base_url, _) -> <<"https://edge.test/edge">>;
                                     (_, Default) -> Default end),
        ?assertEqual({error, forbidden}, attach_logic:web_view_url(2, <<"u1/g">>)),
        ?assertEqual(0, meck:num_calls(attachment_download_ticket, issue_web, '_')),
        ?assertEqual({ok, <<"https://edge.test/edge?ticket=web.a.b-c">>}, attach_logic:web_view_url(1, <<"u1/g">>)),
        ?assertEqual({ok, <<"https://internal.test/signed">>}, attach_logic:web_download(<<"a.b-c">>)),
        meck:expect(attachment_ds, authorize_group_access, fun(_, _) -> false end),
        ?assertEqual({error, forbidden}, attach_logic:web_download(<<"a.b-c">>)),
        meck:expect(attachment_ds, authorize_group_access, fun(_, _) -> true end),
        meck:expect(attachment_storage, frozen_location, fun(_, _) -> {error, location_unverified} end),
        ?assertEqual({error, forbidden}, attach_logic:web_download(<<"a.b-c">>)),
        meck:expect(attachment_ds, find_by_path, fun(_) -> {ok, Rec#{<<"storage_driver">> => <<"legacy">>}} end),
        ?assertEqual({error, forbidden}, attach_logic:web_view_url(1, <<"u1/g">>)),
        ?assertEqual({error, forbidden}, attach_logic:web_download(<<"a.b-c">>))
    after
        case Previous of {ok, V} -> application:set_env(imboy, garage, V); undefined -> application:unset_env(imboy, garage) end,
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.
