-module(w09_client_get_tests).
-include_lib("eunit/include/eunit.hrl").

client_get_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [fun authorized_public/0, fun invalid_denied/0, fun drift_denied/0]}.
setup() ->
    meck:new(attachment_ds, [non_strict, no_link]),
    meck:new(attachment_storage, [non_strict, no_link]),
    meck:expect(attachment_ds, find_by_path, fun(_) ->
        {ok, #{<<"storage_driver">> => <<"garage">>, <<"scope">> => <<"private">>,
               <<"creator_user_id">> => 7, <<"status">> => 1}}
    end), ok.
cleanup(_) -> meck:unload([attachment_ds, attachment_storage]).
location(E) ->
    C = #{endpoint => <<"http://internal.test:3900">>, public_endpoint => E,
          access_key => <<"ak">>, secret_key => <<"sk">>},
    meck:expect(attachment_storage, frozen_location, fun(_, _) ->
        {ok, #{driver => garage, container => <<"b">>, key => <<"physical">>, garage_config => C}}
    end).
authorized_public() ->
    location(<<"https://public.test:19484/s3">>),
    {ok, Url} = attach_logic:view_url(7, <<"logical">>),
    ?assertMatch({0, _}, binary:match(Url, <<"https://public.test:19484/s3/b/physical?">>)),
    ?assertEqual({error, forbidden}, attach_logic:view_url(8, <<"logical">>)).
invalid_denied() ->
    lists:foreach(fun(E) -> location(E), ?assertEqual({error, forbidden}, attach_logic:view_url(7, <<"logical">>)) end,
                  [<<>>, <<"ftp://host">>, <<"http://user@host">>, <<"http://host:80">>,
                   <<"https://host:443">>, <<"http://host:65536">>, <<"http://host/%2e">>,
                   <<"http://host?q">>, <<"http://host#f">>]).
drift_denied() ->
    meck:expect(attachment_storage, frozen_location, fun(_, _) -> {error, location_changed} end),
    ?assertEqual({error, forbidden}, attach_logic:view_url(7, <<"logical">>)).
