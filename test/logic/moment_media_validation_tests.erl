-module(moment_media_validation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

malformed_media_never_reaches_write_or_moderation_test_() ->
    Invalid = [
        #{},
        #{<<"url">> => <<>>},
        #{<<"url">> => <<"  ">>},
        #{<<"url">> => <<255>>},
        #{<<"url">> => <<226, 130>>},
        #{<<"url">> => null},
        #{<<"url">> => 42},
        #{<<"url">> => <<"image">>, <<"cover_url">> => 42},
        [#{<<"url">> => <<"image">>}, bad_item],
        42,
        <<"not-json">>,
        <<"{}">>
    ],
    [
        ?WITH_MECKS(
            [
                {moment_ds, [{'create_post', 2, fun(_, _) -> error(unexpected_write) end}]},
                {moderation_policy, [{'inspect', 2, fun(_, _) -> error(unexpected_moderation) end}]}
            ],
            fun() ->
                ?assertEqual(
                    {error, <<"媒体对象路径无效"/utf8>>},
                    moment_logic:create_post(1001, #{
                        <<"content">> => <<"post">>, <<"media">> => Value
                    })
                ),
                ?assertEqual(0, meck:num_calls(moment_ds, create_post, '_')),
                ?assertEqual(0, meck:num_calls(moderation_policy, inspect, '_'))
            end
        )
     || Value <- Invalid
    ].

valid_image_video_and_json_array_remain_supported_test_() ->
    Image = #{<<"type">> => <<"image">>, <<"url">> => <<"image">>},
    Video = #{<<"type">> => <<"video">>, <<"url">> => <<"video">>, <<"cover_url">> => <<>>},
    Values = [{[Image], [Image]}, {[Video], [Video]}, {jsone:encode([Image]), [Image]}],
    [
        ?WITH_MECKS(
            [
                {moderation_policy, [{'inspect', 2, fun(moment_post, _) -> clean end}]},
                {moment_ds, [
                    {'create_post', 2, fun(1001, Data) ->
                        ?assertEqual(Expected, maps:get(media, Data)),
                        {ok, 555}
                    end},
                    {'get_post', 1, fun(555) ->
                        #{<<"id">> => 555, <<"author_uid">> => 1001, <<"status">> => 1}
                    end}
                ]},
                {moment_logic_notify, [{'notify_post_created', 2, fun(1001, 555) -> ok end}]}
            ],
            fun() ->
                ?assertMatch(
                    {ok, _},
                    moment_logic:create_post(1001, #{
                        <<"content">> => <<"post">>, <<"media">> => Value
                    })
                )
            end
        )
     || {Value, Expected} <- Values
    ].
