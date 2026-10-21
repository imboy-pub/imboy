%% 频道帖图片归属校验单测（prod 2026-10-20 事故二加固）：
%% images 中的附件 key 必须是本频道的 channel 附件或 public 资源；
%% private/跨频道/不存在/查询故障一律拒发（fail-closed），外链跳过。
-module(channel_logic_message_images_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHANNEL_ID, 99001).
-define(KEY_THIS, <<"u995011/channel/99001/ok.png">>).
-define(KEY_OTHER, <<"u995011/channel/99002/other.png">>).
-define(KEY_PRIVATE, <<"u995011/private/secret.png">>).
-define(KEY_PUBLIC, <<"u995011/public/avatar.png">>).
-define(KEY_GHOST, <<"u995011/channel/99999/ghost.png">>).
-define(EXT_URL, <<"https://cdn.example.com/img.png">>).

setup() ->
    ok = meck:new(attachment_ds, [no_link]),
    meck:expect(attachment_ds, find_by_path, fun
        (?KEY_THIS) -> {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => ?CHANNEL_ID}};
        (?KEY_OTHER) -> {ok, #{<<"scope">> => <<"channel">>, <<"scope_ref">> => 99002}};
        (?KEY_PRIVATE) -> {ok, #{<<"scope">> => <<"private">>, <<"scope_ref">> => <<"">>}};
        (?KEY_PUBLIC) -> {ok, #{<<"scope">> => <<"public">>, <<"scope_ref">> => <<"">>}};
        (?KEY_GHOST) -> {error, db_down}
    end),
    [].

cleanup(_) ->
    meck:unload(attachment_ds).

validate_post_images_test_() ->
    {
        foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun no_images_passes/0,
            fun this_channel_attachment_passes/0,
            fun public_attachment_passes/0,
            fun external_url_skips_validation/0,
            fun legacy_item_without_uri_skips/0,
            fun private_scope_rejected/0,
            fun cross_channel_rejected/0,
            fun not_found_rejected/0,
            fun lookup_failure_fail_closed/0,
            fun first_bad_image_wins_with_index/0
        ]
    }.

no_images_passes() ->
    ?assertEqual(ok, channel_logic_message:validate_post_images(?CHANNEL_ID, #{})),
    ?assertEqual(
        ok,
        channel_logic_message:validate_post_images(?CHANNEL_ID, #{<<"images">> => []})
    ).

this_channel_attachment_passes() ->
    Payload = #{<<"images">> => [#{<<"uri">> => ?KEY_THIS, <<"w">> => 1, <<"h">> => 1}]},
    ?assertEqual(ok, channel_logic_message:validate_post_images(?CHANNEL_ID, Payload)).

public_attachment_passes() ->
    Payload = #{<<"images">> => [#{<<"uri">> => ?KEY_PUBLIC}]},
    ?assertEqual(ok, channel_logic_message:validate_post_images(?CHANNEL_ID, Payload)).

external_url_skips_validation() ->
    Payload = #{<<"images">> => [#{<<"uri">> => ?EXT_URL}]},
    ?assertEqual(ok, channel_logic_message:validate_post_images(?CHANNEL_ID, Payload)),
    ?assertEqual(0, meck:num_calls(attachment_ds, find_by_path, '_')).

legacy_item_without_uri_skips() ->
    Payload = #{<<"images">> => [#{<<"url">> => ?KEY_THIS}]},
    ?assertEqual(ok, channel_logic_message:validate_post_images(?CHANNEL_ID, Payload)).

private_scope_rejected() ->
    Payload = #{<<"images">> => [#{<<"uri">> => ?KEY_PRIVATE}]},
    ?assertMatch(
        {error, ErrBin} when is_binary(ErrBin),
        channel_logic_message:validate_post_images(?CHANNEL_ID, Payload)
    ).

cross_channel_rejected() ->
    Payload = #{<<"images">> => [#{<<"uri">> => ?KEY_OTHER}]},
    ?assertMatch(
        {error, _},
        channel_logic_message:validate_post_images(?CHANNEL_ID, Payload)
    ).

not_found_rejected() ->
    %% key 形态合法但附件表无此行：不是本系统签发的资源，拒发
    Key = <<"u995011/channel/99001/missing.png">>,
    meck:expect(attachment_ds, find_by_path, fun(K) when K =:= Key -> {error, not_found} end),
    Payload = #{<<"images">> => [#{<<"uri">> => Key}]},
    ?assertMatch(
        {error, _},
        channel_logic_message:validate_post_images(?CHANNEL_ID, Payload)
    ).

lookup_failure_fail_closed() ->
    Payload = #{<<"images">> => [#{<<"uri">> => ?KEY_GHOST}]},
    ?assertMatch(
        {error, _},
        channel_logic_message:validate_post_images(?CHANNEL_ID, Payload)
    ).

first_bad_image_wins_with_index() ->
    %% 第 1 张合法、第 2 张 private：错误消息指向第 2 张
    Payload = #{
        <<"images">> => [
            #{<<"uri">> => ?KEY_THIS},
            #{<<"uri">> => ?KEY_PRIVATE}
        ]
    },
    {error, Msg} = channel_logic_message:validate_post_images(?CHANNEL_ID, Payload),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"2">>)).
