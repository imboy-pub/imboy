-module(channel_workspace_read_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

workspace_detail_refuses_revoked_or_unavailable_access_test_() ->
    [read_denied(Entry, Code) || Entry <- [id, custom_id], Code <- [403, 503]].

read_denied(Entry, Code) ->
    Channel = #{<<"id">> => 42, <<"scope">> => <<"workspace">>},
    Error = {error, {Code, <<"workspace access denied">>}},
    ?WITH_MECKS(
        [
            {channel_ds, [
                {'find_by_id_with_price', 1, fun(42) -> Channel end},
                {'find_by_custom_id', 1, fun(<<"synthetic">>) -> Channel end}
            ]},
            {workspace_resolver, [
                {'ensure_channel_member_access', 2, fun(1001, 42) -> Error end}
            ]}
        ],
        fun() ->
            Result =
                case Entry of
                    id ->
                        channel_logic_message:get_channel(<<"42">>, 1001);
                    custom_id ->
                        channel_logic_message:get_channel_by_custom_id(<<"synthetic">>, 1001)
                end,
            ?assertEqual(Error, Result),
            ?assertEqual(
                1, meck:num_calls(workspace_resolver, ensure_channel_member_access, [1001, 42])
            )
        end
    ).
