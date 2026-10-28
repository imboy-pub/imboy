-module(channel_legacy_mark_read_watermark_flag_tests).
-include_lib("eunit/include/eunit.hrl").

-define(FLAG, channel_legacy_mark_read_watermark).
-define(CH, 882095).
-define(UID, 882092).

mark_as_read_flag_test_() ->
    {inorder, [
        {"unset flag keeps legacy contract", fun() ->
            flag_case(undefined, {ok, 1}, {ok, 7}, 0, 1, [])
        end},
        {"OFF keeps legacy contract", fun() ->
            flag_case(false, {ok, 1}, {ok, 7}, 0, 1, [])
        end},
        {"ON reaches adapter through real config_ds", fun() ->
            flag_case(true, {ok, 1}, {ok, 7}, 1, 1, [])
        end},
        {"archived writes skip without notification", fun() ->
            flag_case(true, skipped, skipped, 1, 0, [])
        end},
        {"OFF also keeps skipped contract", fun() ->
            flag_case(false, skipped, skipped, 0, 0, [])
        end},
        {"missing subscription is silent", fun() ->
            flag_case(true, {ok, 0}, {error, not_subscribed}, 1, 1, [])
        end},
        {"adapter failure logs and preserves legacy success", fun() ->
            flag_case(true, {ok, 1}, {error, injected_advance}, 1, 1, [
                [
                    "channel_legacy_mark_read_view_advance_failed",
                    ?CH,
                    ?UID,
                    injected_advance
                ]
            ])
        end},
        {"clear failure logs without advancing or notifying", fun() ->
            flag_case(true, {error, injected_clear}, {ok, 7}, 0, 0, [
                ["channel_clear_unread_failed", ?CH, ?UID, injected_clear]
            ])
        end}
    ]}.

flag_case(Flag, ClearResult, AdvanceResult, Advances, Notifications, Logs) ->
    SavedFlag = application:get_env(imboy, ?FLAG),
    Modules = [
        channel_logic_common,
        channel_logic_notify,
        channel_subscription_ds,
        channel_subscription_repo,
        elib_log
    ],
    try
        lists:foreach(fun(Module) -> meck:new(Module, [no_link]) end, Modules),
        set_flag(Flag),
        mock_entry(ClearResult, AdvanceResult),
        ?assertEqual(ok, channel_logic:mark_as_read(?UID, <<"882095">>, <<"m">>)),
        ?assertEqual(
            1,
            meck:num_calls(
                channel_logic_common,
                ensure_channel_content_access,
                [?UID, ?CH]
            )
        ),
        ?assertEqual(
            1,
            meck:num_calls(
                channel_subscription_ds,
                clear_unread,
                [?CH, ?UID]
            )
        ),
        ?assertEqual(
            Advances,
            meck:num_calls(
                channel_subscription_repo,
                advance_view_to_current,
                [?CH, ?UID]
            )
        ),
        ?assertEqual(
            Notifications,
            meck:num_calls(
                channel_logic_notify,
                notify_channel_unread_count,
                [?CH, ?UID, 0]
            )
        ),
        ?assertEqual(Logs, [
            Message
         || {_Pid, {elib_log, internal_log, [error, Message, channel_logic_message, _Line]}, ok} <-
                meck:history(elib_log)
        ]),
        lists:foreach(fun(Module) -> ?assert(meck:validate(Module)) end, Modules)
    after
        lists:foreach(
            fun(Module) -> meck:unload(Module) end,
            [Module || Module <- Modules, lists:member(Module, meck:mocked())]
        ),
        restore_flag(SavedFlag)
    end.

mock_entry(ClearResult, AdvanceResult) ->
    meck:expect(
        channel_logic_common,
        resolve_channel_id,
        fun(<<"882095">>) -> ?CH end
    ),
    meck:expect(
        channel_logic_common,
        ensure_channel_content_access,
        fun(?UID, ?CH) -> ok end
    ),
    meck:expect(
        channel_subscription_ds,
        clear_unread,
        fun(?CH, ?UID) -> ClearResult end
    ),
    meck:expect(
        channel_subscription_repo,
        advance_view_to_current,
        fun(?CH, ?UID) -> AdvanceResult end
    ),
    meck:expect(
        channel_logic_notify,
        notify_channel_unread_count,
        fun(?CH, ?UID, 0) -> ok end
    ),
    meck:expect(
        elib_log,
        internal_log,
        fun(error, _Message, channel_logic_message, _Line) -> ok end
    ).

set_flag(undefined) -> application:unset_env(imboy, ?FLAG);
set_flag(Value) -> application:set_env(imboy, ?FLAG, Value).

restore_flag(undefined) -> application:unset_env(imboy, ?FLAG);
restore_flag({ok, Value}) -> application:set_env(imboy, ?FLAG, Value).
