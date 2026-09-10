-module(bot_webhook_delivery_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

reencrypt_all_reports_failed_bot_ids_without_secrets_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'list_encrypted_bot_ids', 0, fun() -> {ok, [11, 12, 13]} end},
                {'get_verify_token', 1, fun
                    (12) -> {error, authentication_failed};
                    (_) -> {ok, <<"secret-never-returned">>}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, #{processed => 3, failed_bot_ids => [12]}},
                bot_webhook_delivery_logic:reencrypt_all()
            )
        end
    ).

reencrypt_all_passes_when_every_cipher_is_current_or_migrated_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'list_encrypted_bot_ids', 0, fun() -> {ok, [21, 22]} end},
                {'get_verify_token', 1, fun(_) -> {ok, <<"secret-never-returned">>} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {ok, #{processed => 2, failed_bot_ids => []}},
                bot_webhook_delivery_logic:reencrypt_all()
            )
        end
    ).

reencrypt_all_reports_list_query_failure_test_() ->
    ?WITH_MECKS(
        [
            {bot_repo, [
                {'list_encrypted_bot_ids', 0, fun() -> {error, no_connection} end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, {list_failed, no_connection}},
                bot_webhook_delivery_logic:reencrypt_all()
            )
        end
    ).
