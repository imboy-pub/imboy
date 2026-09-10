-module(bot_webhook_delivery_sender_tests).

-include_lib("eunit/include/eunit.hrl").

tls_options_verify_chain_and_hostname_test() ->
    Host = <<"hooks.example.com">>,
    Opts = bot_webhook_delivery_sender:ssl_opts(Host),
    ?assertEqual({verify, verify_peer}, lists:keyfind(verify, 1, Opts)),
    ?assertEqual(
        {server_name_indication, "hooks.example.com"},
        lists:keyfind(server_name_indication, 1, Opts)
    ),
    {customize_hostname_check, HostnameOpts} =
        lists:keyfind(customize_hostname_check, 1, Opts),
    {match_fun, MatchFun} = lists:keyfind(match_fun, 1, HostnameOpts),
    ?assert(is_function(MatchFun)).
