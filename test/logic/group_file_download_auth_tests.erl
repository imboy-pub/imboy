-module(group_file_download_auth_tests).
-include_lib("eunit/include/eunit.hrl").

download_uses_attachment_authorization_test_() ->
    [
        {atom_to_list(Mode), fun() -> verify(Mode) end}
     || Mode <- [allowed, denied]
    ].

verify(Mode) ->
    meck:new(group_file_ds, [non_strict, no_link]),
    meck:expect(group_file_ds, download_file, fun(9, 1) -> {ok, <<"bound/key">>} end),
    meck:new(attach_logic, [non_strict, no_link]),
    Expected =
        case Mode of
            allowed -> {ok, <<"https://storage.example.com/signed">>};
            denied -> {error, forbidden}
        end,
    meck:expect(attach_logic, view_url, fun(1, <<"bound/key">>) -> Expected end),
    try
        ?assertEqual(Expected, group_file_logic:download(9, 1)),
        ?assertEqual(1, meck:num_calls(attach_logic, view_url, 2))
    after
        meck:unload()
    end.
