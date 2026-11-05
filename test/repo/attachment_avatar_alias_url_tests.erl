-module(attachment_avatar_alias_url_tests).
-include_lib("eunit/include/eunit.hrl").

explicit_avatar_edge_origin_test() ->
    Saved = application:get_env(imboy, avatar_edge_base_url),
    Alias = <<"00000000-0000-4000-8000-000000000001">>,
    try
        application:unset_env(imboy, avatar_edge_base_url),
        ?assertEqual({error, edge_not_configured}, attachment_avatar_alias_repo:public_url(Alias)),
        lists:foreach(
            fun(Base) ->
                application:set_env(imboy, avatar_edge_base_url, Base),
                ?assertEqual(
                    {error, edge_not_configured}, attachment_avatar_alias_repo:public_url(Alias)
                )
            end,
            [
                <<"https://other.example/path">>,
                <<"https://a.example/?ticket=x">>,
                <<"https://user@a.example">>
            ]
        ),
        application:set_env(imboy, avatar_edge_base_url, <<"https://self.example/">>),
        ?assertEqual(
            {ok, <<"https://self.example/_avatar/", Alias/binary>>},
            attachment_avatar_alias_repo:public_url(Alias)
        )
    after
        case Saved of
            {ok, Old} -> application:set_env(imboy, avatar_edge_base_url, Old);
            undefined -> application:unset_env(imboy, avatar_edge_base_url)
        end
    end.
