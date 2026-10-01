-module(attachment_download_ticket_tests).
-include_lib("eunit/include/eunit.hrl").
-define(SECRET, <<"synthetic-download-secret-only">>).
-define(KEY, <<"u995011/g995301/synthetic.txt">>).

bound_ticket_test() ->
    {ok, Ticket} = attachment_download_ticket:issue(995011, ?KEY, ?SECRET),
    ?assertEqual({ok, 995011, ?KEY}, attachment_download_ticket:verify(Ticket, ?SECRET)),
    ?assertMatch({error, _}, imboy_jwt:verify(Ticket, ?SECRET)),
    ?assertEqual({error, invalid}, attachment_download_ticket:verify(Ticket, <<"wrong">>)).

login_token_is_not_download_ticket_test() ->
    Token = imboy_jwt:sign(
        #{
            <<"uid">> => 995011,
            <<"sub">> => <<"tk">>,
            <<"exp">> => erlang:system_time(second) + 600
        },
        ?SECRET
    ),
    ?assertEqual({error, invalid}, attachment_download_ticket:verify(Token, ?SECRET)).

malformed_ticket_test() ->
    lists:foreach(
        fun(Ticket) ->
            ?assertEqual({error, invalid}, attachment_download_ticket:verify(Ticket, ?SECRET))
        end,
        [<<>>, <<"malformed">>, binary:copy(<<"x">>, 8193), undefined]
    ),
    ?assertEqual({error, invalid}, attachment_download_ticket:issue(0, ?KEY, ?SECRET)),
    ?assertEqual({error, invalid}, attachment_download_ticket:issue(1, <<>>, ?SECRET)),
    ?assertEqual({error, invalid}, attachment_download_ticket:issue(1, ?KEY, <<>>)).

claims_are_bounded_test() ->
    Now = erlang:system_time(second),
    Base = #{
        <<"sub">> => <<"attachment_download">>,
        <<"uid">> => 995011,
        <<"object_key">> => ?KEY,
        <<"iat">> => Now,
        <<"exp">> => Now + 600
    },
    Key = crypto:mac(hmac, sha256, ?SECRET, <<"imboy:attachment-download:v1">>),
    lists:foreach(
        fun(Changes) ->
            Token = imboy_jwt:sign(maps:merge(Base, Changes), Key),
            ?assertEqual({error, invalid}, attachment_download_ticket:verify(Token, ?SECRET))
        end,
        [
            #{<<"exp">> => Now},
            #{<<"iat">> => Now + 3600},
            #{<<"exp">> => Now + 601},
            #{<<"sub">> => <<"tk">>},
            #{<<"uid">> => 0},
            #{<<"object_key">> => <<>>}
        ]
    ).
