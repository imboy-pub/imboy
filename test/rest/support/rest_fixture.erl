-module(rest_fixture).

-export([create_user/1]).

-spec create_user(map()) -> map().
create_user(Overrides) ->
    Password = maps:get(password, Overrides, <<"RestLogin123!">>),
    Defaults = #{
        account => <<"rest-login-user">>,
        email => <<"rest-login-user@example.invalid">>,
        mobile => <<>>,
        nickname => <<"REST Login Fixture">>,
        password => elib_password:generate(Password)
    },
    Data = maps:merge(Defaults, maps:remove(password, Overrides)),
    {ok, Uid} = user_repo:create(Data),
    Data#{uid => Uid, plain_password => Password}.
