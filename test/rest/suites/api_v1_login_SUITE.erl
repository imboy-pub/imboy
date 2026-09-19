-module(api_v1_login_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    login_001_valid_credentials/1,
    login_002_wrong_password/1,
    login_003_unknown_account/1,
    login_004_malformed_json/1
]).

-define(PATH, <<"/api/v1/passport/login">>).

all() ->
    [
        login_001_valid_credentials,
        login_002_wrong_password,
        login_003_unknown_account,
        login_004_malformed_json
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(imboy),
    Port = ranch:get_port(imboy_listener),
    User = rest_fixture:create_user(#{}),
    [{http_port, Port}, {user, User} | Config].

end_per_suite(_Config) ->
    ok = application:stop(imboy).

login_001_valid_credentials(Config) ->
    User = ?config(user, Config),
    Request = login_request(
        maps:get(account, User), maps:get(plain_password, User), <<"rest-login-device-001">>
    ),
    Response = post(Config, Request),
    verify(
        <<"LOGIN-001">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 0},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 0,
                    <<"payload">> => #{
                        <<"uid">> => maps:get(uid, User),
                        <<"account">> => maps:get(account, User)
                    }
                },
                Resp
            ),
            rest_assert:predicate(
                [<<"payload">>, <<"token">>], fun nonempty_binary/1, Resp
            ),
            rest_assert:predicate(
                [<<"payload">>, <<"refreshtoken">>], fun nonempty_binary/1, Resp
            )
        end
    ).

login_002_wrong_password(Config) ->
    User = ?config(user, Config),
    Request = login_request(
        maps:get(account, User), <<"WrongPassword123!">>, <<"rest-login-device-002">>
    ),
    Response = post(Config, Request),
    verify(
        <<"LOGIN-002">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 1},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 1, <<"payload">> => #{}}, Resp),
            rest_assert:predicate([<<"msg">>], fun nonempty_binary/1, Resp)
        end
    ).

login_003_unknown_account(Config) ->
    Request = login_request(
        <<"rest-login-user-missing">>, <<"RestLogin123!">>, <<"rest-login-device-003">>
    ),
    Response = post(Config, Request),
    verify(
        <<"LOGIN-003">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 1, <<"msg">> => <<"账号不存在"/utf8>>},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(
                #{
                    <<"code">> => 1,
                    <<"msg">> => <<"账号不存在"/utf8>>,
                    <<"payload">> => #{}
                },
                Resp
            )
        end
    ).

login_004_malformed_json(Config) ->
    Request = <<"{not-json">>,
    Response = post(Config, Request),
    verify(
        <<"LOGIN-004">>,
        Request,
        Response,
        #{<<"http_status">> => 200, <<"code">> => 1},
        fun(Resp) ->
            common_assertions(Resp),
            rest_assert:json_contains(#{<<"code">> => 1, <<"payload">> => #{}}, Resp)
        end
    ).

login_request(Account, Password, Did) ->
    #{
        <<"type">> => <<"account">>,
        <<"account">> => Account,
        <<"pwd">> => Password,
        <<"rsa_encrypt">> => <<"0">>,
        <<"did">> => Did
    }.

post(Config, Body) ->
    Headers = #{
        <<"cos">> => <<"android">>,
        <<"did">> => maps:get(<<"did">>, Body, <<"rest-login-device-004">>),
        <<"dname">> => <<"REST Golden Suite">>
    },
    rest_client:post(?config(http_port, Config), ?PATH, Body, Headers).

verify(CaseId, Request, Response, Expected, AssertFun) ->
    Meta = #{
        case_id => CaseId,
        api => <<"api_v1_login">>,
        method => <<"POST">>,
        path => ?PATH,
        expected => Expected
    },
    rest_evidence:verify(Meta, Request, Response, AssertFun).

common_assertions(Response) ->
    rest_assert:status(200, Response),
    rest_assert:header_contains(<<"content-type">>, <<"application/json">>, Response),
    rest_assert:predicate([<<"sv_ts">>], fun is_integer/1, Response).

nonempty_binary(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.
