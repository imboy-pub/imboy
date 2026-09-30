-module(enterprise_oa_context_binding_tests).
-include_lib("eunit/include/eunit.hrl").

context_binding_test() ->
    meck:new(elib_pg, [non_strict, no_link]),
    meck:new(organization_member_repo, [non_strict, no_link]),
    try
        Rows = [app(101, 201), app(102, 202)],
        meck:expect(elib_pg, query, fun(conn, Sql, _Args) ->
            case binary:match(Sql, <<"FROM enterprise_application">>) of
                nomatch ->
                    case binary:match(Sql, <<"FROM organization WHERE">>) of
                        nomatch -> {ok, []};
                        _ -> {ok, [#{<<"status">> => <<"active">>}]}
                    end;
                _ ->
                    {ok, Rows}
            end
        end),
        meck:expect(
            organization_member_repo,
            find_active_tx,
            fun(conn, _, 301, <<"user_id">>) -> {ok, #{}} end
        ),
        Params = #{
            <<"application_key">> => <<"shared-oa-key">>,
            <<"redirect_uri">> => <<"https://oa.example.com/sso">>,
            <<"nonce">> => <<"nonce_0123456789abcdef">>
        },
        %% 未声明企业时保留旧版多义拒绝；选择企业后只校验该企业成员关系。
        ?assertMatch({error, {404, _}}, issue(Params)),
        ?assertMatch({error, {403, _}}, issue(Params#{<<"organization_id">> => 101})),
        ?assertMatch({error, {403, _}}, issue(Params#{<<"organization_id">> => 102})),
        ?assertMatch({error, {404, _}}, issue(Params#{<<"organization_id">> => 103})),
        lists:foreach(
            fun(Value) ->
                ?assertMatch({error, {400, _}}, issue(Params#{<<"organization_id">> => Value}))
            end,
            [0, -1, null, <<"101">>, 1.5, 9223372036854775808]
        ),
        %% 所选企业没有应用时必须拒绝，不能降级到另一个有权企业。
        meck:expect(elib_pg, query, fun(conn, _, _) -> {ok, [app(101, 201)]} end),
        ?assertMatch({error, {404, _}}, issue(Params#{<<"organization_id">> => 102})),
        ?assert(meck:validate(elib_pg)),
        ?assert(meck:validate(organization_member_repo))
    after
        meck:unload(organization_member_repo),
        meck:unload(elib_pg)
    end.

issue(Params) -> enterprise_oa_sso_logic:issue_code_tx(conn, 301, Params).

app(OrgId, AppId) ->
    #{
        <<"organization_id">> => OrgId,
        <<"id">> => AppId,
        <<"status">> => <<"active">>
    }.
