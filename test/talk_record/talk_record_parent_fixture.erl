%% 合成父记录：兼容完整生产 schema 与早期隔离最小 schema。
%% 只过滤固定白名单中的不存在列，不更改约束、默认值或业务资格。
-module(talk_record_parent_fixture).
-export([user/3, organization/5, workspace/5]).

user(C, Id, Overrides) ->
    Allowed = [<<"nickname">>, <<"account">>, <<"status">>, <<"account_type">>],
    true = lists:all(fun(K) -> lists:member(K, Allowed) end, maps:keys(Overrides)),
    Defaults = #{
        <<"id">> => Id,
        <<"password">> => <<"synthetic-unusable-password">>,
        <<"account">> => <<"talk-fixture-", (integer_to_binary(Id))/binary>>,
        <<"reg_ip">> => <<"127.0.0.1">>,
        <<"reg_cosv">> => <<"{}">>
    },
    insert(C, <<"user">>, maps:merge(Defaults, Overrides)).

organization(C, Id, Name, Owner, Status) ->
    insert(C, <<"organization">>, #{
        <<"id">> => Id,
        <<"name">> => Name,
        <<"owner_id">> => Owner,
        <<"status">> => Status
    }).

workspace(C, Id, Org, Name, Owner) ->
    insert(C, <<"workspace">>, #{
        <<"id">> => Id,
        <<"organization_id">> => Org,
        <<"name">> => Name,
        <<"owner_id">> => Owner,
        <<"status">> => <<"active">>
    }).

insert(C, Table, Values) ->
    {ok, Rows} = elib_pg:query(
        C,
        <<
            "SELECT column_name FROM information_schema.columns"
            " WHERE table_schema=current_schema() AND table_name=$1"
        >>,
        [Table]
    ),
    Existing = [maps:get(<<"column_name">>, R) || R <- Rows],
    Pairs = lists:sort([{K, V} || {K, V} <- maps:to_list(Values), lists:member(K, Existing)]),
    true = Pairs =/= [],
    Columns = lists:join(<<",">>, [[<<"\"">>, K, <<"\"">>] || {K, _} <- Pairs]),
    Params = lists:join(<<",">>, [
        [<<"$">>, integer_to_binary(N)]
     || N <- lists:seq(1, length(Pairs))
    ]),
    Sql = iolist_to_binary([
        <<"INSERT INTO \"">>,
        Table,
        <<"\" (">>,
        Columns,
        <<") VALUES (">>,
        Params,
        <<")">>
    ]),
    {ok, _} = elib_pg:query(C, Sql, [V || {_, V} <- Pairs]),
    ok.
