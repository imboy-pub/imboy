-module(group_tag_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

add_success_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() ->
                <<"2026-03-16T00:00:00Z">>
            end}
        ]},
        {group_tag_repo, [
            {'exists', 2, fun(1, <<"tag-a">>) ->
                false
            end},
            {'add', 2, fun(undefined, Data) ->
                ?assertEqual(1, maps:get(group_id, Data)),
                ?assertEqual(<<"tag-a">>, maps:get(tag_name, Data)),
                {ok, 7}
            end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 7}, group_tag_ds:add(1, 100, <<"tag-a">>))
    end).

add_existing_tag_returns_error_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'exists', 2, fun(_GroupId, _TagName) ->
            true
        end}
    ], fun() ->
        ?assertEqual({error, <<"标签已存在"/utf8>>}, group_tag_ds:add(1, 100, <<"tag-a">>))
    end).

add_long_tag_name_returns_error_test() ->
    LongTag = list_to_binary(lists:duplicate(51, $x)),
    ?assertEqual({error, <<"标签名过长，最多50个字符"/utf8>>}, group_tag_ds:add(1, 100, LongTag)).

remove_success_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'delete', 2, fun(1, <<"tag-a">>) ->
            {ok, 1}
        end}
    ], fun() ->
        ?assertEqual(ok, group_tag_ds:remove(1, 100, <<"tag-a">>))
    end).

list_success_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'list_by_group', 2, fun(1, <<"id, tag_name, created_by, created_at">>) ->
            {ok, [#{<<"id">> => 1, <<"tag_name">> => <<"tag-a">>}]}
        end}
    ], fun() ->
        ?assertEqual({ok, [#{<<"id">> => 1, <<"tag_name">> => <<"tag-a">>}]}, group_tag_ds:list(1))
    end).

search_empty_tag_name_returns_error_test() ->
    ?assertEqual({error, <<"标签名不能为空"/utf8>>}, group_tag_ds:search(<<>>)).

hot_tags_invalid_limit_returns_empty_list_test() ->
    ?assertEqual({ok, []}, group_tag_ds:hot_tags(0)).

count_repo_error_returns_zero_test_() ->
    ?WITH_MECK(group_tag_repo, [
        {'count_by_group', 1, fun(_GroupId) ->
            {error, db_error}
        end}
    ], fun() ->
        ?assertEqual({ok, 0}, group_tag_ds:count(1))
    end).
