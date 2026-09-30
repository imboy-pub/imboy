-module(group_file_scope_tests).
-include_lib("eunit/include/eunit.hrl").

revoked_parent_denies_all_file_operations_test_() ->
    [
        {atom_to_list(Op), fun() -> denied(Op) end}
     || Op <- [list, search, categories, download, delete, upload]
    ].

denied(Op) ->
    meck:new(group_ds, [non_strict, no_link]),
    meck:expect(group_ds, is_member, fun(_, _) -> true end),
    meck:new(attachment_ds, [non_strict, no_link]),
    meck:expect(attachment_ds, authorize_group_scope, fun(11, 1) -> false end),
    meck:new(group_file_repo, [non_strict, no_link]),
    meck:expect(group_file_repo, find_by_id, fun(9) ->
        #{
            <<"id">> => 9,
            <<"group_id">> => 11,
            <<"uploader_id">> => 1,
            <<"status">> => 1,
            <<"file_url">> => <<"private-file">>
        }
    end),
    meck:expect(group_file_repo, list_by_group, fun(_, _, _, _) -> {ok, []} end),
    meck:expect(group_file_repo, search_by_name, fun(_, _, _, _) -> {ok, []} end),
    meck:expect(group_file_repo, category_stats, fun(_) -> {ok, []} end),
    meck:expect(group_file_repo, soft_delete_tx, fun(_, _) -> {ok, 1} end),
    meck:new(workspace_guard, [non_strict, no_link]),
    meck:expect(workspace_guard, ensure_writable, fun(_) -> ok end),
    meck:expect(workspace_guard, write_tx, fun(_, F) -> F(conn) end),
    meck:expect(workspace_guard, write_tx_or_skip, fun(_, _) -> ok end),
    meck:new(elib_oss, [non_strict, no_link]),
    meck:expect(elib_oss, validate_file_type, fun(_) -> true end),
    meck:expect(elib_oss, put_object, fun(_, _, _, _) -> {error, must_not_upload} end),
    try
        Result =
            case Op of
                list -> group_file_ds:list_files(11, 1, 1, 10);
                search -> group_file_ds:search_files(11, <<"a">>, 1, 10, 1);
                categories -> group_file_logic:get_categories(<<"11">>, 1);
                download -> group_file_ds:download_file(9, 1);
                delete -> group_file_ds:delete_file(9, 1);
                upload -> group_file_ds:upload_file(11, 1, <<"a.txt">>, <<0>>, <<"text/plain">>)
            end,
        ?assertEqual({error, not_member}, Result),
        ?assertEqual(0, meck:num_calls(group_file_repo, list_by_group, 4)),
        ?assertEqual(0, meck:num_calls(group_file_repo, search_by_name, 4)),
        ?assertEqual(0, meck:num_calls(group_file_repo, category_stats, 1)),
        ?assertEqual(0, meck:num_calls(group_file_repo, soft_delete_tx, 2)),
        ?assertEqual(0, meck:num_calls(elib_oss, put_object, 4))
    after
        meck:unload()
    end.
