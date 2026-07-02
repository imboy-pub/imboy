-module(user_tag_relation_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% @doc user_tag_relation_logic 模块测试

%% Helper: setup multiple meck modules with [no_link] (no passthrough).
setup_mecks([]) ->
    ok;
setup_mecks([{Module, Expects} | Rest]) ->
    meck:new(Module, [no_link]),
    lists:foreach(
        fun({Func, Arity, Fun}) ->
            meck:expect(Module, Func, Arity, Fun)
        end,
        Expects
    ),
    setup_mecks(Rest).

cleanup_mecks([]) ->
    ok;
cleanup_mecks([{Module, _} | Rest]) ->
    catch meck:unload(Module),
    cleanup_mecks(Rest).

set_tag_success_test_() ->
    Mocks = [
        {elib_pg, [
            %% First query: count check (returns 0 => no duplicate)
            %% Second query: SELECT to_user_id (returns empty list)
            {'query', 2, fun(Sql, _Params) ->
                case binary:match(Sql, <<"count">>) of
                    nomatch ->
                        {ok, []};
                    _ ->
                        {ok, [#{<<"count">> => 0}]}
                end
            end},
            {'with_tx', 1, fun(TxFun) ->
                TxFun(dummy_conn),
                ok
            end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tab) -> <<"public.", Tab/binary>> end}
        ]},
        {ec_cnv, [
            {'to_integer', 1, fun
                (I) when is_integer(I) -> I;
                (B) when is_binary(B) ->
                    try
                        binary_to_integer(B)
                    catch
                        _:_ -> 0
                    end
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]},
        {user_tag_ds, [
            {'change_scene_tag', 5, fun(_Conn, _Scene, _Uid, _ObjId, _Tags) -> ok end}
        ]},
        {user_tag_relation_ds, [
            {'save_user_tag_relation', 6, fun(_Conn, _Scene, _Uid, _TagId, _ObjId, _CreatedAt) ->
                ok
            end},
            {'update_tag', 5, fun(_Conn, _TagId, _TagName, _Uid, _CreatedAt) -> ok end},
            {'remove_user_tag_relation', 5, fun(_Conn, _Scene, _Uid, _TagId, _ObjId) -> ok end},
            {'replace_object_tag', 6, fun(_Conn, _Scene, _Uid, _ObjId, _TagName, _Tags) -> ok end},
            {'flush_subtitle', 1, fun(_TagId) -> ok end}
        ]}
    ],
    {setup, fun() -> setup_mecks(Mocks) end, fun(_) -> cleanup_mecks(Mocks) end, fun(_) ->
        [
            fun() ->
                Result = user_tag_relation_logic:set(100, 1, [1, 2], 1, <<"标签1"/utf8>>),
                ?assertEqual(ok, Result)
            end
        ]
    end}.

remove_tag_success_test_() ->
    Mocks = [
        {elib_pg, [
            {'one', 2, fun(_Sql, _Params) -> {ok, #{<<"name">> => <<"tag1">>}} end},
            {'with_tx', 1, fun(TxFun) ->
                TxFun(dummy_conn),
                ok
            end},
            {'query', 3, fun(_Conn, _Sql, _Params) -> {ok, []} end},
            {'execute', 3, fun(_Conn, _Sql, _Params) -> {ok, 1} end}
        ]},
        {elib_pg_sql, [
            {'public_tablename', 1, fun(Tab) -> <<"public.", Tab/binary>> end}
        ]},
        {user_tag_relation_ds, [
            {'remove_user_tag_relation', 5, fun(_Conn, _Scene, _Uid, _TagId, _ObjId) -> ok end},
            {'replace_object_tag', 6, fun(_Conn, _Scene, _Uid, _ObjId, _TagName, _Tags) -> ok end},
            {'flush_subtitle', 1, fun(_TagId) -> ok end},
            {'tablename', 0, fun() -> <<"public.user_tag_relation">> end}
        ]}
    ],
    {setup, fun() -> setup_mecks(Mocks) end, fun(_) -> cleanup_mecks(Mocks) end, fun(_) ->
        [
            fun() ->
                Result = user_tag_relation_logic:remove(100, 1, 1, 1),
                ?assertEqual(ok, Result)
            end
        ]
    end}.
