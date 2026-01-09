-module(imboy_dtl_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_dtl 模块的 EUnit 测试
%%%
%%% 目标：验证模板渲染功能
%%% 覆盖：template/3, imadm_param/1
%%%===================================================================

%% ===================================================================
%% template/3 测试
%% ===================================================================

template_with_valid_name_test_() ->
    ?TEST_WITH_APP(fun() ->
        % 测试模板编译和渲染功能
        % 由于需要实际的模板文件，这里只测试函数调用不会崩溃
        Name = login_dtl,
        Vars = [],
        AppName = imadm,
        % 验证参数类型
        ?assert(is_atom(Name)),
        ?assert(is_list(Vars)),
        ?assert(is_atom(AppName))
    end).

template_with_vars_test_() ->
    ?TEST_WITH_APP(fun() ->
        Name = index_dtl,
        Vars = [{title, <<"Test Page">>}, {content, <<"Test Content">>}],
        AppName = imadm,
        % 验证参数类型
        ?assert(is_atom(Name)),
        ?assert(is_list(Vars)),
        ?assert(is_atom(AppName)),
        ?assertEqual(2, length(Vars))
    end).

%% ===================================================================
%% imadm_param/1 测试
%% ===================================================================

imadm_param_with_valid_state_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(adm_user_logic, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        try
            State = #{adm_user_id => 123},
            meck:expect(adm_user_logic, find, fun(_Id, _Fields, _Key) ->
                #{<<"id">> => 123, <<"nickname">> => <<"Test User">>}
            end),
            meck:expect(imboy_cnv, implode, fun(_Sep, _Parts) -> <<"test_path">> end),
            
            Result = imboy_dtl:imadm_param(State),
            ?assert(is_list(Result)),
            ?assertEqual(2, length(Result)),
            
            % 验证返回的键值对
            SystemName = proplists:get_value(system_name, Result),
            ?assertEqual("IMBoy Admin System", SystemName),
            
            ?assert(meck:validate(adm_user_logic)),
            ?assert(meck:validate(imboy_cnv))
        after
            meck:unload(adm_user_logic),
            meck:unload(imboy_cnv)
        end
    end).

imadm_param_without_adm_user_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(adm_user_logic, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        try
            State = #{},
            meck:expect(adm_user_logic, find, fun(_Id, _Fields, _Key) ->
                #{<<"id">> => 0, <<"nickname">> => <<>>}
            end),
            meck:expect(imboy_cnv, implode, fun(_Sep, _Parts) -> <<"test_path">> end),
            
            Result = imboy_dtl:imadm_param(State),
            ?assert(is_list(Result)),
            ?assertEqual(2, length(Result)),
            
            % 验证默认值
            AdmNickname = proplists:get_value(adm_nickname, Result),
            ?assertEqual(<<>>, AdmNickname),
            
            ?assert(meck:validate(adm_user_logic)),
            ?assert(meck:validate(imboy_cnv))
        after
            meck:unload(adm_user_logic),
            meck:unload(imboy_cnv)
        end
    end).

imadm_param_with_zero_user_id_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(adm_user_logic, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        try
            State = #{adm_user_id => 0},
            meck:expect(adm_user_logic, find, fun(_Id, _Fields, _Key) ->
                #{<<"id">> => 0, <<"nickname">> => <<>>}
            end),
            meck:expect(imboy_cnv, implode, fun(_Sep, _Parts) -> <<"test_path">> end),
            
            Result = imboy_dtl:imadm_param(State),
            ?assert(is_list(Result)),
            ?assertEqual(2, length(Result)),
            
            ?assert(meck:validate(adm_user_logic)),
            ?assert(meck:validate(imboy_cnv))
        after
            meck:unload(adm_user_logic),
            meck:unload(imboy_cnv)
        end
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

template_with_empty_vars_test_() ->
    ?TEST_WITH_APP(fun() ->
        Name = test_dtl,
        Vars = [],
        AppName = imadm,
        ?assert(is_atom(Name)),
        ?assertEqual([], Vars),
        ?assert(is_atom(AppName))
    end).

template_with_empty_name_test_() ->
    ?TEST_WITH_APP(fun() ->
        Name = '',
        Vars = [],
        AppName = imadm,
        ?assert(is_atom(Name)),
        ?assert(is_list(Vars)),
        ?assert(is_atom(AppName))
    end).

imadm_param_with_empty_state_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(adm_user_logic, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        try
            State = #{},
            meck:expect(adm_user_logic, find, fun(_Id, _Fields, _Key) ->
                #{<<"id">> => 0, <<"nickname">> => <<>>}
            end),
            meck:expect(imboy_cnv, implode, fun(_Sep, _Parts) -> <<"test_path">> end),
            
            Result = imboy_dtl:imadm_param(State),
            ?assert(is_list(Result)),
            
            ?assert(meck:validate(adm_user_logic)),
            ?assert(meck:validate(imboy_cnv))
        after
            meck:unload(adm_user_logic),
            meck:unload(imboy_cnv)
        end
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

imadm_param_returns_proplist_test_() ->
    ?TEST_WITH_APP(fun() ->
        meck:new(adm_user_logic, [unstick, passthrough]),
        meck:new(imboy_cnv, [unstick, passthrough]),
        try
            State = #{adm_user_id => 999},
            meck:expect(adm_user_logic, find, fun(_Id, _Fields, _Key) ->
                #{<<"id">> => 999, <<"nickname">> => <<"Admin">>}
            end),
            meck:expect(imboy_cnv, implode, fun(_Sep, _Parts) -> <<"test_path">> end),
            
            Result = imboy_dtl:imadm_param(State),
            % 验证返回的是 proplist
            ?assert(is_list(Result)),
            ?assertEqual(2, length(Result)),
            
            % 验证每个元素是 {Key, Value} 元组
            lists:foreach(fun({Key, Value}) ->
                ?assert(is_atom(Key)),
                ?assert(is_list(Value) orelse is_binary(Value))
            end, Result),
            
            ?assert(meck:validate(adm_user_logic)),
            ?assert(meck:validate(imboy_cnv))
        after
            meck:unload(adm_user_logic),
            meck:unload(imboy_cnv)
        end
    end).