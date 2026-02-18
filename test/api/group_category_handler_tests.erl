-module(group_category_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%%   EUnit Tests for group_category_handler
%% ===================================================================

%% @doc 测试模块是否可以加载
module_loads_test() ->
    ?assertNotEqual(undefined, whereis(group_category_handler)).

%% @doc 测试 init 函数的基本功能
init_test() ->
    %% 创建一个模拟的 Cowboy 请求对象
    MockReq = #{
        bindings => #{},
        headers => #{},
        body => <<>>,
        method => <<"POST">>,
        path => <<"/v1/group/category/create">>
    },

    %% 创建模拟的状态
    MockState = #{
        action => create,
        current_uid => 12345
    },

    %% 测试 init 函数（不抛出错误即为通过）
    try
        {ok, _Req, _State} = group_category_handler:init(MockReq, MockState),
        ?assert(true)
    catch
        _Error:_Reason ->
            ?debugFmt("init 测试失败: ~p: ~p~n", [_Error, _Reason]),
            ?assert(false)
    end.

%% @doc 测试所有 action 类型
actions_test() ->
    ValidActions = [create, list, rename, delete, move_group, sort],

    lists:foreach(fun(Action) ->
        MockReq = #{
            bindings => #{},
            headers => #{},
            body => <<>>,
            method => <<"POST">>,
            path => <<"/v1/group/category/", (atom_to_binary(Action))/binary>>
        },
        MockState = #{
            action => Action,
            current_uid => 12345
        },

        try
            {ok, _Req, _State} = group_category_handler:init(MockReq, MockState),
            ?assert(true)
        catch
            _Error:_Reason ->
                ?debugFmt("action ~p 测试失败: ~p: ~p~n", [Action, _Error, _Reason]),
                ?assert(false)
        end
    end, ValidActions).

%% @doc 测试无效的 action
invalid_action_test() ->
    MockReq = #{
        bindings => #{},
        headers => #{},
        body => <<>>,
        method => <<"POST">>,
        path => <<"/v1/group/category/invalid">>
    },
    MockState = #{
        action => invalid_action,
        current_uid => 12345
    },

    try
        {ok, ReturnedReq, _ReturnedState} = group_category_handler:init(MockReq, MockState),
        %% 无效的 action 应该返回原始请求
        ?assertEqual(MockReq, ReturnedReq)
    catch
        _Error:_Reason ->
            ?debugFmt("无效 action 测试失败: ~p: ~p~n", [_Error, _Reason]),
            ?assert(false)
    end.
