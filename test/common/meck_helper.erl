-module(meck_helper).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% Meck Mock 管理辅助模块
%%%
%%% 提供：
%%% - 统一的 Mock 创建和管理
%%% - Mock 验证和清理
%%% - 常用 Mock 模板
%%% - 测试数据生成器
%%%
%%% 使用方法：
%%% ?WITH_MECK([Module], Expectations, TestFun)
%%%===================================================================

-export([
    setup_mock/2,
    setup_mock/3,
    cleanup_mock/1,
    cleanup_mocks/1,
    verify_mock/1,
    verify_mock/2,
    verify_called/3,
    verify_called_once/3,
    
    % Mock 模板
    mock_elib_param/1,
    mock_elib_response/1,
    mock_passport_logic/1,
    mock_user_repo/1,
    mock_elib_pg/1,
    
    % 测试数据生成
    test_user/0,
    test_user/1,
    test_group/0,
    test_group/1,
    test_message/0,
    test_message/1,
    test_request/0,
    test_request/1
]).

%% ===================================================================
%% Mock 管理函数
%% ===================================================================

%% @doc 为单个模块设置 Mock
%% @param Module 要 Mock 的模块名
%% @param Expectations 期望函数列表，格式为 [{Function, Arity, Fun}]
setup_mock(Module, Expectations) ->
    % 依次尝试多种策略，优先使用 passthrough+unstick 兼容旧测试。
    % 若模块在当前 code path 下短暂不可加载（如 {error,nofile}），
    % 则自动回退到不依赖 unstick 的纯 mock 策略。
    setup_mock_with_fallback(
        Module,
        Expectations,
        [
            [passthrough, no_link, unstick],
            [no_link, unstick],
            [passthrough, no_link],
            [no_link]
        ],
        []
    ).

%% @doc 为单个模块设置 Mock（带选项）
%% @param Module 要 Mock 的模块名
%% @param Options meck 选项，如 [passthrough, unstick, no_link]
%% @param Expectations 期望函数列表
setup_mock(Module, Options, Expectations) ->
    setup_mock(Module, Options, Expectations, 3).

setup_mock(_Module, _Options, _Expectations, Retries) when Retries =< 0 ->
    {error, too_many_retries};
setup_mock(Module, Options, Expectations, Retries) ->
    % 先尝试清理旧 mock，避免同模块重复 mock 卡住。
    cleanup_mock(Module),
    try
        meck:new(Module, Options),
        lists:foreach(fun({Func, Arity, Fun}) ->
            meck:expect(Module, Func, Arity, normalize_mock_fun(Fun, Arity))
        end, Expectations),
        {ok, Module}
    catch
        error:{already_started, _} ->
            timer:sleep(10),
            setup_mock(Module, Options, Expectations, Retries - 1);
        _:Error:StackTrace ->
            % 捕获并返回详细错误
            {error, {Error, StackTrace}}
    end.

setup_mock_with_fallback(_Module, _Expectations, [], Errors) ->
    {error, {mock_setup_failed, lists:reverse(Errors)}};
setup_mock_with_fallback(Module, Expectations, [Options | Rest], Errors) ->
    case setup_mock(Module, Options, Expectations) of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            setup_mock_with_fallback(
                Module,
                Expectations,
                Rest,
                [{Options, Reason} | Errors]
            )
    end.

%% @doc 清理单个 Mock
cleanup_mock(Module) ->
    % 当前 meck 版本不保证提供 is_loaded/1，直接尝试 unload 并忽略未 mock 错误。
    _ = catch meck:unload(Module),
    % 某些场景 unload 后模块会变为未加载状态，下一次含 unstick 的 mock
    % 可能在 ensure_loaded 处报 {error,nofile}；尽力恢复原模块加载状态。
    _ = catch code:ensure_loaded(Module),
    ok.

%% @doc 批量清理 Mock
cleanup_mocks(Modules) when is_list(Modules) ->
    lists:foreach(fun cleanup_mock/1, Modules);
cleanup_mocks(Module) when is_atom(Module) ->
    cleanup_mock(Module).

%% @doc 验证 Mock 调用
%% @param Module Mock 的模块名
verify_mock(Module) ->
    try
        History = meck:history(Module),
        ?debugFmt("Mock history for ~p: ~p", [Module, History]),
        {ok, History}
    catch
        _:Error ->
            {error, Error}
    end.

normalize_mock_fun(Fun, Arity) ->
    {arity, FunArity} = erlang:fun_info(Fun, arity),
    case FunArity of
        Arity ->
            Fun;
        N when N =:= Arity - 1 ->
            wrap_drop_first_arg(Fun, Arity);
        _ ->
            Fun
    end.

wrap_drop_first_arg(Fun, 1) ->
    fun(_A1) ->
        Fun()
    end;
wrap_drop_first_arg(Fun, 2) ->
    fun(_A1, A2) ->
        Fun(A2)
    end;
wrap_drop_first_arg(Fun, 3) ->
    fun(_A1, A2, A3) ->
        Fun(A2, A3)
    end;
wrap_drop_first_arg(Fun, 4) ->
    fun(_A1, A2, A3, A4) ->
        Fun(A2, A3, A4)
    end;
wrap_drop_first_arg(Fun, 5) ->
    fun(_A1, A2, A3, A4, A5) ->
        Fun(A2, A3, A4, A5)
    end;
wrap_drop_first_arg(Fun, _) ->
    Fun.

%% @doc 验证 Mock 调用（带期望）
%% @param Module Mock 的模块名
%% @param ExpectedCalls 期望调用列表，格式为 [{Function, Arity, MinCalls}]
verify_mock(Module, ExpectedCalls) ->
    try
        lists:foreach(fun({Func, Arity, MinCalls}) ->
            CallCount = meck:num_calls(Module, Func, Arity),
            case CallCount >= MinCalls of
                true -> ok;
                false -> ?assert(false,
                                 io_lib:format("Expected ~p:~p/~p to be called at least ~p times, got ~p",
                                               [Module, Func, Arity, MinCalls, CallCount]))
            end
        end, ExpectedCalls),
        {ok, verified}
    catch
        _:Error ->
            {error, Error}
    end.

%% @doc 验证函数被调用
%% @param Module 模块名
%% @param Function 函数名
%% @param Arity 函数元数
verify_called(Module, Function, Arity) ->
    CallCount = meck:num_calls(Module, Function, Arity),
    case CallCount > 0 of
        true -> ok;
        false -> ?assert(false,
                         io_lib:format("Expected ~p:~p/~p to be called, but was not called",
                                       [Module, Function, Arity]))
    end.

%% @doc 验证函数被恰好调用一次
%% @param Module 模块名
%% @param Function 函数名
%% @param Arity 函数元数
verify_called_once(Module, Function, Arity) ->
    CallCount = meck:num_calls(Module, Function, Arity),
    case CallCount of
        1 -> ok;
        _ -> ?assertEqual(1, CallCount,
                          io_lib:format("Expected ~p:~p/~p to be called exactly once, got ~p",
                                        [Module, Function, Arity, CallCount]))
    end.

%% ===================================================================
%% 常用 Mock 模板
%% ===================================================================

%% @doc Mock elib_param 模块
mock_elib_param(Overrides) ->
    DefaultParams = [
        {<<"account">>, <<"test@example.com">>},
        {<<"password">>, <<"password123">>},
        {<<"type">>, <<"email">>},
        {<<"rsa_encrypt">>, <<"0">>}
    ],
    Params = maps:to_list(maps:merge(maps:from_list(DefaultParams), 
                                    maps:from_list(Overrides))),
    
    Expectations = [
        {'post', 1, fun(_Req) -> Params end}
    ],
    {ok, _} = setup_mock(elib_param, Expectations).

%% @doc Mock elib_response 模块
mock_elib_response(Overrides) ->
    DefaultResponse = cowboy_req_h:new(#{
        response_status => 200,
        response_body => #{status => success}
    }),
    Response = maps:merge(DefaultResponse, Overrides),
    
    Expectations = [
        {'success', 3, fun(_Req, _Data, _Message) -> Response end},
        {'error', 2, fun(_Req, _Message) -> 
            Response#{response_status => 400, response_body => #{status => error}} 
        end}
    ],
    {ok, _} = setup_mock(elib_response, Expectations).

%% @doc Mock passport_logic 模块
mock_passport_logic(Overrides) ->
    DefaultUser = #{
        <<"uid">> => 12345,
        <<"account">> => <<"test@example.com">>,
        <<"nickname">> => <<"Test User">>
    },
    User = maps:merge(DefaultUser, Overrides),
    
    Expectations = [
        {'signup', 3, fun(_Type, _Account, _Password) -> {ok, User} end},
        {'do_login', 3, fun(_Type, _Account, _Password) -> {ok, User} end},
        {'find_password', 2, fun(_Type, _Account) -> 
            {ok, #{<<"message">> => <<"重置密码邮件已发送">>}} 
        end}
    ],
    {ok, _} = setup_mock(passport_logic, Expectations).

%% @doc Mock user_repo 模块
mock_user_repo(Overrides) ->
    DefaultUser = #{
        <<"id">> => 12345,
        <<"account">> => <<"test@example.com">>,
        <<"nickname">> => <<"Test User">>,
        <<"status">> => 1
    },
    User = maps:merge(DefaultUser, Overrides),

    Expectations = [
        {'find_by_email', 2, fun(_Email, _Column) -> User end},
        {'find_by_id', 2, fun(_Id, _Column) -> User end},
        {'save', 1, fun(_Data) -> {ok, maps:get(<<"id">>, User)} end},
        {'update', 2, fun(_Id, _Data) -> {ok, 1} end}
    ],
    {ok, _} = setup_mock(user_repo, Expectations).

%% @doc Mock elib_pg 模块
mock_elib_pg(Overrides) ->
    DefaultResult = {ok, [{1, <<"test">>}]},
    Result = maps:get(result, Overrides, DefaultResult),
    
    Expectations = [
        {'query', 2, fun(_Sql, _Params) -> Result end},
        {'query', 3, fun(_Sql, _Params, _Conn) -> Result end},
        {'pluck', 4, fun(_Table, _Column, _Conditions, _Options) -> {ok, 1} end},
        {'page', 6, fun(_Table, _Column, _Where, _OrderBy, _Size, _Offset) -> 
            maps:get(page_result, Overrides, [{<<"id">>, 1, {<<"kind">>, <<"1">>}}]) 
        end},
        {'with_tx', 1, fun(_TxFun) -> ok end}
    ],
    {ok, _} = setup_mock(elib_pg, Expectations).

%% @doc Mock user_collect_repo 模块
%% @private
mock_user_collect_repo(Overrides) ->
    DefaultCount = 0,
    Count = maps:get(count, Overrides, DefaultCount),
    
    Expectations = [
        {'count_by_uid_kind_id', 2, fun(_Uid, _KindId) -> Count end},
        {'delete', 2, fun(_Uid, _KindId) -> {ok, 1} end},
        {'update', 3, fun(_Uid, _KindId, _Data) -> {ok, 1} end},
        {'tablename', 0, fun() -> <<"public.user_collect">> end}
    ],
    {ok, _} = setup_mock(user_collect_repo, Expectations).

%% @doc Mock elib_uri 模块
%% @private
mock_elib_uri(Overrides) ->
    DefaultParams = {#{path => "/uploads/img.jpg"}, []},
    Params = maps:get(params, Overrides, DefaultParams),
    
    Expectations = [
        {'get_params', 1, fun(_Uri) -> Params end}
    ],
    {ok, _} = setup_mock(elib_uri, Expectations).

%% @doc Mock elib_dt 模块
%% @private
mock_elib_dt(Overrides) ->
    DefaultTimestamp = 1640995200,
    Timestamp = maps:get(timestamp, Overrides, DefaultTimestamp),
    
    Expectations = [
        {'now', 0, fun() -> Timestamp end},
        {'timestamp', 0, fun() -> Timestamp end}
    ],
    {ok, _} = setup_mock(elib_dt, Expectations).

%% @doc Mock group_member_repo 模块
%% @doc Mock group_member_repo 模块
%% @private
mock_group_member_repo(Overrides) ->
    DefaultResult = {ok, 1},
    Result = maps:get(result, Overrides, DefaultResult),
    
    Expectations = [
        {'transfer_ownership', 3, fun(_GroupId, _FromUid, _ToUid) -> Result end},
        {'is_owner', 2, fun(_GroupId, _Uid) -> 
            maps:get(is_owner, Overrides, true) 
        end},
        {'is_member', 2, fun(_GroupId, _Uid) -> 
            maps:get(is_member, Overrides, true) 
        end}
    ],
    {ok, _} = setup_mock(group_member_repo, Expectations).

%% @doc Mock group_repo 模块
%% @doc Mock group_repo 模块
%% @private
mock_group_repo(Overrides) ->
    DefaultGroup = #{
        <<"id">> => <<"group123">>,
        <<"name">> => <<"Test Group">>,
        <<"creator_id">> => 1
    },
    Group = maps:get(group, Overrides, DefaultGroup),
    
    Expectations = [
        {'find', 1, fun(_GroupId) -> {ok, Group} end},
        {'update', 2, fun(_GroupId, _Data) -> {ok, 1} end}
    ],
    {ok, _} = setup_mock(group_repo, Expectations).

%% @doc Mock websocket_ds 模块
%% @doc Mock websocket_ds 模块
%% @private
mock_websocket_ds(Overrides) ->
    DefaultResult = ok,
    Result = maps:get(result, Overrides, DefaultResult),
    
    Expectations = [
        {'send', 2, fun(_Uid, _Message) -> Result end},
        {'broadcast', 2, fun(_Uids, _Message) -> Result end}
    ],
    {ok, _} = setup_mock(websocket_ds, Expectations).

%% ===================================================================
%% 测试数据生成器
%% ===================================================================

%% @doc 生成默认用户测试数据
test_user() ->
    test_user(#{}).

%% @doc 生成自定义用户测试数据
test_user(Overrides) ->
    Default = #{
        id => 12345,
        account => <<"test_user_12345">>,
        nickname => <<"Test User">>,
        mobile => <<"+8613800138000">>,
        email => <<"test@example.com">>,
        password => <<"hashed_password">>,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% @doc 生成默认群组测试数据
test_group() ->
    test_group(#{}).

%% @doc 生成自定义群组测试数据
test_group(Overrides) ->
    Default = #{
        id => 54321,
        name => <<"Test Group">>,
        description => <<"A test group for testing">>,
        creator_id => 12345,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% @doc 生成默认消息测试数据
test_message() ->
    test_message(#{}).

%% @doc 生成自定义消息测试数据
test_message(Overrides) ->
    Default = #{
        id => 99999,
        from_uid => 12345,
        to_uid => 67890,
        content => <<"Hello, this is a test message">>,
        msg_type => 1,
        status => 1,
        created_at => elib_dt:timestamp()
    },
    maps:merge(Default, Overrides).

%% @doc 生成默认请求测试数据
test_request() ->
    test_request(#{}).

%% @doc 生成自定义请求测试数据
test_request(Overrides) ->
    Default = #{
        method => <<"POST">>,
        qs => <<>>,
        headers => #{},
        body => <<>>
    },
    cowboy_req_h:new(maps:merge(Default, Overrides)).

%% ===================================================================
%% EUnit 测试宏
%% ===================================================================

%% @doc 创建带单个 Mock 的测试
-define(WITH_MECK(Module, Expectations, TestFun),
    {setup,
     fun() -> 
         case meck_helper:setup_mock(Module, Expectations) of
             {ok, _} -> ok;
             {error, Reason} -> ?debugFmt("Mock setup failed: ~p", [Reason])
         end
     end,
     fun(_) -> 
         meck_helper:cleanup_mock(Module) 
     end,
     fun(_) -> ?_test(TestFun()) end}).

%% @doc 创建带多个 Mock 的测试
-define(WITH_MECKS(MockConfigs, TestFun),
    {setup,
     fun() -> 
         lists:foreach(fun({Module, Expectations}) -> 
             case meck_helper:setup_mock(Module, Expectations) of
                 {ok, _} -> ok;
                 {error, Reason} -> ?debugFmt("Mock setup failed for ~p: ~p", [Module, Reason])
             end
         end, MockConfigs)
     end,
     fun(_) -> 
         Modules = [Module || {Module, _} <- MockConfigs],
         meck_helper:cleanup_mocks(Modules)
     end,
     fun(_) -> ?_test(TestFun()) end}).

%% @doc 创建带 Mock 验证的测试
-define(WITH_MECK_VERIFY(Module, Expectations, VerifyCalls, TestFun),
    {setup,
     fun() -> 
         case meck_helper:setup_mock(Module, Expectations) of
             {ok, _} -> ok;
             {error, Reason} -> ?debugFmt("Mock setup failed: ~p", [Reason])
         end
     end,
     fun(_) -> 
         meck_helper:verify_mock(Module, VerifyCalls),
         meck_helper:cleanup_mock(Module) 
     end,
     fun(_) -> ?_test(TestFun()) end}).

%% @doc 创建强断言宏
-define(ASSERT_EQUAL(Expected, Actual),
    ?assertEqual(Expected, Actual)).

-define(ASSERT_MATCH(Pattern, Value),
    ?assertMatch(Pattern, Value)).

-define(ASSERT_OK(Result),
    ?assertMatch({ok, _}, Result)).

-define(ASSERT_ERROR(Result),
    ?assertMatch({error, _}, Result)).
