%% @doc 群组成员上限压力测试
%% 测试范围：
%% - 大群组成员管理
%% - 群消息广播性能
%% - 成员上限边界测试
-module(group_member_limit_stress_tests).

-include_lib("eunit/include/eunit.hrl").

%% 压力测试参数
-define(LARGE_GROUP_SIZE, 500).          % 大群成员数
-define(MEGA_GROUP_SIZE, 1000).          % 超大群成员数
-define(MAX_GROUP_MEMBERS, 2000).        % 群成员上限

%% 测试夹具
group_limit_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"大群组成员管理压力测试", fun test_large_group_management/0},
      {"大群消息广播压力测试", fun test_large_group_broadcast/0},
      {"群成员上限边界测试", fun test_member_limit_boundary/0}
     ]
    }.

setup() ->
    application:set_env(imboy, env, test),
    % 创建群主
    {ok, Owner} = create_test_user(<<"group_owner">>),

    % 创建大量成员
    MemberIds = lists:map(fun(N) ->
        {ok, Uid} = create_test_user(<<"member", N/integer>>),
        Uid
    end, lists:seq(1, ?MEGA_GROUP_SIZE)),

    #{owner => Owner, members => MemberIds}.

cleanup(_Context) ->
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_large_group_management() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Members = maps:get(members, Context),

    io:format("~n========================================~n"),
    io:format("大群组成员管理压力测试~n"),
    io:format("========================================~n"),
    io:format("群成员数: ~p~n", [?LARGE_GROUP_SIZE]),

    % 1. 创建群组
    {ok, Group} = create_test_group(Owner, <<"大群测试"/utf8>>),

    % 2. 批量添加成员
    StartTime = erlang:monotonic_time(millisecond),

    AddResults = lists:map(fun(MemberId) ->
        try
            case group_member_ds:add_member(Group, MemberId) of
                ok -> success;
                _ -> failure
            end
        catch
            _:_ -> error
        end
    end, lists:sublist(Members, ?LARGE_GROUP_SIZE)),

    EndTime = erlang:monotonic_time(millisecond),
    AddTime = EndTime - StartTime,

    SuccessCount = length(lists:filter(fun(R) -> R =:= success end, AddResults)),

    io:format("~n----------------------------------------~n"),
    io:format("成员添加结果:~n"),
    io:format("  添加耗时: ~p ms~n", [AddTime]),
    io:format("  成功数: ~p~n", [SuccessCount]),
    io:format("  成功率: ~.2f%~n", [SuccessCount / ?LARGE_GROUP_SIZE * 100]),
    io:format("  添加速度: ~.2f members/s~n", [SuccessCount * 1000 / AddTime]),

    % 3. 验证成员数量
    {ok, MemberList} = group_member_ds:list_members(Group),
    ActualCount = length(MemberList),

    io:format("  实际成员数: ~p~n", [ActualCount]),

    % 4. 测试成员查询性能
    QueryStartTime = erlang:monotonic_time(millisecond),
    QueryResults = lists:map(fun(MemberId) ->
        group_member_ds:is_member(Group, MemberId)
    end, lists:sublist(Members, 100)),
    QueryEndTime = erlang:monotonic_time(millisecond),
    QueryTime = QueryEndTime - QueryStartTime,

    QuerySuccessCount = length(lists:filter(fun(R) -> R end, QueryResults)),

    io:format("~n成员查询结果:~n"),
    io:format("  查询次数: 100~n"),
    io:format("  查询耗时: ~p ms~n", [QueryTime]),
    io:format("  平均耗时: ~.2f ms/query~n", [QueryTime / 100]),
    io:format("  查询成功率: ~.2f%~n", [QuerySuccessCount]),
    io:format("========================================~n~n"),

    ?assert(ActualCount >= ?LARGE_GROUP_SIZE * 0.95, "成员添加成功率低于95%"),
    ?assert(QueryTime < 1000, "成员查询耗时过长"),

    ok.

test_large_group_broadcast() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Members = maps:get(members, Context),

    io:format("~n========================================~n"),
    io:format("大群消息广播压力测试~n"),
    io:format("========================================~n"),

    % 1. 创建群组并添加成员
    {ok, Group} = create_test_group(Owner, <<"广播测试群"/utf8>>),

    lists:foreach(fun(MemberId) ->
        group_member_ds:add_member(Group, MemberId)
    end, lists:sublist(Members, ?LARGE_GROUP_SIZE)),

    io:format("群成员数: ~p~n", [?LARGE_GROUP_SIZE]),

    % 2. 发送多条群消息测试广播性能
    MessageCount = 10,
    BroadcastTimes = lists:map(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "群广播测试"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },

        StartTime = erlang:monotonic_time(millisecond),
        Result = msg_c2g_logic:c2g(MsgId, Owner, MsgData#{<<"to">> => elib_hashids:encode(Group)}),
        EndTime = erlang:monotonic_time(millisecond),

        case Result of
            ok -> {success, EndTime - StartTime};
            _ -> {failure, EndTime - StartTime}
        end
    end, lists:seq(1, MessageCount)),

    SuccessCount = length(lists:filter(fun({R, _}) -> R =:= success end, BroadcastTimes)),
    TotalTime = lists:sum(lists:map(fun({_, T}) -> T end, BroadcastTimes)),
    AvgTime = TotalTime / MessageCount,

    io:format("~n----------------------------------------~n"),
    io:format("广播测试结果:~n"),
    io:format("  消息数: ~p~n", [MessageCount]),
    io:format("  成功数: ~p~n", [SuccessCount]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),
    io:format("  预期接收者: ~p~n", [?LARGE_GROUP_SIZE]),
    io:format("  理论吞吐量: ~.2f msg/s~n", [MessageCount * 1000 / TotalTime]),
    io:format("========================================~n~n"),

    ?assert(SuccessCount >= MessageCount * 0.9, "广播成功率低于90%"),

    ok.

test_member_limit_boundary() ->
    Context = get_context(),
    Owner = maps:get(owner, Context),
    Members = maps:get(members, Context),

    io:format("~n========================================~n"),
    io:format("群成员上限边界测试~n"),
    io:format("========================================~n"),
    io:format("群成员上限: ~p~n", [?MAX_GROUP_MEMBERS]),

    % 1. 创建群组
    {ok, Group} = create_test_group(Owner, <<"边界测试群"/utf8>>),

    % 2. 尝试添加超过上限的成员
    TotalMembers = ?MAX_GROUP_MEMBERS + 100,
    ActualMembers = min(TotalMembers, length(Members)),

    AddResults = lists:map(fun(N) ->
        MemberId = lists:nth(N, Members),
        try
            case group_member_ds:add_member(Group, MemberId) of
                ok -> success;
                {error, group_full} -> group_full;
                _ -> failure
            end
        catch
            _:_ -> error
        end
    end, lists:seq(1, ActualMembers)),

    SuccessCount = length(lists:filter(fun(R) -> R =:= success end, AddResults)),
    GroupFullCount = length(lists:filter(fun(R) -> R =:= group_full end, AddResults)),

    % 3. 验证最终成员数量
    {ok, FinalMemberList} = group_member_ds:list_members(Group),
    FinalCount = length(FinalMemberList),

    io:format("~n----------------------------------------~n"),
    io:format("边界测试结果:~n"),
    io:format("  尝试添加: ~p~n", [ActualMembers]),
    io:format("  成功添加: ~p~n", [SuccessCount]),
    io:format("  群满拒绝: ~p~n", [GroupFullCount]),
    io:format("  实际成员: ~p~n", [FinalCount]),
    io:format("  成员上限验证: ~s~n", [
        case FinalCount =< ?MAX_GROUP_MEMBERS of
            true -> "通过";
            false -> "失败（超过上限）"
        end
    ]),
    io:format("========================================~n~n"),

    ?assert(FinalCount =< ?MAX_GROUP_MEMBERS, "群成员超过上限"),
    ?assert(GroupFullCount > 0, "未正确处理群满情况"),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    get(test_context).

create_test_user(Nickname) ->
    Uid = imboy_hashid:uid(),
    User = #{
        <<"uid">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => Nickname,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = user_repo:create(User),
    {ok, Uid}.

create_test_group(OwnerId, Name) ->
    Gid = imboy_hashid:uid(),
    Group = #{
        <<"gid">> => Gid,
        <<"owner_uid">> => OwnerId,
        <<"name">> => Name,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = group_repo:create(Group),
    ok = group_member_ds:add_member(Gid, OwnerId),
    {ok, Gid}.
