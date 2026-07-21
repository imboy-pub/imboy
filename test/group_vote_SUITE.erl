-module(group_vote_SUITE).

%%%===================================================================
%%% @doc
%%% 群投票功能 Common Test 测试套件
%%%
%%% 运行方式：
%%%   ct_run -dir test -suite group_vote_SUITE
%%%   make ct-group_vote
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    %% 创建投票
    create_single_choice_vote_succeeds/1,
    create_multi_choice_vote_succeeds/1,
    create_vote_missing_title_fails/1,
    create_vote_empty_options_fails/1,
    %% 参与投票
    cast_single_choice_vote_succeeds/1,
    cast_multi_choice_vote_succeeds/1,
    duplicate_vote_rejected/1,
    cast_vote_with_invalid_option_fails/1,
    single_choice_with_multiple_options_fails/1,
    multi_choice_with_one_option_fails/1,
    %% 投票结果查询
    get_vote_detail_returns_result/1,
    list_votes_in_group_succeeds/1,
    get_my_vote_record_succeeds/1,
    %% 投票状态
    cast_vote_on_closed_vote_fails/1,
    cast_vote_after_deadline_fails/1,
    close_vote_by_creator_succeeds/1,
    close_already_closed_vote_fails/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
        {group, create_vote},
        {group, cast_vote},
        {group, query_vote},
        {group, vote_lifecycle}
    ].

groups() ->
    [
        {create_vote, [], create_vote_cases()},
        {cast_vote, [], cast_vote_cases()},
        {query_vote, [], query_vote_cases()},
        {vote_lifecycle, [], lifecycle_cases()}
    ].

init_per_suite(Config) ->
    ct:log("开始群投票测试套件"),
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    ct:log("结束群投票测试套件"),
    cleanup_all_test_data(),
    eunit_runner:ct_suite_cleanup(Config).

init_per_group(_Group, Config) ->
    cleanup_all_test_data(),
    Config.

end_per_group(_Group, _Config) ->
    meck:unload(),
    ok.

%% ===================================================================
%% 测试用例分组定义
%% ===================================================================

create_vote_cases() ->
    [
        create_single_choice_vote_succeeds,
        create_multi_choice_vote_succeeds,
        create_vote_missing_title_fails,
        create_vote_empty_options_fails
    ].

cast_vote_cases() ->
    [
        cast_single_choice_vote_succeeds,
        cast_multi_choice_vote_succeeds,
        duplicate_vote_rejected,
        cast_vote_with_invalid_option_fails,
        single_choice_with_multiple_options_fails,
        multi_choice_with_one_option_fails
    ].

query_vote_cases() ->
    [
        get_vote_detail_returns_result,
        list_votes_in_group_succeeds,
        get_my_vote_record_succeeds
    ].

lifecycle_cases() ->
    [
        cast_vote_on_closed_vote_fails,
        cast_vote_after_deadline_fails,
        close_vote_by_creator_succeeds,
        close_already_closed_vote_fails
    ].

%% ===================================================================
%% 创建投票测试
%% ===================================================================

create_single_choice_vote_succeeds(_Config) ->
    ct:log("测试创建单选投票成功"),
    {OwnerUid, _, _} = create_three_users(),
    %% group_logic:add/4 建群返回 {ok, Gid}（Gid 为整数），创建者自动成为群主成员
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    Title = <<"周末去哪里？"/utf8>>,
    Options = [
        #{option_text => <<"公园"/utf8>>, sort_order => 1},
        #{option_text => <<"电影院"/utf8>>, sort_order => 2},
        #{option_text => <<"商场"/utf8>>, sort_order => 3}
    ],
    Extra = #{vote_type => 1, is_anonymous => false, end_at => undefined},

    Result = group_vote_logic:create_vote(Gid, OwnerUid, Title, Options, Extra, #{}),

    ?assertMatch({ok, #{}}, Result),
    {ok, Vote} = Result,
    ?assertEqual(Title, maps:get(<<"title">>, Vote)),
    ?assertEqual(1, maps:get(<<"vote_type">>, Vote)),
    ?assertEqual(Gid, maps:get(<<"group_id">>, Vote)),

    cleanup_users([OwnerUid]),
    {comment, "创建单选投票成功"}.

create_multi_choice_vote_succeeds(_Config) ->
    ct:log("测试创建多选投票成功"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    Title = <<"最喜欢的编程语言（可多选）"/utf8>>,
    Options = [
        #{option_text => <<"Erlang">>, sort_order => 1},
        #{option_text => <<"Elixir">>, sort_order => 2},
        #{option_text => <<"Go">>, sort_order => 3},
        #{option_text => <<"Rust">>, sort_order => 4}
    ],
    Extra = #{vote_type => 2, is_anonymous => false, end_at => undefined},

    Result = group_vote_logic:create_vote(Gid, OwnerUid, Title, Options, Extra, #{}),

    ?assertMatch({ok, #{}}, Result),
    {ok, Vote} = Result,
    ?assertEqual(2, maps:get(<<"vote_type">>, Vote)),

    cleanup_users([OwnerUid]),
    {comment, "创建多选投票成功"}.

create_vote_missing_title_fails(_Config) ->
    ct:log("测试创建缺少标题的投票失败"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    Options = [#{option_text => <<"选项A">>, sort_order => 1}],
    Extra = #{vote_type => 1, is_anonymous => false, end_at => undefined},

    Result = group_vote_logic:create_vote(Gid, OwnerUid, <<>>, Options, Extra, #{}),

    ?assertMatch({error, {missing_param, title}}, Result),

    cleanup_users([OwnerUid]),
    {comment, "缺少标题的投票创建被拒绝"}.

create_vote_empty_options_fails(_Config) ->
    ct:log("测试创建无选项投票失败"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    Title = <<"无效投票"/utf8>>,
    Extra = #{vote_type => 1, is_anonymous => false, end_at => undefined},

    Result = group_vote_logic:create_vote(Gid, OwnerUid, Title, [], Extra, #{}),

    ?assertMatch({error, {missing_param, options}}, Result),

    cleanup_users([OwnerUid]),
    {comment, "无选项投票创建被拒绝"}.

%% ===================================================================
%% 参与投票测试
%% ===================================================================

cast_single_choice_vote_succeeds(_Config) ->
    ct:log("测试单选投票成功"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {Gid, VoteId, OptionIds} = setup_single_choice_vote(OwnerUid, Uid1),

    [OptionId1 | _] = OptionIds,
    Result = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1]),

    ?assertMatch({ok, #{}}, Result),

    % 验证投票记录存在
    {ok, MyVote} = group_vote_logic:get_my_vote(VoteId, Uid1),
    ?assertEqual([OptionId1], maps:get(<<"option_ids">>, MyVote)),

    cleanup_users([OwnerUid, Uid1]),
    _ = Gid,
    {comment, "单选投票成功"}.

cast_multi_choice_vote_succeeds(_Config) ->
    ct:log("测试多选投票成功"),
    {OwnerUid, Uid1, _} = create_three_users(),
    %% 建群时把 Uid1 作为初始成员一并加入（add/4 的 MemberUids 只接受 binary id）
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),

    Title = <<"多选投票"/utf8>>,
    Options = [
        #{option_text => <<"选项A">>, sort_order => 1},
        #{option_text => <<"选项B">>, sort_order => 2},
        #{option_text => <<"选项C">>, sort_order => 3}
    ],
    Extra = #{vote_type => 2, is_anonymous => false, end_at => undefined},
    {ok, Vote} = group_vote_logic:create_vote(Gid, OwnerUid, Title, Options, Extra, #{}),
    VoteId = maps:get(<<"vote_id">>, Vote),

    {ok, Detail} = group_vote_logic:get_vote_detail(VoteId),
    VoteOptions = maps:get(<<"options">>, Detail),
    [Opt1, Opt2 | _] = VoteOptions,
    OptionId1 = maps:get(<<"option_id">>, Opt1),
    OptionId2 = maps:get(<<"option_id">>, Opt2),

    Result = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1, OptionId2]),

    ?assertMatch({ok, #{}}, Result),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "多选投票成功"}.

duplicate_vote_rejected(_Config) ->
    ct:log("测试重复投票被拒绝"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {_Gid, VoteId, OptionIds} = setup_single_choice_vote(OwnerUid, Uid1),

    [OptionId1 | _] = OptionIds,
    {ok, _} = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1]),

    % 再次投票应被拒绝
    Result = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1]),

    ?assertMatch({error, already_voted}, Result),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "重复投票被拒绝"}.

cast_vote_with_invalid_option_fails(_Config) ->
    ct:log("测试使用无效选项投票失败"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {_Gid, VoteId, _OptionIds} = setup_single_choice_vote(OwnerUid, Uid1),

    FakeOptionId = <<"non_existent_option_id">>,
    Result = group_vote_logic:cast_vote(VoteId, Uid1, [FakeOptionId]),

    ?assertMatch({error, invalid_option}, Result),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "无效选项投票被拒绝"}.

single_choice_with_multiple_options_fails(_Config) ->
    ct:log("测试单选投票选多个选项失败"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {_Gid, VoteId, OptionIds} = setup_single_choice_vote(OwnerUid, Uid1),

    % 单选投票选两个选项
    [OptionId1, OptionId2 | _] =
        case length(OptionIds) >= 2 of
            true ->
                OptionIds;
            false ->
                % 只有一个选项时补充一个假 id（会触发 invalid_option 或 single_choice_requires_one_option）
                OptionIds ++ [<<"fake_extra">>]
        end,

    Result = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1, OptionId2]),

    % 期望返回单选限制错误或无效选项错误
    ?assert(
        Result =:= {error, single_choice_requires_one_option} orelse
            Result =:= {error, invalid_option}
    ),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "单选投票选多个选项被拒绝"}.

multi_choice_with_one_option_fails(_Config) ->
    ct:log("测试多选投票仅选一个选项失败"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),

    Options = [
        #{option_text => <<"选项X">>, sort_order => 1},
        #{option_text => <<"选项Y">>, sort_order => 2}
    ],
    Extra = #{vote_type => 2, is_anonymous => false, end_at => undefined},
    {ok, Vote} = group_vote_logic:create_vote(Gid, OwnerUid, <<"多选测试"/utf8>>, Options, Extra, #{}),
    VoteId = maps:get(<<"vote_id">>, Vote),

    {ok, Detail} = group_vote_logic:get_vote_detail(VoteId),
    VoteOptions = maps:get(<<"options">>, Detail),
    [Opt1 | _] = VoteOptions,
    OptionId1 = maps:get(<<"option_id">>, Opt1),

    Result = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1]),

    ?assertMatch({error, multiple_choice_requires_at_least_two_options}, Result),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "多选投票仅选一个选项被拒绝"}.

%% ===================================================================
%% 投票结果查询测试
%% ===================================================================

get_vote_detail_returns_result(_Config) ->
    ct:log("测试查询投票详情返回结果"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {_Gid, VoteId, OptionIds} = setup_single_choice_vote(OwnerUid, Uid1),

    % 用户投票
    [OptionId1 | _] = OptionIds,
    {ok, _} = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1]),

    % 查询详情
    Result = group_vote_logic:get_vote_detail(VoteId),

    ?assertMatch({ok, #{}}, Result),
    {ok, Detail} = Result,
    ?assert(is_list(maps:get(<<"options">>, Detail))),
    TotalVotes = maps:get(<<"total_votes">>, Detail),
    ?assert(TotalVotes >= 1),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "查询投票详情成功"}.

list_votes_in_group_succeeds(_Config) ->
    ct:log("测试列出群内所有投票"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    % 创建两个投票
    Options = [
        #{option_text => <<"是"/utf8>>, sort_order => 1},
        #{option_text => <<"否"/utf8>>, sort_order => 2}
    ],
    Extra = #{vote_type => 1, is_anonymous => false, end_at => undefined},
    {ok, _} = group_vote_logic:create_vote(Gid, OwnerUid, <<"投票一"/utf8>>, Options, Extra, #{}),
    {ok, _} = group_vote_logic:create_vote(Gid, OwnerUid, <<"投票二"/utf8>>, Options, Extra, #{}),

    % 列出投票
    Result = group_vote_logic:list_votes(Gid, 1, 10),

    ?assertMatch({ok, #{}}, Result),
    {ok, Payload} = Result,
    Total = maps:get(<<"total">>, Payload),
    List = maps:get(<<"list">>, Payload),
    ?assert(Total >= 2),
    ?assert(length(List) >= 2),

    cleanup_users([OwnerUid]),
    {comment, "列出群内所有投票成功"}.

get_my_vote_record_succeeds(_Config) ->
    ct:log("测试查询我的投票记录"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {_Gid, VoteId, OptionIds} = setup_single_choice_vote(OwnerUid, Uid1),

    [OptionId1 | _] = OptionIds,
    {ok, _} = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1]),

    Result = group_vote_logic:get_my_vote(VoteId, Uid1),

    ?assertMatch({ok, #{}}, Result),
    {ok, MyVote} = Result,
    ?assertEqual(VoteId, maps:get(<<"vote_id">>, MyVote)),
    ?assertEqual([OptionId1], maps:get(<<"option_ids">>, MyVote)),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "查询我的投票记录成功"}.

%% ===================================================================
%% 投票生命周期测试
%% ===================================================================

cast_vote_on_closed_vote_fails(_Config) ->
    ct:log("测试对已关闭的投票投票失败"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {_Gid, VoteId, OptionIds} = setup_single_choice_vote(OwnerUid, Uid1),

    %% 关闭投票（close_vote/2：仅创建者或群管理员可操作，OwnerUid 为创建者）
    ok = group_vote_logic:close_vote(VoteId, OwnerUid),

    % 尝试投票
    [OptionId1 | _] = OptionIds,
    Result = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1]),

    ?assertMatch({error, vote_is_closed}, Result),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "对已关闭的投票投票被拒绝"}.

cast_vote_after_deadline_fails(_Config) ->
    ct:log("测试投票截止时间验证"),
    {OwnerUid, Uid1, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(Uid1)]),

    % 创建一个已过期的投票（截止时间为过去）
    Options = [
        #{option_text => <<"选项A">>, sort_order => 1},
        #{option_text => <<"选项B">>, sort_order => 2}
    ],
    % 使用一个明确已过期的 RFC3339 时间
    PastEndAt = <<"2020-01-01T00:00:00Z">>,
    Extra = #{vote_type => 1, is_anonymous => false, end_at => PastEndAt},
    {ok, Vote} = group_vote_logic:create_vote(Gid, OwnerUid, <<"过期投票"/utf8>>, Options, Extra, #{}),
    VoteId = maps:get(<<"vote_id">>, Vote),

    {ok, Detail} = group_vote_logic:get_vote_detail(VoteId),
    VoteOptions = maps:get(<<"options">>, Detail),
    [Opt1 | _] = VoteOptions,
    OptionId1 = maps:get(<<"option_id">>, Opt1),

    Result = group_vote_logic:cast_vote(VoteId, Uid1, [OptionId1]),

    % 过期投票应被拒绝
    ?assertMatch({error, vote_is_expired}, Result),

    cleanup_users([OwnerUid, Uid1]),
    {comment, "已过期投票参与被拒绝"}.

close_vote_by_creator_succeeds(_Config) ->
    ct:log("测试创建者关闭投票成功"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    Options = [
        #{option_text => <<"选项1">>, sort_order => 1},
        #{option_text => <<"选项2">>, sort_order => 2}
    ],
    Extra = #{vote_type => 1, is_anonymous => false, end_at => undefined},
    {ok, Vote} = group_vote_logic:create_vote(Gid, OwnerUid, <<"待关闭投票"/utf8>>, Options, Extra, #{}),
    VoteId = maps:get(<<"vote_id">>, Vote),

    Result = group_vote_logic:close_vote(VoteId, OwnerUid),

    ?assertEqual(ok, Result),

    % 再次查询验证状态
    {ok, Detail} = group_vote_logic:get_vote_detail(VoteId),
    Status = maps:get(<<"status">>, Detail),
    %% 2 = closed
    ?assertEqual(2, Status),

    cleanup_users([OwnerUid]),
    {comment, "关闭投票成功"}.

close_already_closed_vote_fails(_Config) ->
    ct:log("测试关闭已关闭的投票失败"),
    {OwnerUid, _, _} = create_three_users(),
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, []),

    Options = [
        #{option_text => <<"选项1">>, sort_order => 1},
        #{option_text => <<"选项2">>, sort_order => 2}
    ],
    Extra = #{vote_type => 1, is_anonymous => false, end_at => undefined},
    {ok, Vote} = group_vote_logic:create_vote(Gid, OwnerUid, <<"测试投票"/utf8>>, Options, Extra, #{}),
    VoteId = maps:get(<<"vote_id">>, Vote),

    ok = group_vote_logic:close_vote(VoteId, OwnerUid),
    Result = group_vote_logic:close_vote(VoteId, OwnerUid),

    ?assertMatch({error, vote_already_closed}, Result),

    cleanup_users([OwnerUid]),
    {comment, "重复关闭投票被拒绝"}.

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 创建三个测试用户
create_three_users() ->
    Mobile1 = unique_mobile("13910"),
    Mobile2 = unique_mobile("13910"),
    Mobile3 = unique_mobile("13910"),
    Password = <<"Test@123456">>,

    {ok, _} = passport_logic:signup(Mobile1, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile2, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile3, Password, <<".@example.com">>, #{}),

    User1 = user_repo:find_by_mobile(Mobile1, <<"id">>),
    User2 = user_repo:find_by_mobile(Mobile2, <<"id">>),
    User3 = user_repo:find_by_mobile(Mobile3, <<"id">>),

    Uid1 = maps:get(<<"id">>, User1),
    Uid2 = maps:get(<<"id">>, User2),
    Uid3 = maps:get(<<"id">>, User3),

    {Uid1, Uid2, Uid3}.

%% 创建单选投票并返回 {Gid, VoteId, OptionIds}
%% 建群时把 MemberUid 作为初始成员一并加入（add/4 的 MemberUids 只接受 binary id）
setup_single_choice_vote(OwnerUid, MemberUid) ->
    {ok, Gid} = group_logic:add(0, OwnerUid, 1, [integer_to_binary(MemberUid)]),

    Options = [
        #{option_text => <<"选项A">>, sort_order => 1},
        #{option_text => <<"选项B">>, sort_order => 2},
        #{option_text => <<"选项C">>, sort_order => 3}
    ],
    Extra = #{vote_type => 1, is_anonymous => false, end_at => undefined},
    {ok, Vote} = group_vote_logic:create_vote(
        Gid, OwnerUid, <<"单选投票测试"/utf8>>, Options, Extra, #{}
    ),
    VoteId = maps:get(<<"vote_id">>, Vote),

    {ok, Detail} = group_vote_logic:get_vote_detail(VoteId),
    VoteOptions = maps:get(<<"options">>, Detail),
    OptionIds = [maps:get(<<"option_id">>, O) || O <- VoteOptions],

    {Gid, VoteId, OptionIds}.

unique_mobile(Prefix) ->
    Suffix = erlang:phash2(
        {erlang:system_time(microsecond), erlang:unique_integer([monotonic, positive]), self()},
        1000000
    ),
    list_to_binary(io_lib:format("~s~6..0B", [Prefix, Suffix])).

cleanup_users([]) ->
    ok;
cleanup_users([Uid | Rest]) ->
    user_repo:delete(Uid),
    cleanup_users(Rest).

cleanup_all_test_data() ->
    Sql = <<"SELECT id FROM \"user\" WHERE mobile LIKE '13910%'">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            lists:foreach(
                fun(#{<<"id">> := Id}) ->
                    user_repo:delete(Id)
                end,
                Rows
            );
        _ ->
            ok
    end.
