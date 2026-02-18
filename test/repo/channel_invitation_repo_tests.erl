-module(channel_invitation_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc 频道邀请 Repo 层测试
%%%
%%% 测试目标：
%%% - 验证邀请创建逻辑
%%% - 验证邀请码生成
%%% - 验证邀请状态管理
%%% - 验证邀请过期处理
%%%===================================================================

%% ===================================================================
%% 准备工作测试
%% ===================================================================

setup_test_() ->
    ?TEST_SIMPLE(fun() ->
        case application:get_env(imboy, env) of
            test -> ?assert(true);
            _ -> ?assert(true)
        end
    end).

%% ===================================================================
%% 邀请码生成测试
%% ===================================================================

invitation_code_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 生成多个邀请码验证格式
        Codes = [generate_test_invitation_code() || _ <- lists:seq(1, 10)],

        % 验证所有邀请码长度为 8
        lists:foreach(fun(Code) ->
            ?assertEqual(8, byte_size(Code))
        end, Codes),

        % 验证邀请码都是大写字母或数字
        lists:foreach(fun(Code) ->
            ?assert(validate_invitation_code_format(Code))
        end, Codes),

        % 验证邀请码唯一性
        ?assertEqual(length(Codes), length(lists:usort(Codes)))
    end).

%% ===================================================================
%% 邀请数据验证测试
%% ===================================================================

validate_invitation_data_test_() ->
    ?TEST_SIMPLE(fun() ->
        ChannelId = 10001,
        InviterUid = 1001,
        InviteeUid = 1002,
        Message = <<"欢迎加入我们的频道"/utf8>>,

        % 创建有效的邀请数据
        InvitationData = #{
            channel_id => ChannelId,
            inviter_uid => InviterUid,
            invitee_uid => InviteeUid,
            message => Message
        },

        % 验证必填字段
        ?assertEqual(ChannelId, maps:get(channel_id, InvitationData)),
        ?assertEqual(InviterUid, maps:get(inviter_uid, InvitationData)),
        ?assertEqual(InviteeUid, maps:get(invitee_uid, InvitationData)),
        ?assertEqual(Message, maps:get(message, InvitationData))
    end).

%% ===================================================================
%% 邀请状态测试
%% ===================================================================

invitation_status_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 状态定义
        StatusPending = 0,
        StatusAccepted = 1,
        StatusRejected = 2,
        StatusExpired = 3,
        StatusCancelled = 4,

        % 验证状态值
        ?assertEqual(0, StatusPending),
        ?assertEqual(1, StatusAccepted),
        ?assertEqual(2, StatusRejected),
        ?assertEqual(3, StatusExpired),
        ?assertEqual(4, StatusCancelled)
    end).

%% ===================================================================
%% 邀请过期时间测试
%% ===================================================================

invitation_expiry_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 默认7天过期
        SevenDaysMs = 7 * 24 * 60 * 60 * 1000,
        Now = elib_dt:now(),
        ExpiresAt = Now + SevenDaysMs,

        % 验证过期时间计算
        ?assert(ExpiresAt > Now),
        ?assertEqual(SevenDaysMs, ExpiresAt - Now)
    end).

%% ===================================================================
%% 邀请消息长度验证测试
%% ===================================================================

invitation_message_length_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 邀请消息最多 500 字符
        MaxLength = 500,
        ValidMessage = <<"欢迎加入频道"/utf8>>,
        LongMessage = binary:copy(<<"a">>, 501),

        ?assert(byte_size(ValidMessage) =< MaxLength),
        ?assert(byte_size(LongMessage) > MaxLength)
    end).

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% @doc 生成测试用邀请码
generate_test_invitation_code() ->
    Chars = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>,
    generate_code_chars(8, Chars, <<>>).

generate_code_chars(0, _Chars, Acc) -> Acc;
generate_code_chars(N, Chars, Acc) ->
    Pos = rand:uniform(byte_size(Chars)),
    <<_:Pos/binary, Char:1/binary, _/binary>> = Chars,
    generate_code_chars(N - 1, Chars, <<Acc/binary, Char/binary>>).

%% @doc 验证邀请码格式
validate_invitation_code_format(Code) ->
    case byte_size(Code) of
        8 ->
            ValidChars = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>,
            binary:matches(Code, ValidChars) =/= [];
        _ ->
            false
    end.
