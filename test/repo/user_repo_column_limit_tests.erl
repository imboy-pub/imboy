-module(user_repo_column_limit_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% W2 收敛后缺陷立项（ZC-08 环境发现）——user 表列宽契约钉子
%%%
%%% 背景：scripts/imboy_ctl user create 把 account 同值写入
%%% user.mobile varchar(40)；账号 >40 字符时 INSERT 报 22001，
%%% 失败在调用管道中被丢弃 → 静默延迟暴露（demo R1）。
%%% 本套件钉死 DB 层行为，作为 ctl 前置校验的契约依据：
%%% 若未来放宽 mobile 列宽，本套件变红，提示同步修改 ctl 校验上限。
%%% fixture 自清理：按 w2fix 账号前缀硬删（裸用户行无从属 FK 子行）。

column_limit_contract_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        Ts = integer_to_binary(erlang:unique_integer([positive])),
        Acc40 = padded_account(<<"w2fix", Ts/binary>>, 40),
        Acc41 = <<Acc40/binary, "X">>,
        cleanup_by_prefix(Conn),
        try
            %% 恰好 40 字符（= mobile varchar(40) 上限）→ 创建成功且可查
            {ok, Id40} = user_repo:save(user_fixture(Acc40)),
            {ok, _, [{Id40, Acc40, Acc40}]} =
                epgsql:equery(
                    Conn, <<"SELECT id,account,mobile FROM \"user\" WHERE id = $1">>, [Id40]
                ),
            %% 41 字符（> mobile 上限，仍 < account varchar(80)）→ DB 拒绝
            %% （ctl 必须在此之前前置拦截，不得让该 22001 延迟暴露）
            ?assertMatch({error, _}, user_repo:save(user_fixture(Acc41))),
            ?assertEqual(0, count_by_prefix(Conn, Acc41))
        after
            cleanup_by_prefix(Conn)
        end
    end).

%%% ===================================================================
%%% Internal
%%% ===================================================================

user_fixture(Account) ->
    #{
        <<"account">> => Account,
        <<"nickname">> => <<"w2fix-col-limit"/utf8>>,
        <<"password">> => elib_password:generate(<<"admin888">>),
        <<"mobile">> => Account,
        <<"email">> => <<>>,
        <<"region">> => <<>>,
        <<"avatar">> => <<>>,
        <<"sign">> => <<>>,
        <<"gender">> => 0,
        <<"status">> => 1,
        <<"created_at">> => elib_dt:now(),
        <<"reg_ip">> => <<"127.0.0.1">>,
        <<"reg_cosv">> => <<"eunit">>,
        <<"level_id">> => 1,
        <<"experience">> => 0,
        <<"ref_user_id">> => 0,
        <<"ref_parent_user_id">> => 0,
        <<"source">> => <<"eunit">>
    }.

%% 以 "0" 右填充至指定字节数（测试账号全 ASCII，字符数=字节数）
padded_account(Prefix, Len) when byte_size(Prefix) < Len ->
    Pad = Len - byte_size(Prefix),
    <<Prefix/binary, (binary:copy(<<"0">>, Pad))/binary>>.

count_by_prefix(Conn, Account) ->
    {ok, _, [{Cnt}]} =
        epgsql:equery(
            Conn, <<"SELECT count(*)::bigint FROM \"user\" WHERE account = $1">>, [Account]
        ),
    Cnt.

cleanup_by_prefix(Conn) ->
    {ok, _} =
        epgsql:equery(
            Conn, <<"DELETE FROM \"user\" WHERE account LIKE 'w2fix%'">>
        ),
    ok.
