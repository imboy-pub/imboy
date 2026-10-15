#!/usr/bin/env escript
%%! -noshell -noinput +A 4 -pa /Users/leeyi/project/imboy.pub/.worktrees/limiter-v1-a4/ebin
%%%
%%% Limiter V1 AC-16 基线 A：旧代码（0d28ad4f）E2E 旧真实成本。
%%% 真实 passport_logic:do_login/5 → login_security_logic →
%%% login_attempt_ds（真实 gen_server+ETS+imboy_cache/depcache）→
%%% verify_user（真实 elib_password 校验）。
%%%
%%% mock 边界（记录）：仅 user_ds:find_by_account/2 等查询以内存 stub 替代
%%% （返回合成用户 map：status=1、account/email/mobile/password 齐备）；
%%% elib_password 真实校验（本仓 default_md5+salt 档，单次 ~µs 级，无需
%%% mock 密码校验函数）。旧代码 Ip 恒 <<>>（do_login_verify 硬编码），
%%% 即真实本地直连形态。
%%%
%%% 冻结输入：账户键 16B（acct_e2e_0000001..500，唯一账户各 1 次）；
%%% N=500 错误密码；warmup 20 次；3 次独立重复；monotonic_time 计时。
%%% 记账 oracle：get_attempts 恒 0（record_login_failure 先
%%% check_ip_rate_limit(<<>>) → invalid_ip → 不记账——旧缺陷）；
%%% IP 计数 Cache 键 {login_attempt_ip_limit, <<>>} 不存在（未写入）。

-define(N, 500).
-define(N_WARMUP, 20).
-define(N_REPEATS, 3).

main(_) ->
    OldRoot = "/Users/leeyi/project/imboy.pub/.worktrees/limiter-v1-a4",
    [code:add_patha(P) || P <- filelib:wildcard(OldRoot ++ "/deps/*/ebin")],
    {ok, _} = imboy_cache:start_link([]),
    {ok, _} = login_attempt_ds:start_link(),
    ok = load_user_ds_stub(),
    {ok, Salt} = ensure_password_salt(),

    Env = #{
        otp => otp_release_s(),
        os_version => os:cmd("sw_vers -productVersion") -- "\n",
        old_head => string:trim(os:cmd("git -C " ++ OldRoot ++ " rev-parse --short HEAD")),
        n_requests => ?N,
        warmup => ?N_WARMUP,
        repeats => ?N_REPEATS,
        account_key_bytes => 16,
        ip => "<<>> (old code hardcodes empty IP in do_login_verify)",
        mock => "user_ds find_by_* in-memory stub only; real elib_password verify",
        password_salt_source => Salt
    },

    warmup(),
    Runs = [run_once(R) || R <- lists:seq(1, ?N_REPEATS)],

    Out = #{
        env => Env,
        e2e_wrong_password_login => #{
            runs => Runs,
            aggregate => aggregate(Runs)
        }
    },
    io:format("~s~n", [json_encode(Out)]),
    halt(0).

run_once(_) ->
    %% 每次 run 独立账户空间（run 前缀），避免任何跨 run 残留
    Accounts = [account_key(I) || I <- lists:seq(1, ?N)],
    {_, Red0} = worker_reductions(),
    T0 = erlang:monotonic_time(nanosecond),
    {Times, ErrC, OtherC} =
        lists:foldl(
            fun(Account, {Acc, E, O}) ->
                Ts0 = erlang:monotonic_time(nanosecond),
                Res = passport_logic:do_login(
                    <<"account">>, Account, <<"TotallyWrong!">>, <<"android">>, <<"benchdid">>
                ),
                Ts1 = erlang:monotonic_time(nanosecond),
                case Res of
                    {error, _} -> {[Ts1 - Ts0 | Acc], E + 1, O};
                    _ -> {[Ts1 - Ts0 | Acc], E, O + 1}
                end
            end,
            {[], 0, 0},
            Accounts
        ),
    T1 = erlang:monotonic_time(nanosecond),
    {_, Red1} = worker_reductions(),
    %% 记账 oracle：预期 0（旧缺陷）
    AccountingCounts =
        [begin
             {ok, C} = login_attempt_ds:get_attempts(A, <<>>),
             C
         end
         || A <- Accounts],
    NonZero = length([C || C <- AccountingCounts, C =/= 0]),
    %% IP 计数 Cache 行为：空 IP 键从未写入
    IpCacheKey = {login_attempt_ip_limit, <<>>},
    IpCacheValue =
        case imboy_cache:get(IpCacheKey) of
            undefined -> undefined;
            {ok, V} -> V;
            Raw -> Raw
        end,
    Sorted = lists:sort(Times),
    #{
        events => ?N,
        errors_returned => ErrC,
        unexpected_non_error => OtherC,
        p50_us => ns_to_us(percentile(Sorted, 0.50)),
        p95_us => ns_to_us(percentile(Sorted, 0.95)),
        p99_us => ns_to_us(percentile(Sorted, 0.99)),
        max_us => ns_to_us(lists:last(Sorted)),
        mean_us => ns_to_us(lists:sum(Sorted) div max(1, length(Sorted))),
        throughput_rps => round(?N / (lists:sum(Sorted) / 1.0e9)),
        reductions_per_request => round((Red1 - Red0) / ?N),
        wall_ms => (T1 - T0) div 1000000,
        account_rows_accounted => length(AccountingCounts) - NonZero,
        account_rows_nonzero => NonZero,
        ip_cache_key_value => value_repr(IpCacheValue)
    }.

aggregate(Runs) ->
    Ps = [maps:get(p95_us, R) || R <- Runs],
    P99s = [maps:get(p99_us, R) || R <- Runs],
    Ths = [maps:get(throughput_rps, R) || R <- Runs],
    Acc = [maps:get(account_rows_accounted, R) || R <- Runs],
    #{
        p95_us_range => [lists:min(Ps), lists:max(Ps)],
        p99_us_range => [lists:min(P99s), lists:max(P99s)],
        throughput_rps_range => [lists:min(Ths), lists:max(Ths)],
        account_rows_accounted_each_run => Acc
    }.

warmup() ->
    [
        passport_logic:do_login(
            <<"account">>, <<"warmup_acct_", (integer_to_binary(I))/binary>>, <<"Wrong!">>,
            <<"android">>, <<"warmupdid">>
        )
     || I <- lists:seq(1, ?N_WARMUP)
    ],
    ok.

%% -------------------------------------------------------------------------
%% 合成用户 / stub user_ds
%% -------------------------------------------------------------------------

account_key(I) ->
    N = integer_to_list(I),
    Padded = string:right(N, 7, $0),
    %% 16 字节：acct_e2e_ (9) + 7 位序号
    list_to_binary("acct_e2e_" ++ Padded).

load_user_ds_stub() ->
    Src =
        "-module(user_ds).\n"
        "-export([find_by_account/2, find_by_email/2, find_by_mobile/2]).\n"
        "find_by_account(_, _) -> synth().\n"
        "find_by_email(_, _) -> synth().\n"
        "find_by_mobile(_, _) -> synth().\n"
        "synth() ->\n"
        "    #{<<\"id\">> => 1, <<\"uid\">> => <<\"1\">>,\n"
        "      <<\"account\">> => <<\"bench_user\">>,\n"
        "      <<\"email\">> => <<\"bench@example.invalid\">>,\n"
        "      <<\"mobile\">> => <<>>, <<\"nickname\">> => <<\"Bench\">>,\n"
        "      <<\"status\">> => 1,\n"
        "      <<\"password\">> => elib_password:generate(<<\"RightPassword123!\">>)}.\n",
    Tmp = "/tmp/limiter-bench/user_ds_bench_stub.erl",
    ok = file:write_file(Tmp, Src),
    {ok, user_ds, Bin} = compile:file(Tmp, [binary, report_errors]),
    {module, user_ds} = code:load_binary(user_ds, Tmp, Bin),
    ok.

ensure_password_salt() ->
    %% 真实 config_ds:env 读取（无 DB 依赖时回落 <<>>）
    try {ok, config_ds:env(password_salt, <<>>)}
    catch _:_ -> {ok, default_missing}
    end.

worker_reductions() ->
    {reductions, R} = process_info(self(), reductions),
    {ok, R}.

percentile(Sorted, Q) ->
    N = length(Sorted),
    Idx = max(1, round(Q * N)),
    lists:nth(Idx, Sorted).

ns_to_us(Ns) when is_integer(Ns) -> round(Ns / 100) / 10;
ns_to_us(X) -> X.

value_repr(undefined) -> null;
value_repr(V) -> list_to_binary(io_lib:format("~0p", [V])).

otp_release_s() ->
    case erlang:system_info(otp_release) of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L) -> L
    end.

%% -------------------------------------------------------------------------
%% JSON
%% -------------------------------------------------------------------------

json_encode(Term) -> iolist_to_binary(json(Term)).

json(V) when is_map(V) ->
    Inner = lists:join(
        <<",">>,
        [[$", key_bin(K), $", $:] ++ json(Val) || {K, Val} <- maps:to_list(V)]
    ),
    [${, Inner, $}];
json(V) when is_list(V) ->
    case is_printable_list(V) of
        true -> [$", V, $"];
        false ->
            Inner = lists:join(<<",">>, [json(E) || E <- V]),
            [$[, Inner, $]]
    end;
json(V) when is_binary(V) -> [$", esc(V), $"];
json(null) -> <<"null">>;
json(V) when is_atom(V) -> [$", atom_to_binary(V, utf8), $"];
json(V) when is_integer(V) -> integer_to_binary(V);
json(V) when is_float(V) -> float_to_binary(V, [{decimals, 2}, compact]);
json(V) -> list_to_binary(io_lib:format("~p", [V])).

key_bin(K) when is_atom(K) -> atom_to_binary(K, utf8);
key_bin(K) when is_binary(K) -> K;
key_bin(K) when is_list(K) -> list_to_binary(K);
key_bin(K) -> list_to_binary(io_lib:format("~p", [K])).

is_printable_list([C | Rest]) when is_integer(C), C >= 32, C =< 126 -> is_printable_list(Rest);
is_printable_list([]) -> true;
is_printable_list(_) -> false.

esc(B) ->
    binary:replace(
        binary:replace(B, <<"\\">>, <<"\\\\">>, [global]), <<"\"">>, <<"\\\"">>, [global]
    ).
