#!/usr/bin/env escript
%%! -noshell -noinput +A 4
%%%
%%% Limiter V1 验收最小入口（计划 docs/plans/2026-10-13-limiter-v1-*
%%% §9.2 命令别名 C）。L0 职责：实现入口、参数校验、失败退出码传播；
%%% 仅承载现有 EUnit runner 无法表达的场景（REST 组合 / 负载 / 节点切换）。
%%% EUnit 承载的 AC（Q(模块)）不在本工具重复执行。
%%%
%%% 用法:
%%%   scripts/limiter_v1_acceptance.escript --case AC-01 --manifest <清单>
%%%   scripts/limiter_v1_acceptance.escript --list
%%%   scripts/limiter_v1_acceptance.escript --emit-manifest <输出路径>
%%%
%%% 选项:
%%%   --case AC-XX           执行指定验收场景
%%%   --manifest <路径>      候选清单（每行 `<sha256>  <path>`；--emit-manifest 生成）
%%%   --oracle baseline|fixed  断言模式（默认 baseline = 断言旧缺陷仍在；
%%%                          fixed = 修复后 oracle，L3 前未实现）
%%%   --evidence-dir <目录>  证据输出目录（默认 docs/qa/limiter-v1/...，
%%%                          该路径在 .gitignore 的 /docs/qa/ 下）
%%%
%%% 退出码:
%%%   0  PASS（该 case 在所选 oracle 下成立）
%%%   2  用法/环境错误（参数缺失、manifest 不可读、共享节点被占用等）
%%%   3  场景执行失败（断言/子命令非零退出）
%%%   4  INPUT_PENDING / NOT_IMPLEMENTED_IN_L0（如 AC-16 输入未冻结）
%%%   5  未知 case
%%%

main(Args) ->
    case parse(Args, #{oracle => baseline, evidence_dir => undefined}) of
        {ok, Opts} -> dispatch(Opts);
        {error, UsageError} -> usage_error(UsageError)
    end.

%% -------------------------------------------------------------------------
%% 参数解析
%% -------------------------------------------------------------------------

parse([], Acc) ->
    case Acc of
        #{action := _} -> {ok, Acc};
        _ -> {error, missing_case_or_list}
    end;
parse(["--list" | Rest], Acc) ->
    parse(Rest, Acc#{action => list});
parse(["--case", Case | Rest], Acc) ->
    parse(Rest, Acc#{action => {run_case, Case}});
parse(["--emit-manifest", Path | Rest], Acc) ->
    parse(Rest, Acc#{action => {emit_manifest, Path}});
parse(["--manifest", Path | Rest], Acc) ->
    parse(Rest, Acc#{manifest => Path});
parse(["--oracle", Oracle | Rest], Acc) when Oracle =:= "baseline"; Oracle =:= "fixed" ->
    parse(Rest, Acc#{oracle := list_to_atom(Oracle)});
parse(["--oracle", Other | _], _Acc) ->
    {error, {bad_oracle, Other}};
parse(["--evidence-dir", Dir | Rest], Acc) ->
    parse(Rest, Acc#{evidence_dir => Dir});
parse([Unknown | _], _Acc) ->
    {error, {unknown_arg, Unknown}}.

dispatch(#{action := list}) ->
    list_cases(),
    halt(0);
dispatch(#{action := {emit_manifest, Path}}) ->
    emit_manifest(Path);
dispatch(#{action := {run_case, Case}} = Opts) ->
    run_case(Case, Opts).

%% -------------------------------------------------------------------------
%% case 登记
%% -------------------------------------------------------------------------

%% C 承载的 case：REST / 负载 / 节点切换。
-define(CASES, [
    {<<"AC-01">>, c_rest_login_real_failures},
    {<<"AC-13">>, not_implemented_l0},
    {<<"AC-15">>, not_implemented_l0},
    {<<"AC-16">>, input_pending},
    {<<"AC-18">>, not_implemented_l0}
]).

%% Q(模块) 承载的 case：本工具不重复执行，给出定向命令。
-define(Q_CASES, [
    {<<"AC-02">>, <<"passport_logic_tests">>},
    {<<"AC-03">>, <<"login_attempt_ds_tests">>},
    {<<"AC-04">>, <<"login_attempt_ds_tests">>},
    {<<"AC-05">>, <<"login_security_logic_tests">>},
    {<<"AC-06">>, <<"login_attempt_ds_tests">>},
    {<<"AC-07">>, <<"login_attempt_ds_tests">>},
    {<<"AC-08">>, <<"login_attempt_ds_tests">>},
    {<<"AC-09">>, <<"elib_req_client_ip_tests">>},
    {<<"AC-10">>, <<"login_attempt_ds_tests">>},
    {<<"AC-11">>, <<"login_attempt_ds_tests">>},
    {<<"AC-12">>, <<"login_security_logic_tests">>},
    {<<"AC-14">>, <<"throttle_middleware_tests">>},
    {<<"AC-17">>, <<"passport_handler_tests">>},
    {<<"AC-19">>, <<"passport_logic_tests">>}
]).

list_cases() ->
    io:format("~ts~n", [<<"C 承载（本工具）:"/utf8>>]),
    [io:format("  ~s~n", [C]) || {C, _} <- ?CASES],
    io:format("~ts~n", [<<"Q 承载（IMBOYENV=local make eunit-local t=<模块>）:"/utf8>>]),
    [io:format("  ~s -> ~s~n", [C, M]) || {C, M} <- ?Q_CASES].

run_case(CaseBin, Opts) ->
    Case = unicode:characters_to_binary(CaseBin),
    Manifest = maps:get(manifest, Opts, undefined),
    case lists:keyfind(Case, 1, ?CASES) of
        {Case, c_rest_login_real_failures} ->
            ensure_manifest(Manifest, fun() -> run_ac01(Opts) end);
        {Case, input_pending} ->
            pending(Case, <<"AC-16: §7.6 输入表未冻结（INPUT_PENDING），候选测量前须冻结预算与推导"/utf8>>);
        {Case, not_implemented_l0} ->
            pending(Case, <<"节点/切换类场景未在 L0 实现（NOT_IMPLEMENTED_IN_L0）"/utf8>>);
        false ->
            case lists:keyfind(Case, 1, ?Q_CASES) of
                {Case, Module} ->
                    io:format(
                        "~ts IMBOYENV=local make eunit-local t=~s~n",
                        [Case, Module]
                    ),
                    halt(4);
                false ->
                    UnknownMsg = <<"未知 case: "/utf8, Case/binary, "（--list 查看登记）"/utf8>>,
                    io:format(standard_error, "~ts~n", [UnknownMsg]),
                    halt(5)
            end
    end.

ensure_manifest(undefined, _Fun) ->
    io:format(standard_error, "~ts~n", [<<"缺少 --manifest（--emit-manifest 生成候选清单）"/utf8>>]),
    halt(2);
ensure_manifest(Path, Fun) ->
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > 0 ->
            Fun();
        {ok, _} ->
            io:format(standard_error, "manifest 为空: ~s~n", [Path]),
            halt(2);
        {error, Reason} ->
            io:format(standard_error, "manifest 不可读: ~s (~p)~n", [Path, Reason]),
            halt(2)
    end.

pending(Case, Reason) ->
    io:format("~ts INPUT_PENDING/NOT_IMPLEMENTED: ~ts~n", [Case, Reason]),
    halt(4).

%% -------------------------------------------------------------------------
%% AC-01：真实 REST 登录链失败计数（L0 baseline oracle）
%% -------------------------------------------------------------------------

run_ac01(Opts) ->
    case maps:get(oracle, Opts) of
        baseline ->
            run_ac01_baseline(Opts);
        fixed ->
            io:format("AC-01 fixed oracle 未实现（L3 修复后接线）~n"),
            halt(4)
    end.

run_ac01_baseline(Opts) ->
    Root = project_root(),
    EvDir = abs_evidence_dir(Root, evidence_dir(Opts, <<"AC-01">>)),
    ok = filelib:ensure_dir(<<EvDir/binary, "/x">>),
    RunId = "limiter-l0-" ++ strftime_run_ref(),
    %% 走项目正规 REST 通道（scripts/run_rest_api_tests.sh：scratch 数据库
    %% + 完整环境 + evidence 交叉校验）。数据库凭据由调用方环境提供
    %% （REST_PG_*，runner 合同），本工具不读取、不落盘任何凭据。
    %% runner 自带 ct_imboy 共享节点等待（BLOCKED_SHARED_CT 退出码 75）。
    Cmd = lists:flatten(
        io_lib:format(
            "cd ~s && REST_RUN_ID=~s bash scripts/run_rest_api_tests.sh 2>&1 | tee ~s/output.log",
            [Root, RunId, EvDir]
        )
    ),
    io:format("AC-01 (baseline): ~s~n", [Cmd]),
    Rc = run_shell(Cmd),
    ok = file:write_file(<<EvDir/binary, "/exit_code">>, integer_to_binary(Rc)),
    ManifestVal =
        case maps:find(manifest, Opts) of
            {ok, V} when is_list(V) -> list_to_binary(V);
            {ok, V2} -> value_to_binary(V2);
            error -> undefined
        end,
    write_input_json(EvDir, #{
        case_id => <<"AC-01">>,
        oracle => baseline,
        candidate => candidate_id(),
        manifest => ManifestVal,
        rest_run_id => list_to_binary(RunId)
    }),
    case Rc of
        0 ->
            PassMsg = <<"AC-01 baseline PASS（真实链失败计数断链复现成功；证据: "/utf8, EvDir/binary>>,
            io:format("~ts~n", [PassMsg]),
            halt(0);
        _ ->
            FailMsg0 =
                <<"AC-01 baseline FAIL rc="/utf8, (integer_to_binary(Rc))/binary, "（证据: "/utf8,
                    EvDir/binary, "）"/utf8>>,
            io:format(standard_error, "~ts~n", [FailMsg0]),
            halt(3)
    end.

%% -------------------------------------------------------------------------
%% manifest / 证据 / 杂项
%% -------------------------------------------------------------------------

emit_manifest(Path) ->
    Root = project_root(),
    Files = [
        "src/ds/login_attempt_ds.erl",
        "src/logic/login_security_logic.erl",
        "src/logic/passport_logic.erl",
        "src/api/passport_handler.erl",
        "src/api/throttle_middleware.erl",
        "src/api/auth_oidc_handler.erl",
        "src/imboy_app.erl"
    ],
    {ok, Head} = run_capture(Root, "git rev-parse HEAD"),
    {ok, Sums} = run_capture(Root, "shasum -a 256 " ++ string:join(Files, " ")),
    Entries =
        lists:map(
            fun(Line) ->
                %% shasum 输出: "<sha256>  <path>"（Erlang 侧切分，避免 shell 引号嵌套）
                [Sum | Rest] = string:split(Line, "  "),
                string:trim(Sum) ++ "  " ++ string:trim(hd(Rest))
            end,
            [L || L <- string:split(Sums, "\n", all), L =/= []]
        ),
    Body = string:join([string:trim(Head) ++ "  HEAD" | Entries], "\n") ++ "\n",
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, list_to_binary(Body)),
    io:format("manifest generated: ~s (~b items)~n", [Path, length(Entries) + 1]),
    halt(0).

run_capture(Root, Cmd) ->
    Port = open_port({spawn, "sh -c '" ++ Cmd ++ "'"}, [exit_status, {cd, Root}, {line, 4096}, hide]),
    collect_port(Port, []).

collect_port(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} -> collect_port(Port, [Line | Acc]);
        {Port, {data, {noeol, Line}}} -> collect_port(Port, [Line | Acc]);
        {Port, {exit_status, 0}} -> {ok, lists:join("\n", lists:reverse(Acc))};
        {Port, {exit_status, S}} -> {error, S}
    after 30000 -> {error, timeout}
    end.

candidate_id() ->
    Root = project_root(),
    case run_capture(Root, "git rev-parse --short HEAD") of
        {ok, Out} -> list_to_binary(string:trim(Out));
        _ -> <<"unknown">>
    end.

project_root() ->
    %% escript 脚本位于 <root>/scripts/，向上取仓根（absname 防相对调用）
    Es = escript:script_name(),
    filename:dirname(filename:dirname(filename:absname(Es))).

evidence_dir(#{evidence_dir := Dir}, _Case) when is_list(Dir) ->
    list_to_binary(Dir);
evidence_dir(_Opts, Case) ->
    RunRef = list_to_binary(strftime_run_ref()),
    <<"docs/qa/limiter-v1/", (candidate_id())/binary, "/", RunRef/binary, "/", Case/binary>>.

%% @private 默认证据目录相对仓根：绝对化，避免 Cmd 子 shell（cd Root 后 tee
%% output.log）与主进程 write_file（调用者 cwd）各按一处相对基准落盘。
abs_evidence_dir(Root, Dir) ->
    case filename:pathtype(Dir) of
        absolute -> Dir;
        _ -> list_to_binary(filename:join(Root, unicode:characters_to_list(Dir)))
    end.

strftime_run_ref() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:local_time(),
    lists:flatten(
        io_lib:format(
            "~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0b-~s",
            [Y, Mo, D, H, Mi, S, os:getpid()]
        )
    ).

write_input_json(EvDir, Map) ->
    Json = lines_json(maps:to_list(Map)),
    ok = file:write_file(<<EvDir/binary, "/input.json">>, Json).

lines_json(Kvs) ->
    Inner = lists:join(
        <<",\n">>,
        [json_kv(K, V) || {K, V} <- Kvs, V =/= undefined]
    ),
    <<"{\n", (list_to_binary(Inner))/binary, "\n}\n">>.

json_kv(K, V) ->
    list_to_binary(io_lib:format("  \"~s\": \"~s\"", [to_bin(K), value_to_binary(V)])).

value_to_binary(V) when is_binary(V) -> V;
value_to_binary(V) when is_atom(V) -> atom_to_binary(V);
value_to_binary(V) when is_list(V) -> list_to_binary(V);
value_to_binary(V) -> list_to_binary(io_lib:format("~p", [V])).

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V) -> atom_to_binary(V);
to_bin(V) -> list_to_binary(V).

%% @private os:cmd 不返回退出码：用尾部标记 __LVA_EXIT__<rc> 传递。
%% 命令本体由本工具构造（无该标记字面量），tee 输出与标记同流。
run_shell(Cmd) ->
    Out = os:cmd(Cmd ++ " ; printf '\n__LVA_EXIT__%s' \"$?\""),
    case string:split(Out, "\n__LVA_EXIT__", trailing) of
        [_, RcStr] ->
            list_to_integer(string:trim(RcStr));
        _ ->
            %% 标记缺失（异常路径）按失败处理
            255
    end.

usage_error(missing_case_or_list) ->
    io:format(standard_error, "missing --case / --list / --emit-manifest~n", []),
    halt(2);
usage_error({unknown_arg, Arg}) ->
    io:format(standard_error, "unknown arg: ~s~n", [Arg]),
    halt(2);
usage_error({bad_oracle, O}) ->
    io:format(standard_error, "--oracle must be baseline|fixed: ~s~n", [O]),
    halt(2).
