-module(imboy_capacity_config_tests).

%%%===================================================================
%%% R0（P1-1）容量配置一致性测试（设计 §9.1 T1/T2/T3/T6 的静态层）
%%%
%%% 五个 VM 参数部署入口的 +P/+Q 必须：为 2 的幂（请求值=实际值）、
%%% ≥ 派生下限。D2/D3 批准范围：docker/dev 路径（config/vm.args、
%%% start_node.sh）= 65536/65536；蓝绿（1048576）与 native-shell
%%% （262144/65536）本批不改数值，由下限一致性断言守住。
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%% next_pow2(ceil(1.25×(4096+20000+20400)))
-define(DERIVED_P_FLOOR, 65536).
-define(DERIVED_Q_FLOOR, 65536).
-define(APPROVED_W_DEFAULT, 20000).

%% ------------------------------------------------------------------
%% T1：五个来源的 +P/+Q 一致性
%% ------------------------------------------------------------------

vm_args_derived_values_test() ->
    S = code_bin("config/vm.args"),
    ?assertEqual([65536], flag_values(S, "+P")),
    ?assertEqual([65536], flag_values(S, "+Q")),
    ok.

start_node_script_derived_values_test() ->
    S = code_bin("scripts/start_node.sh"),
    ?assertEqual([65536], flag_values(S, "+P")),
    ?assertEqual([65536], flag_values(S, "+Q")),
    ok.

blue_green_values_unchanged_and_above_floor_test() ->
    %% D2/D3：蓝绿本批不改数值（1048576），只做"下限一致性"守住
    S = code_bin("scripts/lib/blue_green_deploy.sh"),
    Ps = flag_values(S, "+P"),
    Qs = flag_values(S, "+Q"),
    ?assertMatch([_ | _], Ps),
    ?assertMatch([_ | _], Qs),
    [?assert(N >= ?DERIVED_P_FLOOR) || N <- Ps],
    [?assert(N >= ?DERIVED_Q_FLOOR) || N <- Qs],
    [?assert(is_pow2(N)) || N <- Ps ++ Qs],
    ok.

native_shell_values_unchanged_and_above_floor_test() ->
    S = code_bin("deploy/native-shell/lib/render.sh"),
    Ps = flag_values(S, "+P"),
    Qs = flag_values(S, "+Q"),
    ?assertMatch([_ | _], Ps),
    ?assertMatch([_ | _], Qs),
    [?assert(N >= ?DERIVED_P_FLOOR) || N <- Ps],
    [?assert(N >= ?DERIVED_Q_FLOOR) || N <- Qs],
    [?assert(is_pow2(N)) || N <- Ps ++ Qs],
    ok.

approved_ws_default_matches_budget_model_test() ->
    %% D1 批准值 20000 必须仍满足派生式：P_need = B_p + W + N_L×H×2
    W = approved_ws_default_from_source(),
    ?assertEqual(?APPROVED_W_DEFAULT, W),
    PNeed = 4096 + W + 10240 * 2,
    ?assert(?DERIVED_P_FLOOR >= next_pow2(PNeed)),
    ?assert(is_pow2(?DERIVED_P_FLOOR)),
    ok.

%% ------------------------------------------------------------------
%% T2/T3：监听器显式选项 + 死键移除
%% ------------------------------------------------------------------

listener_pins_explicit_http_phase_budget_test() ->
    %% D4（H-A）：TransportOpts 显式钉住 1024 × 10（= 10,240/listener）。
    %% 键必须在 map 顶层：ranch normalize_opts 只把 list 当 socket_opts。
    S = code_bin("src/imboy_app.erl"),
    ?assertNotEqual(nomatch, binary:match(S, <<"max_connections => 1024">>)),
    ?assertNotEqual(nomatch, binary:match(S, <<"num_conns_sups => 10">>)),
    ok.

websocket_options_dead_keys_removed_test() ->
    %% 曾有的 num_acceptors/max_connections => infinity 是流级死配置，
    %% T3 防回归：websocket_handler 中不得再现
    S = code_bin("src/api/websocket_handler.erl"),
    ?assertEqual(nomatch, binary:match(S, <<"num_acceptors">>)),
    ?assertEqual(nomatch, binary:match(S, <<"max_connections">>)),
    ok.

%% ------------------------------------------------------------------
%% T4：env 注入链
%% ------------------------------------------------------------------

env_wiring_for_max_ws_connections_test() ->
    EnvS = code_bin("src/lib/imboy_env.erl"),
    ?assertNotEqual(nomatch, binary:match(EnvS, <<"IMBOY_MAX_WS_CONNECTIONS">>)),
    AdmS = code_bin("src/lib/ws_admission.erl"),
    %% 启动期 fail-fast 校验存在（不允许静默回退为无限制）
    ?assertNotEqual(nomatch, binary:match(AdmS, <<"validate_config">>)),
    ?assertNotEqual(nomatch, binary:match(AdmS, <<"invalid_config">>)),
    ok.

%% ------------------------------------------------------------------
%% T7 静态面：compose 注入与告警指标按名对账
%% ------------------------------------------------------------------

compose_files_use_injection_not_dead_erl_max_ports_test() ->
    Community = code_bin("deploy/docker-compose.community.yml"),
    Local = code_bin("docker-compose.yml"),
    %% D7：死配置 ERL_MAX_PORTS 已删除
    ?assertEqual(nomatch, binary:match(Community, <<"ERL_MAX_PORTS">>)),
    ?assertEqual(nomatch, binary:match(Local, <<"ERL_MAX_PORTS">>)),
    %% 注入面：ERL_FLAGS 经 install.sh 重算透传；WS 上限直传
    ?assertNotEqual(nomatch, binary:match(Community, <<"ERL_FLAGS: ${IMBOY_ERL_FLAGS:-}">>)),
    ?assertNotEqual(nomatch, binary:match(Community, <<"IMBOY_MAX_WS_CONNECTIONS">>)),
    ?assertNotEqual(nomatch, binary:match(Local, <<"IMBOY_MAX_WS_CONNECTIONS">>)),
    ok.

install_sh_capacity_block_present_test() ->
    S = code_bin("deploy/install.sh"),
    ?assertNotEqual(nomatch, binary:match(S, <<"ensure_capacity_vars">>)),
    ?assertNotEqual(nomatch, binary:match(S, <<"IMBOY_ERL_FLAGS">>)),
    ok.

alert_watermark_names_align_with_metrics_test() ->
    %% T6/B-26 对账纪律：告警 expr 引用的指标名必须真实存在
    Alerts = code_bin("deploy/prometheus/rules/imboy-alerts.yml"),
    Metrics = code_bin("src/api/metrics_handler.erl"),
    ?assertNotEqual(nomatch, binary:match(Alerts, <<"ImBoyWSAdmissionHigh">>)),
    ?assertNotEqual(nomatch, binary:match(Metrics, <<"imboy_ws_admission_current">>)),
    ?assertNotEqual(nomatch, binary:match(Metrics, <<"imboy_ws_admission_limit">>)),
    %% 换源：不再把 ranch active_connections 当已建立 WS 数
    ?assertEqual(nomatch, binary:match(Metrics, <<"ranch:info(imboy_listener)">>)),
    ok.

%% ------------------------------------------------------------------
%% helpers
%% ------------------------------------------------------------------

repo_file(Rel) ->
    {ok, Bin} = file:read_file(filename:join(repo_root(), Rel)),
    Bin.

%% 读文件并剔除注释行（erlang %%/%、shell/yml/args_file #）：
%% T1/T3 断言的是可执行配置，不是文档注释——注释里提到旧串（勘误说明）
%% 不应触发防回归断言。
code_bin(Rel) ->
    Raw = repo_file(Rel),
    Lines = binary:split(Raw, <<"\n">>, [global]),
    Kept = [L || L <- Lines, not is_comment_line(L)],
    iolist_to_binary(lists:join(<<"\n">>, Kept)).

is_comment_line(<<>>) ->
    false;
is_comment_line(Line) ->
    Trimmed = trim_leading_ws(Line),
    case Trimmed of
        <<>> -> false;
        <<"%%", _/binary>> -> true;
        <<"%", _/binary>> -> true;
        <<"#", _/binary>> -> true;
        _ -> false
    end.

trim_leading_ws(<<" ", Rest/binary>>) -> trim_leading_ws(Rest);
trim_leading_ws(<<"\t", Rest/binary>>) -> trim_leading_ws(Rest);
trim_leading_ws(Other) -> Other.

repo_root() ->
    %% Source contracts also run in exported/container workspaces without Git.
    %% Locate the source tree itself, independently of Git's ownership/config.
    {ok, Cwd} = file:get_cwd(),
    repo_root(Cwd).

repo_root(Dir) ->
    case filelib:is_file(filename:join(Dir, "config/vm.args")) andalso
        filelib:is_file(filename:join(Dir, "src/imboy_app.erl"))
    of
        true -> Dir;
        false ->
            case filename:dirname(Dir) of
                Dir -> erlang:error({repo_root_not_found, Dir});
                Parent -> repo_root(Parent)
            end
    end.

%% 提取文件中所有 `+P <int>` / `+Q <int>` 形态的标志值（Flag 自带 + 号）。
%% binary:matches 一次取全部匹配位点，再逐字节取数字——不做 sub-binary
%% 偏移算术（先前实现在此产生 badarg，见 r0-capacity-test-results）。
flag_values(Bin, Flag) ->
    Pat = <<(iolist_to_binary(Flag))/binary, " ">>,
    Ms = binary:matches(Bin, Pat),
    Values = [take_digits_at(Bin, Pos + Len) || {Pos, Len} <- Ms],
    [binary_to_integer(D) || D <- Values, byte_size(D) > 0].

take_digits_at(Bin, Pos) ->
    take_digits_at(Bin, Pos, []).

take_digits_at(Bin, Pos, Acc) when Pos < byte_size(Bin) ->
    case binary:at(Bin, Pos) of
        C when C >= $0, C =< $9 -> take_digits_at(Bin, Pos + 1, [C | Acc]);
        _ -> iolist_to_binary(lists:reverse(Acc))
    end;
take_digits_at(_Bin, _Pos, Acc) ->
    iolist_to_binary(lists:reverse(Acc)).

is_pow2(N) when is_integer(N), N > 0 ->
    N band (N - 1) =:= 0.

next_pow2(N) when N =< 1 ->
    1;
next_pow2(N) ->
    next_pow2(N, 1).

next_pow2(N, P) when P >= N ->
    P;
next_pow2(N, P) ->
    next_pow2(N, P * 2).

approved_ws_default_from_source() ->
    S = code_bin("src/lib/ws_admission.erl"),
    {Start, Len} = binary:match(S, <<"-define(DEFAULT_MAX_WS_CONNECTIONS, ">>),
    binary_to_integer(take_digits_at(S, Start + Len)).
