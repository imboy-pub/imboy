%%% @doc customer_service 静态闭环自检（纯静态，不依赖 DB；镜像
%%% `test/features/enterprise_business/application/eb_contract_closure_tests.erl`
%%% 的白名单思路）。
%%%
%%% 判定（违规即红）：
%%%   1. **引用白名单**：扫描 `src/features/customer_service/**` 全部模块的
%%%      remote call（`Mod:fun(`），每个被引用模块必须属于——
%%%        a) customer_service 自己的模块；或
%%%        b) `cs_ports:external_reference_whitelist()`（当前仅
%%%          `enterprise_business_facade`，EB-D08 / A03 的唯一跨 Feature 入口）；或
%%%        c) OTP / core lib 白名单（`otp_lib_whitelist/0`）。
%%%      任何 `eb_*` 内层模块（application/infrastructure）出现在客服代码里
%%%      都会因不在白名单而变红——消息/客户/附件不得绕过 facade（A03）。
%%%   2. **负向对照**：把白名单判定用在故意越界的样例上必须判红（判定有牙齿）。
%%%   3. **契约同步**：`cs_ports:contracts()` 与 port 模块的
%%%      `behaviour_info(callbacks)` 逐字一致；装配覆盖全部端口。
%%%   4. **facade 纪律**：`customer_service_facade` 的委派目标 ⊆
%%%      `cs_ports:facade_targets()`（application 层）。
%%%   5. **A03 正向证据**：`cs_session_app` 源码确实含
%%%      `enterprise_business_facade:append_message` 调用点。
-module(cs_closure_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FEATURE_REL, "src/features/customer_service").
-define(FACADE_MOD, customer_service_facade).
-define(APP_FACADE_REL, ?FEATURE_REL ++ "/application/session/cs_session_app.erl").

%% ===================================================================
%% 1. 引用白名单（cs 全模块 remote call 判定）
%% ===================================================================

all_remote_calls_within_whitelist_test() ->
    Modules = cs_modules(),
    ?assert(length(Modules) > 0),
    Refs = lists:usort(lists:append([remote_calls_in_module(M) || M <- Modules])),
    ?assert(length(Refs) > 0),
    Allowed = allowed_modules(Modules),
    Offenders = [R || R <- Refs, not lists:member(R, Allowed)],
    ?assertEqual([], Offenders).

%% 负向对照 1：引用 eb 内层模块必须判红（A03 的牙齿）
eb_inner_module_reference_is_flagged_test() ->
    Sample = <<
        "-module(sample).\n"
        "f() -> eb_pg_store:fetch_conversation(1, 2, 3).\n"
    >>,
    Refs = calls_in_source(Sample),
    ?assertEqual([eb_pg_store], Refs),
    Allowed = allowed_modules(cs_modules()),
    ?assertNot(lists:member(eb_pg_store, Allowed)),
    ?assertEqual([eb_pg_store], [R || R <- Refs, not lists:member(R, Allowed)]).

%% 负向对照 2：判定不是恒空——真实 cs 模块集与白名单都非空且相交
whitelist_and_module_set_are_alive_test() ->
    Modules = cs_modules(),
    %% 自己引用自己（如 cs_app_support 被 app 模块引用）应被放行
    ?assert(lists:member(cs_app_support, allowed_modules(Modules))),
    ?assert(lists:member(enterprise_business_facade, allowed_modules(Modules))),
    ?assert(lists:member(elib_pg, allowed_modules(Modules))).

%% ===================================================================
%% 3. 契约同步
%% ===================================================================

port_contracts_match_behaviour_info_test() ->
    Contracts = cs_ports:contracts(),
    lists:foreach(
        fun(Port) ->
            Declared = Port:behaviour_info(callbacks),
            Frozen = maps:get(Port, Contracts, missing),
            ?assertEqual(lists:sort(Declared), lists:sort(Frozen))
        end,
        cs_ports:all()
    ).

assembly_covers_every_port_test() ->
    Assembled = [Port || {Port, _Impl} <- cs_infra_ports:implementations()],
    lists:foreach(
        fun(Port) -> ?assert(lists:member(Port, Assembled)) end,
        cs_ports:all()
    ),
    %% 实现模块确实声明了对应 behaviour（编译期已查，这里对源码再静态取证）
    {cs_store_port, StoreImpl} = lists:keyfind(cs_store_port, 1, cs_infra_ports:implementations()),
    {cs_id_port, IdImpl} = lists:keyfind(cs_id_port, 1, cs_infra_ports:implementations()),
    ?assert(declares_behaviour(StoreImpl, cs_store_port)),
    ?assert(declares_behaviour(IdImpl, cs_id_port)).

declares_behaviour(ImplMod, PortMod) ->
    Source = read_file(module_source_rel(ImplMod)),
    Source =/= undefined andalso
        match ==
            re:run(
                strip_comments(Source),
                <<"^-behaviour\\(\\s*", (atom_to_binary(PortMod, utf8))/binary, "\\s*\\)">>,
                [{capture, none}, multiline]
            ).

%% ===================================================================
%% 4. facade 纪律
%% ===================================================================

facade_delegates_only_to_declared_targets_test() ->
    Refs = remote_calls_in_module(?FACADE_MOD),
    ?assert(length(Refs) > 0),
    Offenders = [R || R <- Refs, not lists:member(R, cs_ports:facade_targets())],
    ?assertEqual([], Offenders).

facade_reference_whitelist_is_empty_test() ->
    ?assertEqual([], cs_ports:facade_reference_whitelist()).

%% ===================================================================
%% 5. A03 正向证据：消息唯一入口是 enterprise facade
%% ===================================================================

session_app_calls_eb_facade_for_messages_test() ->
    Source = read_file(?APP_FACADE_REL),
    ?assertNotEqual(undefined, Source),
    Code = strip_comments(Source),
    ?assertMatch(
        {match, _},
        re:run(Code, <<"enterprise_business_facade\\s*:\\s*append_message\\s*\\(">>)
    ),
    %% 且 cs_session_app 的全部跨单元引用恰为 enterprise_business_facade
    %%（"eb_" 前缀的内层模块一个都没有；唯一跨 Feature 引用就是 facade）
    Refs = remote_calls_in_module(cs_session_app),
    CrossFeature = [
        R
     || R <- Refs,
        lists:prefix("eb_", atom_to_list(R)) orelse R =:= enterprise_business_facade
    ],
    ?assertEqual([enterprise_business_facade], CrossFeature).

%% ===================================================================
%% 扫描辅助
%% ===================================================================

cs_modules() ->
    Files = filelib:wildcard(?FEATURE_REL ++ "/**/*.erl"),
    lists:usort([module_of(read_file(F)) || F <- Files, read_file(F) =/= undefined]).

module_of(Source) ->
    case
        re:run(Source, "^-module\\(([a-z][a-z0-9_]*)\\)", [
            {capture, [1], binary}, multiline
        ])
    of
        {match, [ModBin]} -> binary_to_atom(ModBin, utf8);
        nomatch -> unknown_module
    end.

allowed_modules(CsModules) ->
    CsModules ++
        cs_ports:external_reference_whitelist() ++
        otp_lib_whitelist().

%% OTP / core lib 白名单（`src/lib` 与 OTP stdlib；不含任何 feature 模块）。
%% interfaces 层合法依赖（cowboy 绑定 / 响应信封 / 编码 / 只读缓存 / 表单解析）
%% 一并在册：cs_http / cs_auth / *_handler 的远程调用面，缺项会导致存量误红
%% （CS-04 E2E 轮 A0 复核发现，该套件在 CS-02 验收时未被执行过）。
otp_lib_whitelist() ->
    [
        elib_pg,
        elib_tsid,
        jsone,
        jsx,
        crypto,
        erlang,
        lists,
        maps,
        binary,
        string,
        unicode,
        io_lib,
        re,
        file,
        filelib,
        filename,
        epgsql,
        cowboy_req,
        elib_response,
        os,
        persistent_term,
        proplists
    ].

remote_calls_in_module(Mod) ->
    beam_lib_chunks(Mod, abstract_code).

beam_lib_chunks(Mod, abstract_code) ->
    %% 优先源码扫描（worktree 内有源码；beam 抽象码依赖 debug_info 不保证存在）
    Rel = module_source_rel(Mod),
    case read_file(Rel) of
        undefined -> calls_in_source(<<"">>);
        Source -> calls_in_source(Source)
    end.

module_source_rel(Mod) ->
    %% cs 模块的源码按 -module 名定位（features 树内唯一）
    Candidates = filelib:wildcard(?FEATURE_REL ++ "/**/" ++ atom_to_list(Mod) ++ ".erl"),
    case Candidates of
        [F | _] -> F;
        [] -> "/dev/null/never"
    end.

calls_in_source(Source) when is_binary(Source) ->
    Code = strip_comments(Source),
    case
        re:run(Code, "\\b([a-z][a-z0-9_]*):[a-z_][a-z0-9_]*\\s*\\(", [
            global, {capture, [1], binary}
        ])
    of
        {match, Pairs} ->
            lists:usort([binary_to_atom(M, utf8) || [M] <- Pairs]);
        nomatch ->
            []
    end.

strip_comments(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    iolist_to_binary([
        [
            case binary:split(Line, <<"%">>) of
                [Before, _] -> Before;
                [Only] -> Only
            end,
            <<"\n">>
        ]
     || Line <- Lines
    ]).

read_file(Rel) ->
    case file:read_file(Rel) of
        {ok, Bin} -> Bin;
        {error, _} -> undefined
    end.
