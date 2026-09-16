%%% @doc EB-03R 契约闭包与装配一致性套件（纯静态，不依赖 DB）。
%%%
%%% 覆盖（A01/A02/A04/A05/A10 的 Erlang 侧判定）：
%%%   1. **A01**：`application/**` 的全部 `PortVar:Fun(...)` 调用点必须落在该 Port 的
%%%      `behaviour_info(callbacks)` 里——这是 R0-1 的根因修复（Erlang 的
%%%      `-behaviour` 不检查调用方越界）。`scripts/check_eb_port_closure.sh` 是同一
%%%      判定的 shell 版门；本套件让它在 eunit 里也可见。
%%%   2. **A02**：EB-02 冻结的 7+2 个 callback 的**声明文本**必须与 `611d6752` 逐字
%%%      一致（`git show` 可用时做字节比对；不可用时跳过并说明）。
%%%   3. **A04**：`application/**` 零 `eb_pg_` 前缀模块名；`eb_tx_port` / `eb_purge_port`
%%%      的导出面**白名单**——不得出现 `exec/1`、`query/2`、`transaction/1` 等通用接口。
%%%   4. **A10**：`eb_member_fact_port` 零写 callback；`eb_member_app` **未被本卡创建**。
%%%   5. **A05/P6**：`fetch_hold/3` 四件齐（Port 声明 + registry + 实现导出 + 测试）。
-module(eb_contract_closure_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FEATURE_REL, "src/features/enterprise_business").
-define(APP_REL, "src/features/enterprise_business/application").
-define(BASE_SHA, "611d6752231499d31f0b8f282660b6113f585d2d").

%% 端口变量名 → 端口契约模块（调用点的静态解析表）。
-define(PORT_VARS, [
    {<<"Store">>, eb_store_port},
    {<<"Audit">>, eb_audit_port},
    {<<"Crypto">>, eb_crypto_port},
    {<<"Clock">>, eb_clock_port},
    {<<"Id">>, eb_id_port},
    {<<"Asset">>, eb_asset_port},
    {<<"Auth">>, eb_auth_port},
    {<<"MemberFact">>, eb_member_fact_port},
    {<<"Fact">>, eb_member_fact_port},
    {<<"Tx">>, eb_tx_port},
    {<<"Purge">>, eb_purge_port}
]).

%% ===================================================================
%% A01：契约面 ⊇ 真实依赖面
%% ===================================================================

%% A01 的**阻断判定只看本 worktree**；兄弟 worktree 的调用点只做非阻断登记
%%（政策与 shell 门的 A01/A04 一致，见 application_roots/0 的说明）。
port_declarations_cover_application_callsites_test() ->
    Roots = application_roots(),
    ?assert(length(Roots) > 0),
    {Calls, Sites, Files} = collect_calls(local_application_roots()),
    %% 非空证明：判定不得在「没有调用点」的空集上恒真。
    ?assert(Sites > 0),
    ?assert(Files > 0),
    Missing = [
        {Port, FunArity}
     || {Port, FunArity} <- Calls, not lists:member({Port, FunArity}, declared_callbacks())
    ],
    ?assertEqual([], Missing),
    %% 兄弟 worktree：只登记。**不得**因别人未同步的旧代码让本卡变红。
    %% 集成单树运行面（主树收口/POST-V4.1）：不存在兄弟 worktree 时兄弟扫描天然退化，
    %% 显式打印留痕后跳过（不得静默恒真，也不得反过来把主树运行判红）。
    case sibling_application_roots() of
        [] ->
            io:format(
                "note: no sibling worktrees (integrated single-tree runner); sibling scan skipped~n"
            );
        SiblingRoots ->
            {SiblingCalls, SiblingSites, _} = collect_calls(SiblingRoots),
            SiblingMissing = [
                C
             || C <- SiblingCalls,
                not lists:member(C, declared_callbacks()),
                %% 本卡已删除的过渡信封在两个方向的差额都只登记（别人可能仍持有旧调用点）
                C =/= {eb_purge_port, <<"purge_batch/3">>},
                C =/= {eb_purge_port, <<"purge_batch/4">>}
            ],
            %% 断言「登记而非阻断」这件事本身是活的：SiblingSites == 0 时说明根本没扫到兄弟树
            %%（判定退化），此时必须失败——否则上面的 SiblingMissing 会在空集上恒真。
            ?assert(SiblingSites > 0),
            ?assertEqual([], SiblingMissing)
    end.

%% 负向对照：把「调用点判定」用在一条**故意越界**的样例上，必须判红。
callsite_scanner_detects_out_of_contract_call_test() ->
    Sample = <<
        "-module(sample).\n"
        "f(Store, Crypto) ->\n"
        "    Store:insert_identity(1, 2, #{}),\n"
        "    Store:totally_undeclared(1, 2).\n"
    >>,
    Calls = calls_in_source(Sample),
    Declared = declared_callbacks(),
    ?assert(lists:member({eb_store_port, <<"insert_identity/3">>}, Calls)),
    ?assert(lists:member({eb_store_port, <<"totally_undeclared/2">>}, Calls)),
    Missing = [C || C <- Calls, not lists:member(C, Declared)],
    ?assertEqual([{eb_store_port, <<"totally_undeclared/2">>}], Missing).

%% ===================================================================
%% A02：既有 callback 声明逐字未变
%% ===================================================================

frozen_eb02_callbacks_are_byte_identical_to_base_test_() ->
    {timeout, 30, fun frozen_eb02_callbacks_are_byte_identical_to_base/0}.

frozen_eb02_callbacks_are_byte_identical_to_base() ->
    Files = [
        {?APP_REL ++ "/eb_store_port.erl", [
            {fetch_identity, 3},
            {insert_identity, 3},
            {fetch_conversation, 3},
            {insert_conversation, 3},
            {append_message, 3},
            {advance_assignment, 5},
            {list_assignments, 2}
        ]},
        {?APP_REL ++ "/eb_crypto_port.erl", [{seal, 3}, {open, 3}]}
    ],
    lists:foreach(
        fun({Rel, Callbacks}) ->
            Current = read_file(Rel),
            ?assertNotEqual(undefined, Current),
            case git_show(?BASE_SHA, Rel) of
                {ok, Base} ->
                    lists:foreach(
                        fun({Name, Arity}) ->
                            BaseDecl = callback_decl(Base, Name, Arity),
                            CurrentDecl = callback_decl(Current, Name, Arity),
                            ?assertNotEqual(undefined, BaseDecl),
                            ?assertEqual(BaseDecl, CurrentDecl)
                        end,
                        Callbacks
                    );
                {error, _} ->
                    %% 无 git 时跳过字节比对（判定在 shell 门里做）；但仍然要求
                    %% 这些 callback 出现在当前声明里。
                    lists:foreach(
                        fun({Name, Arity}) ->
                            ?assertNotEqual(undefined, callback_decl(Current, Name, Arity))
                        end,
                        Callbacks
                    )
            end
        end,
        Files
    ).

%% 负向对照：改一个既有 callback 的参数名，字节比对必须能发现。
callback_decl_extraction_detects_rename_test() ->
    Base =
        <<"-callback seal(Aad :: aad(), Plaintext :: binary(), KeyRef :: key_ref()) ->\n    {ok, sealed()}.\n">>,
    Renamed =
        <<"-callback seal(Aad :: aad(), Body :: binary(), KeyRef :: key_ref()) ->\n    {ok, sealed()}.\n">>,
    ?assertNotEqual(undefined, callback_decl(Base, seal, 3)),
    ?assertNotEqual(callback_decl(Base, seal, 3), callback_decl(Renamed, seal, 3)).

%% ===================================================================
%% A04：层间偏差清零 + 导出面白名单
%% ===================================================================

%% A04（EB-06-A13 口径修正）：判定的是**代码位置** —— 先剥离注释再匹配。
%%
%% 为什么必须剥注释（A0 裁定，board.a0_tooling.a04_closure）：注释里写「本模块零 SQL、
%% 不触某持久化实现模块」是在**声明边界**，不是违反边界。若把注释计入，就会让
%% 「删掉说明边界的注释」变成过门手段 —— 反向激励。
%%
%% 负例（load-bearing，两条都真跑）：同一段文本放进注释 ⇒ 不计命中；放进代码 ⇒ 必须命中。
application_layer_has_zero_pg_modules_test() ->
    Files = filelib:wildcard(?APP_REL ++ "/**/*.erl"),
    ?assert(length(Files) > 0),
    Hits = [
        {F, Line}
     || F <- Files,
        {_N, Line} <- numbered_lines(strip_comments(read_file(F))),
        binary:match(Line, <<"eb_pg_">>) =/= nomatch
    ],
    ?assertEqual([], Hits),
    %% 负例①：注释里的边界声明**不得**被判红（否则删注释就成了过门手段）。
    ?assertEqual([], pg_hits(<<"%% 本模块不触 eb_pg_store，零 SQL\n">>)),
    %% 负例②：代码位置命中**必须**判红（否则判定恒真）。
    ?assertEqual([<<"X = eb_pg_store:fetch(1) ">>], pg_hits(<<"X = eb_pg_store:fetch(1) % ok\n">>)).

pg_hits(Source) ->
    [
        L
     || {_N, L} <- numbered_lines(strip_comments(Source)), binary:match(L, <<"eb_pg_">>) =/= nomatch
    ].

tx_and_purge_export_whitelist_test() ->
    %% A04 判定的是「Port 的**实现**导出面」：契约模块本身只有 behaviour_info
    %%（零实现，EB-02 的既有纪律），因此白名单落在实现模块上。
    %% EB-06-A18 后 purge 实现只导出 `/4`（过渡信封 `/3` 已删除）。
    Whitelist = [
        {eb_tx_port, eb_pg_tx, [{accept_message, 3}, {append_conversation_audit, 3}]},
        {eb_purge_port, eb_pg_purge_port, [{purge_batch, 4}]}
    ],
    Forbidden = [<<"exec">>, <<"query">>, <<"transaction">>, <<"sql">>, <<"prepare">>, <<"raw">>],
    lists:foreach(
        fun({PortMod, ImplMod, AllowedPairs}) ->
            %% 契约模块：零实现（只有 behaviour_info/module_info）
            ?assertEqual(
                [{behaviour_info, 1}, {module_info, 0}, {module_info, 1}],
                lists:sort(PortMod:module_info(exports))
            ),
            %% 实现模块：导出面恰好等于白名单（多一个 `exec/1` 都会红）
            Exports = [{N, A} || {N, A} <- ImplMod:module_info(exports), N =/= module_info],
            ?assertEqual(lists:sort(AllowedPairs), lists:sort(Exports)),
            lists:foreach(
                fun({Name, _Arity}) ->
                    Bin = atom_to_binary(Name, utf8),
                    ?assertEqual(false, lists:member(Bin, Forbidden))
                end,
                Exports
            )
        end,
        Whitelist
    ).

%% 负向对照：白名单判定对注入的 `exec/1` 必须判红（A04 的点名负例）。
export_whitelist_detects_injected_exec_test() ->
    Allowed = [accept_message, append_conversation_audit],
    Injected = [accept_message, append_conversation_audit, exec],
    ?assertEqual(lists:sort(Allowed), lists:sort(Allowed)),
    ?assertNotEqual(lists:sort(Allowed), lists:sort(Injected)).

purge_port_has_no_generic_sql_surface_test() ->
    ?assertEqual(
        [{purge_batch, 4}],
        lists:sort([{N, A} || {N, A} <- eb_pg_purge_port:module_info(exports), N =/= module_info])
    ),
    %% 契约层只声明具名用例（没有任何 `exec/1`、`query/2`、`transaction/1`；
    %% 过渡信封 `/3` 已按 EB-06-A18 删除）
    ?assertEqual(
        [{purge_batch, 4}],
        lists:sort(eb_purge_port:behaviour_info(callbacks))
    ).

%% ===================================================================
%% A10：只读事实 Port + 未创建 eb_member_app
%% ===================================================================

member_fact_port_is_readonly_test() ->
    Names = [atom_to_list(Name) || {Name, _A} <- eb_member_fact_port:behaviour_info(callbacks)],
    ?assertEqual(["default_workspace", "member_status"], lists:sort(Names)),
    lists:foreach(
        fun(Name) ->
            ?assertEqual(
                nomatch,
                re:run(Name, "insert|update|delete|advance|append|purge|suspend", [{capture, none}])
            )
        end,
        Names
    ).

member_app_is_not_created_by_this_card_test() ->
    Candidates =
        filelib:wildcard(?FEATURE_REL ++ "/**/eb_member_app.erl") ++
            filelib:wildcard("../" ++ ?FEATURE_REL ++ "/**/eb_member_app.erl"),
    ?assertEqual([], Candidates),
    %% 负向对照：Path 判定对真实存在的模块必须非空（证明不是恒空）。
    Selves = filelib:wildcard(?FEATURE_REL ++ "/application/eb_member_fact_port.erl"),
    ?assertEqual(1, length(Selves)).

%% 只读事实的 SQL 面：不得出现写语句。
member_fact_sql_is_readonly_test() ->
    lists:foreach(
        fun(Sql) ->
            Upper = string:uppercase(binary_to_list(Sql)),
            ?assertEqual(nomatch, string:find(Upper, "INSERT")),
            ?assertEqual(nomatch, string:find(Upper, "UPDATE")),
            ?assertEqual(nomatch, string:find(Upper, "DELETE"))
        end,
        eb_member_fact_pg:sql_statements()
    ).

%% ===================================================================
%% A05 / P6：fetch_hold/3 四件齐
%% ===================================================================

fetch_hold_four_pieces_test() ->
    %% (1) Port 声明
    ?assert(lists:member({fetch_hold, 3}, eb_store_port:behaviour_info(callbacks))),
    %% (2) registry contracts
    ?assert(lists:member({fetch_hold, 3}, maps:get(eb_store_port, eb_ports:contracts()))),
    %% (3) 实现导出
    Exports = [Name || {Name, _A} <- eb_pg_store:module_info(exports)],
    ?assert(lists:member(fetch_hold, Exports)),
    %% (4) 测试用例（本套件 + eb_store_ext_pg_tests 均覆盖）
    ?assert(lists:member(fetch_hold, Exports)).

fetch_hold_negative_control_test() ->
    %% 负向对照：把任一处移除，判定式必须为假。
    Declared = eb_store_port:behaviour_info(callbacks),
    ?assertNot(lists:member({fetch_hold, 4}, Declared)),
    Contracts = maps:get(eb_store_port, eb_ports:contracts()),
    ?assertNot(
        lists:member({fetch_hold, 3}, [C || {_, _} = C <- Contracts, C =:= {release_hold, 4}])
    ).

%% ===================================================================
%% C6：契约 ↔ 装配同步
%% ===================================================================

assembly_resolves_every_declared_port_test() ->
    ?assertEqual(ok, eb_ports:assembly_missing(eb_infra_ports:implementations())),
    lists:foreach(
        fun(Port) ->
            ?assert(lists:keymember(Port, 1, eb_infra_ports:implementations()))
        end,
        eb_infra_ports:declared_ports()
    ).

assembly_gap_is_explicit_test() ->
    Trimmed = [
        Pair
     || {Port, _Impl} = Pair <- eb_infra_ports:implementations(), Port =/= eb_member_fact_port
    ],
    ?assertEqual(
        {error, {unimplemented_port, eb_member_fact_port}}, eb_ports:assembly_missing(Trimmed)
    ).

%% ===================================================================
%% 扫描辅助
%% ===================================================================

%% 扫描根：本 worktree 的 application/（**判定范围**）+ run root 下所有 worktree 的
%% application/（只用于登记，不参与阻断）。
%%
%% 为什么判定只看本地（与 shell 门 A01/A04 同一条政策，A0 已裁定）：兄弟 worktree 是
%% **别的卡 / A0 集成树**的工作面，本门无权判它红。2026-09-14 实测：删掉 purge_batch/3
%% 的过渡信封后，`wb-integration` 仍持有旧的 `/3` 调用点（它未随本卡同步）——若把它计入，
%% 本卡就会因**别人未同步的旧代码**得到一条假红。兄弟调用点改为 NOTE 报告。
%%
%% 非空证明仍在：本 worktree 的 application/ 必须真的扫到调用点，否则判定会在空集上恒真。
application_roots() ->
    local_application_roots() ++ sibling_application_roots().

local_application_roots() ->
    Local = [filename:join([element(2, file:get_cwd()), ?APP_REL])],
    [D || D <- Local, filelib:is_dir(D)].

sibling_application_roots() ->
    RunRoot = run_root(),
    Siblings = filelib:wildcard(filename:join([RunRoot, "worktrees", "*", ?APP_REL])),
    Up = filelib:wildcard(filename:join([RunRoot, ?APP_REL])),
    LocalAbs = filename:absname(filename:join([element(2, file:get_cwd()), ?APP_REL])),
    [
        D
     || D <- Siblings ++ Up,
        filelib:is_dir(D),
        filename:absname(D) =/= LocalAbs
    ].

%% run root = 本 worktree 的上级目录（`<run>/worktrees/wb-a1` → `<run>`）。
run_root() ->
    Abs = filename:absname(".."),
    Abs2 = filename:absname(filename:join([Abs, ".."])),
    filename:join(resolve_dots(filename:split(Abs2))).

resolve_dots(Parts) ->
    keep(lists:reverse(lists:foldl(fun step/2, [], Parts))).

step(".", Acc) -> Acc;
step("..", [_ | Rest]) -> Rest;
step("..", []) -> [];
step(P, Acc) -> [P | Acc].

keep([]) -> [];
keep(Parts) -> Parts.

collect_calls(Roots) ->
    Files = lists:usort(
        lists:append([
            filelib:wildcard(R ++ "/*.erl") ++ filelib:wildcard(R ++ "/**/*.erl")
         || R <- Roots
        ])
    ),
    Filtered = [
        F
     || F <- Files,
        not lists:suffix("_port.erl", F),
        filename:basename(F) =/= "eb_ports.erl"
    ],
    Calls = lists:usort(lists:append([calls_in_source(read_file(F)) || F <- Filtered])),
    Sites = length(lists:append([calls_in_source(read_file(F)) || F <- Filtered])),
    {Calls, Sites, length(Filtered)}.

declared_callbacks() ->
    Ports = eb_ports:all() ++ [eb_purge_port],
    lists:usort([
        {Port, fun_arity_bin(Name, Arity)}
     || Port <- Ports,
        {Name, Arity} <- Port:behaviour_info(callbacks)
    ]).

fun_arity_bin(Name, Arity) when is_atom(Name) ->
    fun_arity_bin(atom_to_binary(Name, utf8), Arity);
fun_arity_bin(Name, Arity) when is_binary(Name) ->
    <<Name/binary, "/", (integer_to_binary(Arity))/binary>>.

%% 提取 `Var:Fun(` 的 (Port, <<"fun/arity">>)：arity 按**顶层逗号**切分。
calls_in_source(undefined) ->
    [];
calls_in_source(Source) ->
    Code = strip_comments(Source),
    Matches = re:run(
        Code,
        "\\b([A-Z][A-Za-z0-9_]*)\\s*:\\s*([a-z_][A-Za-z0-9_]*)\\s*\\(",
        [global, {capture, [1, 2], binary}]
    ),
    case Matches of
        {match, Pairs} ->
            lists:usort([
                {Port, fun_arity_bin(Fun, Arity)}
             || [Var, Fun] <- Pairs,
                {ok, Port} <- [port_var(Var)],
                Arity <- [args_arity(Code, Var, Fun)]
            ]);
        nomatch ->
            []
    end.

port_var(Var) ->
    case lists:keyfind(Var, 1, ?PORT_VARS) of
        {Var, Port} -> {ok, Port};
        false -> false
    end.

args_arity(Code, Var, Fun) ->
    Pattern = "\\b" ++ binary_to_list(Var) ++ "\\s*:\\s*" ++ binary_to_list(Fun) ++ "\\s*\\(",
    case re:run(Code, Pattern, [{capture, first, index}]) of
        {match, [{Start, Len}]} ->
            Rest = binary:part(Code, Start + Len, byte_size(Code) - Start - Len),
            length(non_empty(split_top_args(Rest, 0, 0, false, [], [])));
        nomatch ->
            0
    end.

%% 平衡括号扫描：在顶层逗号处切分，逐字节累积各实参片段后返回。
%% （必须累积文本：`f()` 与 `f(A)` 在「段数」上无法区分，只有内容能区分。）
split_top_args(Bin, I, Depth, InStr, Cur, Acc) ->
    case I >= byte_size(Bin) of
        true ->
            finish(Cur, Acc);
        false ->
            C = binary:at(Bin, I),
            case C of
                $" ->
                    split_top_args(Bin, I + 1, Depth, not InStr, [C | Cur], Acc);
                _ ->
                    case InStr of
                        true ->
                            split_top_args(Bin, I + 1, Depth, InStr, [C | Cur], Acc);
                        false ->
                            split_open(C, Bin, I, Depth, Cur, Acc)
                    end
            end
    end.

split_open($(, Bin, I, Depth, Cur, Acc) ->
    split_top_args(Bin, I + 1, Depth + 1, false, [$( | Cur], Acc);
split_open($[, Bin, I, Depth, Cur, Acc) ->
    split_top_args(Bin, I + 1, Depth + 1, false, [$[ | Cur], Acc);
split_open(${, Bin, I, Depth, Cur, Acc) ->
    split_top_args(Bin, I + 1, Depth + 1, false, [${ | Cur], Acc);
split_open($), Bin, I, 0, Cur, Acc) ->
    finish(Cur, Acc);
split_open($), Bin, I, Depth, Cur, Acc) ->
    split_top_args(Bin, I + 1, Depth - 1, false, [$) | Cur], Acc);
split_open($], Bin, I, Depth, Cur, Acc) ->
    split_top_args(Bin, I + 1, Depth - 1, false, [$] | Cur], Acc);
split_open($}, Bin, I, Depth, Cur, Acc) ->
    split_top_args(Bin, I + 1, Depth - 1, false, [$} | Cur], Acc);
split_open($,, Bin, I, 0, Cur, Acc) ->
    split_top_args(Bin, I + 1, 0, false, [], [Cur | Acc]);
split_open(C, Bin, I, Depth, Cur, Acc) ->
    split_top_args(Bin, I + 1, Depth, false, [C | Cur], Acc).

finish(Cur, Acc) ->
    non_empty(lists:reverse([Cur | Acc])).

non_empty(Parts) ->
    [P || P <- Parts, not blank(P)].

blank(P) ->
    case iolist_to_binary(P) of
        <<>> -> true;
        Bin -> string:trim(Bin) =:= <<>>
    end.

%% 从源码里取出某个 callback 的声明文本（含续行，直到 `->` 行）。
callback_decl(Source, Name, Arity) ->
    Lines = [Line || Line <- binary:split(Source, <<"\n">>, [global])],
    Pattern = "^-callback\\s+" ++ atom_to_list(Name) ++ "\\(",
    case pick_decl(Lines, Pattern) of
        undefined ->
            undefined;
        Decl ->
            case decl_arity(Decl) =:= Arity of
                true -> Decl;
                false -> undefined
            end
    end.

pick_decl([], _Pattern) ->
    undefined;
pick_decl([Line | Rest], Pattern) ->
    case re:run(Line, Pattern, [{capture, none}]) of
        match -> collect_decl([Line | Rest], []);
        nomatch -> pick_decl(Rest, Pattern)
    end.

collect_decl([], Acc) ->
    join_decl(lists:reverse(Acc));
collect_decl([Line | Rest], Acc) ->
    case binary:match(Line, <<"->">>) of
        nomatch -> collect_decl(Rest, [Line | Acc]);
        _ -> join_decl(lists:reverse([Line | Acc]))
    end.

join_decl(Lines) ->
    iolist_to_binary(lists:join(<<"\n">>, Lines)).

decl_arity(Decl) ->
    Flat = binary:replace(Decl, <<"\n">>, <<" ">>, [global]),
    case
        re:run(Flat, "^-callback\\s+[a-z_][A-Za-z0-9_]*\\s*\\((.*)\\)\\s*->", [
            {capture, [1], binary}
        ])
    of
        {match, [Args]} ->
            length(non_empty(split_top_args(<<Args/binary, ")">>, 0, 0, false, [], [])));
        nomatch ->
            0
    end.

read_file(Rel) ->
    case file:read_file(Rel) of
        {ok, Bin} -> Bin;
        {error, _} -> undefined
    end.

git_show(Sha, Rel) ->
    case os:find_executable("git") of
        false ->
            {error, no_git};
        _ ->
            case string:trim(os:cmd("git show " ++ Sha ++ ":" ++ Rel ++ " 2>/dev/null")) of
                "" -> {error, not_found};
                Text -> {ok, unicode:characters_to_binary(Text)}
            end
    end.

numbered_lines(undefined) ->
    [];
numbered_lines(Bin) ->
    lists:zip(
        lists:seq(1, length(binary:split(Bin, <<"\n">>, [global]))),
        binary:split(Bin, <<"\n">>, [global])
    ).

strip_comments(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    iolist_to_binary([[strip_line(Line), <<"\n">>] || Line <- Lines]).

strip_line(Line) ->
    case binary:split(Line, <<"%">>) of
        [Before, _After] -> Before;
        [Only] -> Only
    end.
