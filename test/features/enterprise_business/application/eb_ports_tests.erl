%%% @doc Port 扩展点与 facade 分层契约测试（EB-02-A04 核心证据）。
%%%
%%% 本套件不依赖任何 I/O，只做**语法与注册表事实**的判定：
%%%   1. eb_ports 注册表与六个 behaviour 扩展点一一对应；
%%%   2. 每个 port 的 behaviour_info(callbacks) 与 eb_ports:contracts() 声明的
%%%      冻结契约逐项一致（多/少/改都红）；
%%%   3. port 只声明扩展点，零实现（除 module_info 外无导出）、零 meck、
%%%      零 `*_repo` / `*_ds` / `elib_pg` 引用；
%%%   4. 租户作用域：store port 的每个 callback 至少 2 参（OrgId/WorkspaceId），
%%%      并含 CAS 形状的 `advance_assignment/5`；
%%%   5. crypto port 的 AAD 入参形状固定为 OrgId/WorkspaceId/ConversationId/MessageId；
%%%   6. asset port 的 callback 名不可能出现 presign/url/object_key/garage/bucket；
%%%   7. **铁律 3 契约**：解析 `enterprise_business_facade.erl` 源码，提取全部
%%%      `mod:fun(` 与 `-behaviour(mod)` 引用，断言每个被引用模块都归属
%%%      `src/features/enterprise_business/application/`（或显式白名单，本卡为空集）。
-module(eb_ports_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FEATURE_ROOT, "src/features/enterprise_business").
-define(FACADE_REL, "src/features/enterprise_business/enterprise_business_facade.erl").
-define(APPLICATION_PREFIX, "src/features/enterprise_business/application/").

%% ===================================================================
%% 1. 注册表
%% ===================================================================

ports_registry_is_frozen_test() ->
    %% EB-03R：追加 auth（C5 注册）、member_fact（P10）、tx（T1/T2）；原六个逐字未动。
    ?assertEqual(
        [
            eb_asset_port,
            eb_audit_port,
            eb_auth_port,
            eb_clock_port,
            eb_crypto_port,
            eb_id_port,
            eb_member_fact_port,
            eb_purge_port,
            eb_store_port,
            eb_tx_port
        ],
        lists:sort(eb_ports:all())
    ).

ports_registry_lookup_test() ->
    ?assertEqual(eb_store_port, eb_ports:port_module_for(store)),
    ?assertEqual(eb_crypto_port, eb_ports:port_module_for(crypto)),
    ?assertEqual(eb_clock_port, eb_ports:port_module_for(clock)),
    ?assertEqual(eb_id_port, eb_ports:port_module_for(id)),
    ?assertEqual(eb_audit_port, eb_ports:port_module_for(audit)),
    ?assertEqual(eb_asset_port, eb_ports:port_module_for(asset)),
    %% EB-03R 新增的三个域键
    ?assertEqual(eb_auth_port, eb_ports:port_module_for(auth)),
    ?assertEqual(eb_member_fact_port, eb_ports:port_module_for(member_fact)),
    ?assertEqual(eb_tx_port, eb_ports:port_module_for(tx)),
    ?assertEqual(eb_purge_port, eb_ports:port_module_for(purge)),
    ?assertEqual({error, {unknown_port, nope}}, eb_ports:port_module_for(nope)).

ports_registry_has_no_repo_or_ds_test() ->
    %% Port 是持久化扩展点，不是 *_repo / *_ds 的别名。
    lists:foreach(
        fun(Mod) ->
            Name = atom_to_list(Mod),
            ?assertEqual(false, lists:suffix("_repo", Name)),
            ?assertEqual(false, lists:suffix("_ds", Name))
        end,
        eb_ports:all()
    ).

%% ===================================================================
%% 2. behaviour 契约一致
%% ===================================================================

ports_declare_callbacks_test() ->
    lists:foreach(
        fun(Mod) ->
            Callbacks = Mod:behaviour_info(callbacks),
            ?assert(is_list(Callbacks)),
            ?assert(length(Callbacks) > 0)
        end,
        eb_ports:all()
    ).

ports_contracts_match_declared_callbacks_test() ->
    Contracts = eb_ports:contracts(),
    ?assertEqual(lists:sort(eb_ports:all()), lists:sort(maps:keys(Contracts))),
    lists:foreach(
        fun(Mod) ->
            Expected = maps:get(Mod, Contracts),
            Actual = Mod:behaviour_info(callbacks),
            ?assertEqual(lists:sort(Expected), lists:sort(Actual)),
            %% 无重复 callback（重复声明会让契约含混）。
            ?assertEqual(length(Actual), length(lists:usort(Actual)))
        end,
        eb_ports:all()
    ).

ports_have_explicit_frozen_contracts_test() ->
    %% 冻结值逐项断言：任何松动/扩参都必须在可见 diff 中发生。
    %%
    %% EB-03R：EB-02 冻结的 7/2/0/0/0/3 项**逐字保留**，本卡只在末尾追加
    %%（R0-1 的 10 个在用 callback + P1..P11 能力 + P6 fetch_hold/3、
    %%  R0-3 的真实 crypto 签名、R0-4 的 asset metadata 生命周期）。
    Contracts = eb_ports:contracts(),
    ?assertEqual(
        [
            {ack_delivery, 3},
            {advance_assignment, 5},
            {advance_offboarding_case, 6},
            {append_message, 3},
            {fetch_contact, 3},
            {fetch_conversation, 3},
            {fetch_hold, 3},
            {fetch_identity, 3},
            {fetch_message, 3},
            {fetch_offboarding_case, 3},
            {insert_assignment, 3},
            {insert_contact, 3},
            {insert_contact_assignment, 3},
            {insert_contact_identity, 3},
            {insert_conversation, 3},
            {insert_hold, 3},
            {insert_identity, 3},
            {insert_note, 3},
            {insert_offboarding_case, 3},
            {insert_offboarding_item, 3},
            {insert_policy, 3},
            {latest_policy, 3},
            {list_active_holds, 2},
            {list_assignments, 2},
            {list_contacts, 2},
            {list_conversations, 2},
            {list_identities, 2},
            {list_messages_after, 3},
            {list_offboarding_cases, 2},
            {list_offboarding_items, 3},
            {release_hold, 4},
            {update_contact, 3},
            {update_conversation_assignee, 4},
            {update_offboarding_case_counts, 6},
            {update_offboarding_item, 3}
        ],
        lists:sort(maps:get(eb_store_port, Contracts))
    ),
    %% 原 {seal,3} / {open,3} 保留，追加 R0-3 的真实签名
    ?assertEqual(
        [{open, 3}, {seal, 3}, {seal_scoped, 3}, {subject_hmac, 4}],
        lists:sort(maps:get(eb_crypto_port, Contracts))
    ),
    ?assertEqual([{now, 0}], maps:get(eb_clock_port, Contracts)),
    ?assertEqual([{new_id, 1}], maps:get(eb_id_port, Contracts)),
    ?assertEqual([{append, 2}], maps:get(eb_audit_port, Contracts)),
    %% 原 put/stream/delete 三件保留，追加 R0-4 的 metadata 生命周期
    ?assertEqual(
        [
            {cleanup_asset, 3},
            {confirm_asset, 3},
            {delete_private, 3},
            {fetch_asset, 3},
            {insert_asset, 3},
            {put_private, 3},
            {stream_content, 3}
        ],
        lists:sort(maps:get(eb_asset_port, Contracts))
    ),
    %% EB-03R 新增端口
    ?assertEqual([{load_request_facts, 1}], maps:get(eb_auth_port, Contracts)),
    ?assertEqual(
        [{default_workspace, 2}, {member_status, 2}],
        lists:sort(maps:get(eb_member_fact_port, Contracts))
    ),
    ?assertEqual(
        [{accept_message, 3}, {append_conversation_audit, 3}],
        lists:sort(maps:get(eb_tx_port, Contracts))
    ),
    %% 用例级 purge 端口：**只剩 /4**（T3 窄形状）。过渡信封 /3 已按 EB-06-A18
    %% （E6-D3 / E5-D2）删除 —— 契约面不再可能接受一个 Opts map。
    ?assertEqual(
        [{purge_batch, 4}],
        lists:sort(maps:get(eb_purge_port, Contracts))
    ).

%% EB-03R C6：契约与装配的同步校验——声明了就必须有实现。
ports_assembly_covers_declared_ports_test() ->
    ?assertEqual(ok, eb_ports:assembly_missing(eb_infra_ports:implementations())),
    %% 负例（证明判定非恒真）：把 auth 从装配里去掉 ⇒ 必须点名 unimplemented_port。
    Trimmed = [
        Pair
     || {Port, _Impl} = Pair <- eb_infra_ports:implementations(), Port =/= eb_auth_port
    ],
    ?assertEqual({error, {unimplemented_port, eb_auth_port}}, eb_ports:assembly_missing(Trimmed)),
    %% resolve/1 的同步校验：未装配的已声明端口必须显式失败，不得静默。
    ?assertEqual({ok, eb_pg_store}, eb_infra_ports:resolve(store)),
    ?assertEqual({ok, eb_asset_store}, eb_infra_ports:resolve(asset)),
    ?assertEqual({ok, eb_member_fact_pg}, eb_infra_ports:resolve(member_fact)),
    ?assertEqual({ok, eb_pg_tx}, eb_infra_ports:resolve(tx)),
    ?assertEqual({ok, eb_pg_purge_port}, eb_infra_ports:resolve(purge)),
    ?assertEqual({error, {unknown_port, nope}}, eb_infra_ports:resolve(nope)).

%% EB-03R R0-5：asset 端口不再是 not_implemented_yet。
asset_port_is_assembled_test() ->
    ?assertNotEqual({error, {not_implemented_yet, asset}}, eb_infra_ports:resolve(asset)),
    {ok, Impl} = eb_infra_ports:resolve(asset),
    Exports = [Name || {Name, _A} <- Impl:module_info(exports)],
    lists:foreach(
        fun({Name, _Arity}) -> ?assert(lists:member(Name, Exports)) end,
        maps:get(eb_asset_port, eb_ports:contracts())
    ).

%% ===================================================================
%% 3. port 零实现 / 零 meck / 零 repo 引用
%% ===================================================================

ports_declare_no_implementation_test() ->
    %% 扩展点只声明 -callback：除编译器自动生成的 behaviour_info/1 与
    %% module_info/0,1 外不得有任何导出（即零实现）。
    lists:foreach(
        fun(Mod) ->
            ?assertEqual(
                [{behaviour_info, 1}, {module_info, 0}, {module_info, 1}],
                lists:sort(Mod:module_info(exports))
            )
        end,
        eb_ports:all()
    ).

ports_sources_are_contract_only_test() ->
    lists:foreach(
        fun(Mod) ->
            Rel = port_source_rel(Mod),
            Source = read_source(Rel),
            ?assertNotEqual(undefined, Source),
            %% 零模块引用：契约层不得调用任何别的模块（含 repo/ds/pg）。
            ?assertEqual([], mod_refs(Source)),
            ?assertEqual(false, has_meck(Source)),
            ?assertEqual(false, contains(Source, "elib_pg")),
            ?assertEqual(false, contains(Source, "elib_pg_sql"))
        end,
        eb_ports:all()
    ).

%% ===================================================================
%% 4. 租户作用域与 CAS 形状
%% ===================================================================

store_port_is_tenant_scoped_test() ->
    %% 第一个业务参数必须是 OrgId（可选第二是 WorkspaceId）→ 至少 2 参。
    lists:foreach(
        fun({_Name, Arity}) -> ?assert(Arity >= 2) end,
        eb_store_port:behaviour_info(callbacks)
    ).

store_port_has_cas_shaped_advance_test() ->
    %% 铁律 7 的形状：CAS 推进 = OrgId, WorkspaceId, Id, Expected, Next。
    ?assertEqual(5, callback_arity(eb_store_port, advance_assignment)).

audit_port_is_append_only_test() ->
    %% append-only：只有 append，不得出现 update/delete/rewrite。
    Names = [Name || {Name, _} <- eb_audit_port:behaviour_info(callbacks)],
    ?assertEqual([append], Names),
    lists:foreach(
        fun(Name) ->
            ?assertEqual(
                nomatch,
                re:run(atom_to_list(Name), "update|delete|rewrite|purge", [{capture, none}])
            )
        end,
        Names
    ).

%% ===================================================================
%% 5. crypto port 的 AAD 形状
%% ===================================================================

crypto_port_aad_shape_frozen_test() ->
    %% seal/3 与 open/3：AAD 必须绑定 OrgId/WorkspaceId/ConversationId/MessageId。
    %% EB-03R：追加 seal_scoped/3、subject_hmac/4；seal/3、open/3 逐字未动。
    ?assertEqual(
        [{open, 3}, {seal, 3}, {seal_scoped, 3}, {subject_hmac, 4}],
        lists:sort(eb_crypto_port:behaviour_info(callbacks))
    ),
    Source = read_source(port_source_rel(eb_crypto_port)),
    lists:foreach(
        fun(Field) -> ?assert(contains(Source, Field)) end,
        ["organization_id", "workspace_id", "conversation_id", "message_id"]
    ).

%% ===================================================================
%% 6. asset port 不可能把 object key / presigned URL 交给调用方
%% ===================================================================

asset_port_callback_names_cannot_leak_storage_handle_test() ->
    Names = [Name || {Name, _} <- eb_asset_port:behaviour_info(callbacks)],
    %% 冻结正表：新增任何泄漏型 callback 都会让本断言变红。
    %% EB-03R：追加 metadata 生命周期四件；名称仍不得泄漏存储句柄。
    ?assertEqual(
        [
            cleanup_asset,
            confirm_asset,
            delete_private,
            fetch_asset,
            insert_asset,
            put_private,
            stream_content
        ],
        lists:sort(Names)
    ),
    lists:foreach(
        fun(Name) ->
            ?assertEqual(
                nomatch,
                re:run(
                    atom_to_list(Name),
                    "(presign|url|object_key|objectkey|garage|bucket|endpoint|cname)",
                    [caseless]
                )
            )
        end,
        Names
    ).

asset_port_source_declares_no_url_callback_test() ->
    %% 源码证据：-callback 声明行不得出现 presign/url/object_key。
    Source = read_source(port_source_rel(eb_asset_port)),
    CallbackLines = [L || L <- binary:split(Source, <<"\n">>, [global]), is_callback_line(L)],
    ?assert(length(CallbackLines) > 0),
    lists:foreach(
        fun(Line) ->
            ?assertEqual(
                nomatch,
                re:run(Line, "(presign|_url|url_|object_key)", [caseless])
            )
        end,
        CallbackLines
    ).

%% ===================================================================
%% 7. 铁律 3 契约：facade 只调 application/
%% ===================================================================

facade_source_is_readable_test() ->
    Source = facade_source(),
    ?assertNotEqual(undefined, Source),
    ?assert(byte_size(Source) > 0).

facade_covers_required_plan_actions_test() ->
    %% plan §2.1 由本 facade 承载的动作名必须全部存在。
    Exports = [
        Name
     || {Name, _} <- enterprise_business_facade:module_info(exports), Name =/= module_info
    ],
    Required = [
        create_identity,
        bind_assignment,
        end_assignment,
        open_conversation,
        append_message,
        handover_identity,
        request_presign,
        confirm_asset,
        content_stream
    ],
    lists:foreach(
        fun(Fun) -> ?assert(lists:member(Fun, Exports)) end,
        Required
    ).

facade_exports_are_convergence_delegates_test() ->
    %% 每个对外函数都是「收敛校验 + 委派」两段式：(OrgId, Params) 同形状。
    Exports = [
        {Name, Arity}
     || {Name, Arity} <- enterprise_business_facade:module_info(exports), Name =/= module_info
    ],
    ?assert(length(Exports) >= 20),
    lists:foreach(fun({_Name, Arity}) -> ?assertEqual(2, Arity) end, Exports).

facade_references_only_application_layer_test() ->
    %% **本卡 A04 的核心证据。**
    Refs = facade_refs(),
    ?assert(length(Refs) > 0),
    lists:foreach(
        fun(Ref) ->
            ?assertEqual(
                ok,
                facade_ref_violation(
                    Ref,
                    eb_ports:facade_targets(),
                    feature_module_index(),
                    eb_ports:facade_reference_whitelist()
                )
            )
        end,
        Refs
    ).

facade_ref_violation_detects_injected_violations_test() ->
    %% 负向对照：证明上面的判定不是恒真——同层违规、越层引用、repo/port 直连、
    %% 未登记模块都必须被判红。
    Index = #{
        eb_identity_app => "src/features/enterprise_business/application/eb_identity_app.erl",
        eb_identity => "src/features/enterprise_business/domain/eb_identity.erl",
        eb_identity_http => "src/features/enterprise_business/interfaces/eb_identity_http.erl",
        eb_identity_seq => "src/features/enterprise_business/infrastructure/eb_identity_seq.erl"
    },
    Targets = eb_ports:facade_targets(),
    Whitelist = eb_ports:facade_reference_whitelist(),
    %% application 层 → 放行
    ?assertEqual(ok, facade_ref_violation(eb_identity_app, Targets, Index, Whitelist)),
    %% domain / interfaces / infrastructure → 判红（铁律 3 只进 application）
    ?assertMatch(
        {error, {outside_application_layer, eb_identity, _}},
        facade_ref_violation(eb_identity, Targets, Index, Whitelist)
    ),
    ?assertMatch(
        {error, {outside_application_layer, eb_identity_http, _}},
        facade_ref_violation(eb_identity_http, Targets, Index, Whitelist)
    ),
    ?assertMatch(
        {error, {outside_application_layer, eb_identity_seq, _}},
        facade_ref_violation(eb_identity_seq, Targets, Index, Whitelist)
    ),
    %% repo / ds / port 直连 → 判红
    ?assertEqual(
        {error, {persistence_reference, eb_identity_repo}},
        facade_ref_violation(eb_identity_repo, Targets, Index, Whitelist)
    ),
    ?assertEqual(
        {error, {persistence_reference, eb_identity_ds}},
        facade_ref_violation(eb_identity_ds, Targets, Index, Whitelist)
    ),
    ?assertEqual(
        {error, {port_reference, eb_store_port}},
        facade_ref_violation(eb_store_port, Targets, Index, Whitelist)
    ),
    %% 未登记模块 → 判红；登记后放行（证明判定确实读取白名单）
    ?assertEqual(
        {error, {undeclared_reference, maps}},
        facade_ref_violation(maps, Targets, Index, Whitelist)
    ),
    ?assertEqual(ok, facade_ref_violation(maps, Targets, Index, [maps])).

facade_never_references_repo_ds_or_port_test() ->
    %% 铁律 3：facade 不得直达 repo/ds，也不得越过 application 直调扩展点。
    Source = facade_source(),
    ?assertNotEqual(undefined, Source),
    Refs = mod_refs(Source),
    ?assert(length(Refs) > 0),
    lists:foreach(
        fun(Ref) ->
            Name = atom_to_list(Ref),
            ?assertEqual(false, lists:suffix("_repo", Name)),
            ?assertEqual(false, lists:suffix("_ds", Name)),
            ?assertEqual(false, lists:suffix("_port", Name))
        end,
        Refs
    ).

facade_source_has_no_sql_or_pg_test() ->
    Source = facade_source(),
    ?assertNotEqual(undefined, Source),
    %% 只在代码部分判定：注释里出现 "SELECT" 不构成违规。
    Code = strip_comments(Source),
    ?assertEqual(false, contains(Code, "elib_pg")),
    ?assertEqual(false, contains(Code, "elib_pg_sql")),
    ?assertEqual(false, contains(Code, "SELECT ")),
    ?assertEqual(false, contains(Code, "UPDATE ")),
    ?assertEqual(false, contains(Code, "INSERT ")),
    ?assertEqual(false, contains(Code, "DELETE ")),
    ?assertEqual(false, has_meck(Code)).

facade_reference_whitelist_is_minimal_test() ->
    %% 白名单显式且最小：本卡为空集（facade 只应引用 application 模块）。
    ?assertEqual([], eb_ports:facade_reference_whitelist()).

facade_targets_are_application_use_cases_test() ->
    Targets = eb_ports:facade_targets(),
    ?assert(length(Targets) > 0),
    ?assertEqual(length(Targets), length(lists:usort(Targets))),
    lists:foreach(
        fun(Target) ->
            Name = atom_to_list(Target),
            %% 命名契约：eb_<域>_app，杜绝把 repo/ds/port 误登记为 facade 目标。
            ?assertEqual(true, lists:prefix("eb_", Name)),
            ?assertEqual(true, lists:suffix("_app", Name)),
            ?assertEqual(match, re:run(Name, "^eb_[a-z_]+_app$", [{capture, none}])),
            ?assertEqual(false, lists:suffix("_repo", Name)),
            ?assertEqual(false, lists:suffix("_ds", Name)),
            ?assertEqual(false, lists:suffix("_port", Name))
        end,
        Targets
    ).

%% ===================================================================
%% 辅助：源码读取与引用提取（与 arch-check 同口径）
%% ===================================================================

%% EB-03R：端口分文件可能落在 application/ 的子目录（如 `application/auth/`），
%% 故用 feature 模块索引解析（而不是拼一个固定前缀路径）。
port_source_rel(Mod) ->
    case maps:find(Mod, feature_module_index()) of
        {ok, Rel} -> Rel;
        error -> ?APPLICATION_PREFIX ++ atom_to_list(Mod) ++ ".erl"
    end.

read_source(Rel) ->
    case resolve(Rel) of
        undefined ->
            undefined;
        Path ->
            case file:read_file(Path) of
                {ok, Bin} -> Bin;
                {error, _} -> undefined
            end
    end.

facade_source() -> read_source(?FACADE_REL).

resolve(Rel) ->
    Candidates = [Rel, "../" ++ Rel, "../../" ++ Rel],
    case [P || P <- Candidates, filelib:is_regular(P)] of
        [Path | _] -> Path;
        [] -> undefined
    end.

%% 与 arch-check 的 refs_in/1 同口径：Mod:fun( 与 -behaviour(Mod)。
facade_refs() -> mod_refs(facade_source()).

%% 单条引用是否满足铁律 3 的 facade 约束。判定顺序固定：
%% 持久化名 → 扩展点名 → 已落盘则必须在 application/ → 否则必须已登记。
facade_ref_violation(Ref, Targets, Index, Whitelist) ->
    Name = atom_to_list(Ref),
    case suffix_any(Name, ["_repo", "_ds"]) of
        true ->
            {error, {persistence_reference, Ref}};
        false ->
            case suffix_any(Name, ["_port"]) of
                true ->
                    {error, {port_reference, Ref}};
                false ->
                    case maps:find(Ref, Index) of
                        {ok, Rel} ->
                            case lists:prefix(?APPLICATION_PREFIX, Rel) of
                                true -> ok;
                                false -> {error, {outside_application_layer, Ref, Rel}}
                            end;
                        error ->
                            case lists:member(Ref, Targets) orelse lists:member(Ref, Whitelist) of
                                true -> ok;
                                false -> {error, {undeclared_reference, Ref}}
                            end
                    end
            end
    end.

suffix_any(Name, Suffixes) ->
    lists:any(fun(Suffix) -> lists:suffix(Suffix, Name) end, Suffixes).

refs_in_detects_calls_and_ignores_comments_test() ->
    %% 负向对照：证明引用提取非恒空，且注释不算引用（与 arch-check 的
    %% code_only 同口径）。
    Source = <<
        "-module(sample).\n"
        "%% cowboy_req:method(X) 只是注释\n"
        "-callback pick(term()) -> ok.\n"
        "f() -> eb_identity_app:create_identity(1, #{}).\n"
    >>,
    ?assertEqual([eb_identity_app], mod_refs(Source)),
    CommentOnly = <<"-module(sample).\n%% elib_pg:query(S, A)\nf() -> ok.\n">>,
    ?assertEqual([], mod_refs(CommentOnly)).

mod_refs(undefined) ->
    [];
mod_refs(Source) ->
    Code = strip_comments(Source),
    CallRefs = capture_all(Code, "\\b([a-z][a-z0-9_]*):[a-z_][a-z0-9_]*\\("),
    BehaviourRefs = capture_all(Code, "^-behaviou?r\\(([a-z][a-z0-9_]*)\\)"),
    lists:usort(CallRefs ++ BehaviourRefs).

capture_all(Code, Pattern) ->
    case re:run(Code, Pattern, [global, multiline, {capture, [1], binary}]) of
        {match, Matches} -> lists:usort([binary_to_atom(M, utf8) || [M] <- Matches]);
        nomatch -> []
    end.

%% 只保留代码部分（去掉 Erlang 行注释），与 arch-check 的 code_only 同口径。
strip_comments(Source) ->
    Lines = binary:split(Source, <<"\n">>, [global]),
    iolist_to_binary([[strip_line(Line), <<"\n">>] || Line <- Lines]).

strip_line(Line) ->
    case binary:split(Line, <<"%">>) of
        [Before, _After] -> Before;
        [Only] -> Only
    end.

%% feature 内 -module 索引：模块名 → 相对路径（"." / ".." 段已归一）。
feature_module_index() ->
    maps:from_list([
        {module_name(Path), normalize(Path)}
     || Path <- feature_files()
    ]).

feature_files() ->
    Candidates = [?FEATURE_ROOT, "../" ++ ?FEATURE_ROOT, "../../" ++ ?FEATURE_ROOT],
    case [D || D <- Candidates, filelib:is_dir(D)] of
        [Dir | _] -> filelib:wildcard(Dir ++ "/**/*.erl");
        [] -> []
    end.

module_name(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            Code = strip_comments(Bin),
            case
                re:run(Code, "^-module\\(([a-z][a-z0-9_]*)\\)", [multiline, {capture, [1], binary}])
            of
                {match, [M]} -> binary_to_atom(M, utf8);
                nomatch -> undefined
            end;
        {error, _} ->
            undefined
    end.

normalize(Path) ->
    Parts = [P || P <- filename:split(Path), P =/= ".", P =/= ".."],
    filename:join(Parts).

callback_arity(Mod, Name) ->
    case lists:keyfind(Name, 1, Mod:behaviour_info(callbacks)) of
        {Name, Arity} -> Arity;
        false -> undefined
    end.

is_callback_line(Line) ->
    %% Erlang 形态为 `-callback name(Args) -> Ret.`：名字在 -callback 与左括号之间。
    case re:run(Line, "^\\s*-callback\\s+[a-z]", [{capture, none}]) of
        match -> true;
        nomatch -> false
    end.

has_meck(undefined) ->
    false;
has_meck(Source) ->
    %% 只在代码部分判定（注释里写「零 mock」不构成违规）。
    %% 判定口径比 arch-check 的 domain 测试规则更严格（任何提及都算），
    %% 因此本文件内刻意不出现该规则所用的两个字面量。
    Code = strip_comments(Source),
    contains(Code, "meck") orelse contains(Code, "WITH_MECKS").

contains(undefined, _Needle) ->
    false;
contains(Source, Needle) ->
    binary:match(Source, list_to_binary(Needle)) =/= nomatch.
