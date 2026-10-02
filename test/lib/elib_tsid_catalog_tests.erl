%%%-------------------------------------------------------------------
%%% @doc TSID catalog 防漂移静态测试（不连数据库、不依赖网络）
%%%
%%% 口径依据 src/lib/elib_tsid_scan.erl 的反向发现（discover_and_compare）：
%%% current_schema() 下「单列 bigint 主键」表未入 catalog 即
%%% {error, {unclassified_primary_keys, L}} 拒启。因此 catalog 必须覆盖
%%% priv/migrations 顺序执行产出的全部单列 bigint 主键表；新增此类迁移
%%% 而不更新 catalog 会让真实 schema 首启 fail-closed——本测试把该契约
%%% 钉在 eunit 层，避免再出现 v1（104 表）式的批量漏登。
%%%
%%% 三组断言：
%%%  1. v2 审计补全的 79 项 + v3 运行时/台账 3 项（含审计点名 5 表与
%%%     全部特例主键列名）在册；
%%%  2. catalog 形态：187 项、无重复、version 4、digest 稳定 32 字节；
%%%  3. 迁移静态解析 ⊆ catalog：宽松解析 priv/migrations/*.up.sql
%%%     （顺序模拟 CREATE TABLE / DROP TABLE / ALTER ADD PRIMARY KEY，
%%%     剥 public. 前缀与引号标识符、去注释与 $$ 块、字符串字面量脱敏），
%%%     断言解析出的全部「存活·单列·bigint 主键」表均已入册且主键列
%%%     一致；4 张 hypertable（msg_c2c/c2g/s2c/store）为复合主键特例，
%%%     单独钉住 (id, created_at) 形态与 id bigint 首列，作为
%%%     elib_tsid_scan:partition_pk_ok 的放行前提。
%%%
%%% 解析抛错的迁移文件必须使测试失败；跳过任何文件都会留下 catalog
%%% 漏检窗口。
%%%-------------------------------------------------------------------
-module(elib_tsid_catalog_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 1. v2 审计补全条目在册（防递减漂移的最小清单）
%% ===================================================================

audit_v2_entries_present_test() ->
    Catalog = elib_tsid_catalog:primary_keys(),
    Missing = [E || E <- audit_v2_added(), not lists:member(E, Catalog)],
    ?assertEqual([], Missing),
    %% 审计点名 5 表逐一在册（列名以 DDL 为准）。
    ?assert(lists:member({project_milestone, id}, Catalog)),
    ?assert(lists:member({organization_department, id}, Catalog)),
    ?assert(lists:member({agent_grant, id}, Catalog)),
    ?assert(lists:member({agent_grant_event, id}, Catalog)),
    ?assert(lists:member({customer_service_seat_console, id}, Catalog)).

%% v3（2026-09-30 生产首启事故）：migrations 之外的三个单列 bigint 主键表。
%% msg_store_staging 为运行时建表（TSID 写入），两个 schema_migrations* 为
%% erlang_migrate 台账表（version 列）——漏登即生产 unclassified_primary_keys
%% 拒启（fail-closed + heart 复活循环）。
v3_runtime_and_ledger_tables_present_test() ->
    Catalog = elib_tsid_catalog:primary_keys(),
    ?assert(lists:member({msg_store_staging, id}, Catalog)),
    ?assert(lists:member({schema_migrations, version}, Catalog)),
    ?assert(lists:member({schema_migrations_history, version}, Catalog)).

%% v2（2026-09-30 审计补全）在 v1 的 104 项之上新增的 79 项。
audit_v2_added() ->
    [
        {adm_auth_epoch, admin_id},
        {agent_effect, id},
        {agent_grant, id},
        {agent_grant_event, id},
        {agent_payment_mandate, id},
        {agent_run, id},
        {agent_run_event, id},
        {ai_agent, user_id},
        {announcement, id},
        {app_version_policy, id},
        {bot, user_id},
        {bot_oauth_grant, id},
        {calligraphy_review_draft, id},
        {channel_category, id},
        {channel_reaction, id},
        {channel_stats_daily, id},
        {class_profile, group_id},
        {conversation, id},
        {customer_service_event, id},
        {customer_service_read_cursor, id},
        {customer_service_seat, business_identity_id},
        {customer_service_seat_console, id},
        {customer_service_seat_limit, organization_id},
        {customer_service_session, id},
        {customer_service_shop_key, id},
        {customer_service_visit_token, id},
        {customer_service_widget_identity_key, id},
        {customer_service_widget_installation, id},
        {customer_service_widget_nonce, id},
        {e2ee_key_shares, id},
        {enterprise_asset, id},
        {enterprise_attachment_retention, attachment_id},
        {enterprise_contact, id},
        {enterprise_contact_assignment, id},
        {enterprise_contact_identity, id},
        {enterprise_conversation, id},
        {enterprise_group_origin, group_id},
        {enterprise_message_delivery, id},
        {enterprise_note, id},
        {enterprise_offboarding_case, id},
        {enterprise_offboarding_item, id},
        {enterprise_retention_hold, id},
        {enterprise_retention_policy, id},
        {fts_channel, channel_id},
        {fts_group, group_id},
        {fts_user, user_id},
        {geo_people_nearby, user_id},
        {group_album_photo_like, id},
        {group_member_generation, id},
        {group_schedule_participant, id},
        {group_vote_record, id},
        {homework_submission, id},
        {learner, id},
        {moment_post_acl, id},
        {moya_subscribe_grant, id},
        {msg_topic, id},
        {organization_business_identity, id},
        {organization_business_identity_assignment, id},
        {organization_default_workspace, organization_id},
        {organization_department, id},
        {organization_invitation, id},
        {project_milestone, id},
        {review_asset, id},
        {review_queue, id},
        {sensitive_word, id},
        {sso_config, id},
        {submission_asset, id},
        {system_datacenter_log, id},
        {system_id_segment, id},
        {system_id_segment_stats, id},
        {teacher_review, id},
        {teaching_admin_audit, id},
        {trust_audit, id},
        {user_auth_epoch, user_id},
        {user_collect, id},
        {user_dnd_rule, id},
        {user_group, id},
        {user_group_category, id},
        {user_setting, user_id}
    ].

%% ===================================================================
%% 2. catalog 形态
%% ===================================================================

catalog_shape_test() ->
    Catalog = elib_tsid_catalog:primary_keys(),
    ?assertEqual(187, length(Catalog)),
    ?assertEqual([], Catalog -- lists:usort(Catalog)),
    ?assertEqual(4, elib_tsid_catalog:version()),
    D = elib_tsid_catalog:digest(),
    ?assertEqual(32, byte_size(D)),
    ?assertEqual(D, elib_tsid_catalog:digest()).

%% ===================================================================
%% 2b. catalog rebind 溯源（known_versions / verified_rebind_transitions）
%% ===================================================================

%% 形态：四个已登记版本、版本号升序、当前版本条目与 digest() 权威源一致、
%% 四个 digest 两两互异且均为 32 字节（防常量誊抄错位）。
known_versions_shape_test() ->
    Known = elib_tsid_catalog:known_versions(),
    ?assertEqual([1, 2, 3, 4], [V || {V, _} <- Known]),
    ?assertEqual(elib_tsid_catalog:digest(), proplists:get_value(4, Known)),
    Digests = [D || {_, D} <- Known],
    ?assertEqual(4, length(lists:usort(Digests))),
    lists:foreach(fun(D) -> ?assertEqual(32, byte_size(D)) end, Digests).

%% v2 常量自证：v3 清单恰为 v2 + 3 项（v3 变更记录），故用当前清单剔除
%% 这 3 项及 v4 新增游标按同一哈希链（sha256(term_to_binary({2, sorted}))）可重构 v2
%% digest——常量誊抄错误在此即刻暴露。
known_versions_v2_digest_selfverifying_test() ->
    V3Only = [
        {msg_store_staging, id},
        {schema_migrations, version},
        {schema_migrations_history, version}
    ],
    V2List =
        elib_tsid_catalog:primary_keys() -- [{workspace_group_read_cursor, generation_id} | V3Only],
    ?assertEqual(183, length(V2List)),
    ExpectV2 = crypto:hash(sha256, term_to_binary({2, lists:sort(V2List)})),
    ?assertEqual(ExpectV2, proplists:get_value(2, elib_tsid_catalog:known_versions())).

known_versions_v3_digest_selfverifying_test() ->
    V3List = elib_tsid_catalog:primary_keys() -- [{workspace_group_read_cursor, generation_id}],
    ?assertEqual(186, length(V3List)),
    ExpectV3 = crypto:hash(sha256, term_to_binary({3, lists:sort(V3List)})),
    ?assertEqual(ExpectV3, proplists:get_value(3, elib_tsid_catalog:known_versions())).

%% v1 常量无法在当前代码内重构自证（v1→v2 为 79 项扩容），钉住与 v2/v3
%% 互异 + 32 字节形状；其 git 溯源取证（352bee53 独立编译重算）登记于
%% 硬化状态文档 §10。
known_versions_v1_anchor_test() ->
    V1 = proplists:get_value(1, elib_tsid_catalog:known_versions()),
    ?assertEqual(32, byte_size(V1)),
    ?assertNotEqual(V1, proplists:get_value(2, elib_tsid_catalog:known_versions())),
    ?assertNotEqual(V1, elib_tsid_catalog:digest()).

%% 已验证迁移邻接：保留历史单步 v1→v2、v2→v3 并追加 v3→v4；全部端点必须在
%% known_versions 中；不含跨步/降级/自环。
verified_rebind_transitions_shape_test() ->
    Trans = elib_tsid_catalog:verified_rebind_transitions(),
    ?assertEqual([{1, 2}, {2, 3}, {3, 4}], Trans),
    Known = [V || {V, _} <- elib_tsid_catalog:known_versions()],
    lists:foreach(
        fun({From, To}) ->
            ?assert(lists:member(From, Known)),
            ?assert(lists:member(To, Known)),
            ?assert(From < To)
        end,
        Trans
    ).

%% ===================================================================
%% 3. 迁移静态解析 ⊆ catalog（反向发现口径的防漂移钉子）
%% ===================================================================

migrations_single_bigint_pk_covered_test() ->
    {Tables, Skipped} = scan_migrations(),
    %% 零跳过合同与解析健全性下限（基线：155 文件 / 226 建表 / 0 跳过；
    %% 未达下限说明 cwd 或仓库结构异常，属环境漂移而非测试误报）。
    ?assertEqual(0, Skipped),
    ?assert(maps:size(Tables) >= 200),
    Catalog = elib_tsid_catalog:primary_keys(),
    Single =
        [
            {T, C}
         || {T, Entry} <- maps:to_list(Tables),
            not maps:get(dropped, Entry, false),
            [C] <- [maps:get(pk, Entry, undefined)],
            is_bigint(maps:get(C, maps:get(cols, Entry, #{}), undefined))
        ],
    Missing = [{T, C} || {T, C} <- Single, not lists:member({bta(T), bta(C)}, Catalog)],
    %% 非空转下限（当前基线 179，仅 migrations 解析口径）：解析退化（如文本倒序）会让 Single
    %% 静默变空、上方断言空转通过——此下限保证解析必须真正咬到 DDL。
    ?assert(length(Single) >= 170),
    ?assertEqual([], Missing).

hypertable_partition_pk_shape_test() ->
    {Tables, _Skipped} = scan_migrations(),
    Catalog = elib_tsid_catalog:primary_keys(),
    [
        begin
            Entry = maps:get(atom_to_binary(T, utf8), Tables),
            ?assertMatch([<<"id">>, <<"created_at">>], maps:get(pk, Entry)),
            ?assert(is_bigint(maps:get(<<"id">>, maps:get(cols, Entry)))),
            ?assert(lists:member({T, id}, Catalog))
        end
     || T <- [msg_c2c, msg_c2g, msg_s2c, msg_store]
    ].

%%%===================================================================
%%% 迁移顺序模拟（宽松解析；与第三方审计解析器同口径）
%%%===================================================================

scan_migrations() ->
    Files = lists:sort(filelib:wildcard("priv/migrations/*.up.sql")),
    lists:foldl(
        fun(File, {Tables, Skipped}) ->
            try
                {apply_file(File, Tables), Skipped}
            catch
                _:_ -> {Tables, Skipped + 1}
            end
        end,
        {#{}, 0},
        Files
    ).

%% 单文件三段式（与审计解析器同序）：CREATE → DROP → ALTER ADD PK。
apply_file(File, Tables0) ->
    {ok, Bin} = file:read_file(File),
    Sql = mask_dollar_blocks(mask_strings(strip_line_comments(Bin))),
    Tables1 = apply_creates(Sql, Tables0),
    Tables2 = apply_drops(Sql, Tables1),
    apply_alter_pks(Sql, Tables2).

apply_creates(Sql, Tables) ->
    Fold =
        fun([{S, L}, {NS, NL}], Acc) ->
            Name = norm_name(binary:part(Sql, NS, NL)),
            %% 整个匹配的最后一个字符是 '('：括号平衡扫描取建表体。
            case body_between(Sql, S + L - 1) of
                {ok, Body} ->
                    {Pk, Cols} = parse_create_body(Body),
                    Acc#{Name => #{pk => Pk, cols => Cols, dropped => false}};
                error ->
                    Acc
            end
        end,
    lists:foldl(Fold, Tables, all_matches(Sql, create_re())).

apply_drops(Sql, Tables) ->
    Fold =
        fun([{_S, _L}, {NS, NL}], Acc) ->
            Names =
                [
                    norm_name(P)
                 || P0 <- re:split(binary:part(Sql, NS, NL), "\\s*,\\s*", [{return, binary}]),
                    P <- [strip_space(P0)],
                    P =/= <<>>
                ],
            lists:foldl(
                fun(Name, A) ->
                    case A of
                        #{Name := Entry} -> A#{Name := Entry#{dropped => true}};
                        _ -> A
                    end
                end,
                Acc,
                Names
            )
        end,
    lists:foldl(Fold, Tables, all_matches(Sql, drop_re())).

apply_alter_pks(Sql, Tables) ->
    Fold =
        fun([{_S0, _L0}, {NS, NL}, {CS, CL}], Acc) ->
            Name = norm_name(binary:part(Sql, NS, NL)),
            case Acc of
                #{Name := Entry} ->
                    Pk = split_pk_cols(binary:part(Sql, CS, CL)),
                    Acc#{Name := Entry#{pk => Pk}};
                _ ->
                    Acc
            end
        end,
    lists:foldl(Fold, Tables, all_matches(Sql, alter_pk_re())).

all_matches(Sql, Re) ->
    case re:run(Sql, Re, [global, caseless]) of
        {match, Matches} ->
            Matches;
        nomatch ->
            []
    end.

%%===================================================================
%% 建表体解析：{Pk :: [Col] | undefined, Cols :: #{Col => Type}}
%%===================================================================

parse_create_body(Body) ->
    Items = [I || I0 <- split_top(Body), I <- [strip_space(I0)], I =/= <<>>],
    lists:foldl(fun parse_item/2, {undefined, #{}}, Items).

parse_item(Item, {Pk, Cols}) ->
    case is_table_constraint(Item) of
        true ->
            case re:run(Item, pk_cols_re(), [caseless, {capture, [1], binary}]) of
                {match, [PkCols]} -> {split_pk_cols(PkCols), Cols};
                nomatch -> {Pk, Cols}
            end;
        false ->
            case column_def(Item) of
                {Name, Rest} ->
                    Pk2 =
                        case re:run(Rest, "\\bPRIMARY\\s+KEY\\b", [caseless]) of
                            {match, _} -> [Name];
                            nomatch -> Pk
                        end,
                    {Pk2, Cols#{Name => first_type_word(Rest)}};
                nomatch ->
                    {Pk, Cols}
            end
    end.

is_table_constraint(Item) ->
    re:run(
        Item,
        "^(?:CONSTRAINT\\s+\\S+\\s+)?(?:PRIMARY\\s+KEY|UNIQUE|CHECK|FOREIGN\\s+KEY|EXCLUDE)\\b",
        [caseless]
    ) =/= nomatch.

%% 列定义：首个 token 为列名（支持引号标识符），其余为类型与约束。
column_def(Item) ->
    case re:run(Item, "^\"([^\"]+)\"\\s+([\\s\\S]*)$", [{capture, [1, 2], binary}]) of
        {match, [Name, Rest]} ->
            {string:lowercase(Name), Rest};
        nomatch ->
            case
                re:run(Item, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s+([\\s\\S]*)$", [
                    {capture, [1, 2], binary}
                ])
            of
                {match, [Name, Rest]} -> {string:lowercase(Name), Rest};
                nomatch -> nomatch
            end
    end.

split_pk_cols(PkCols) ->
    [
        norm_name(P)
     || P0 <- re:split(PkCols, ",", [{return, binary}]),
        P <- [norm_name(P0)],
        P =/= <<>>
    ].

%%%===================================================================
%%% SQL 预处理：行注释剥离、字符串脱敏、$$ 块遮蔽
%%%===================================================================

strip_line_comments(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global, trim]),
    iolist_to_binary(lists:join(<<"\n">>, [strip_one_comment(L) || L <- Lines])).

strip_one_comment(Line) ->
    iolist_to_binary(strip_comment_c(binary_to_list(Line), [], false)).

strip_comment_c([], Acc, _InStr) ->
    lists:reverse(Acc);
strip_comment_c([$-, $- | _], Acc, false) ->
    lists:reverse(Acc);
strip_comment_c([$', $' | T], Acc, InStr) ->
    strip_comment_c(T, [$', $' | Acc], InStr);
strip_comment_c([$' | T], Acc, InStr) ->
    strip_comment_c(T, [$' | Acc], not InStr);
strip_comment_c([C | T], Acc, InStr) ->
    strip_comment_c(T, [C | Acc], InStr).

%% 字符串字面量内容替换为 '.'（保留引号骨架，括号/逗号解析安全）。
mask_strings(Bin) ->
    iolist_to_binary(mask_str_c(binary_to_list(Bin), [])).

mask_str_c([], Acc) ->
    lists:reverse(Acc);
mask_str_c([$' | T], Acc) ->
    mask_str_body_c(T, [$' | Acc]);
mask_str_c([C | T], Acc) ->
    mask_str_c(T, [C | Acc]).

mask_str_body_c([$', $' | T], Acc) ->
    mask_str_body_c(T, [$', $' | Acc]);
mask_str_body_c([$' | T], Acc) ->
    mask_str_c(T, [$' | Acc]);
mask_str_body_c([_ | T], Acc) ->
    mask_str_body_c(T, [$. | Acc]).

%% $$ ... $$ / $tag$ ... $tag$（DO 块等动态 SQL）整块遮蔽为换行。
mask_dollar_blocks(Sql) ->
    re:replace(Sql, "\\$([^$\\n]*)\\$.*?\\$\\1\\$", <<"\n">>, [
        global, {return, binary}, dotall
    ]).

%%%===================================================================
%%% 小工具
%%%===================================================================

%% 建表体（顶层逗号切分，忽略嵌套括号内的逗号）。
%% 累积器 Cur 按「字符前插」收集 → 每个完成项须 lists:reverse/1 恢复
%% 正序；项列表整体再 reverse/1 恢复出现顺序。此处一旦弄错方向，列名
%% 会整体倒序、解析静默退化 —— 见下方非空转下限断言的防线。
split_top(Bin) ->
    [iolist_to_binary(I) || I <- lists:reverse(split_top_c(binary_to_list(Bin), 0, [], []))].

split_top_c([], _D, Cur, Acc) ->
    [lists:reverse(Cur) | Acc];
split_top_c([$, | T], 0, Cur, Acc) ->
    split_top_c(T, 0, [], [lists:reverse(Cur) | Acc]);
split_top_c([$( | T], D, Cur, Acc) ->
    split_top_c(T, D + 1, [$( | Cur], Acc);
split_top_c([$) | T], D, Cur, Acc) ->
    split_top_c(T, D - 1, [$) | Cur], Acc);
split_top_c([C | T], D, Cur, Acc) ->
    split_top_c(T, D, [C | Cur], Acc).

%% OpenIdx 指向 '('：返回与之配对的括号间内容；失衡返回 error。
body_between(Sql, Open) ->
    body_between(Sql, Open + 1, 1, Open + 1).

body_between(Sql, I, 0, Start) when I > Start ->
    {ok, binary:part(Sql, Start, I - 1 - Start)};
body_between(Sql, I, D, _Start) when I >= byte_size(Sql), D > 0 ->
    _ = Sql,
    error;
body_between(Sql, I, D, Start) ->
    case binary:at(Sql, I) of
        $( -> body_between(Sql, I + 1, D + 1, Start);
        $) -> body_between(Sql, I + 1, D - 1, Start);
        _ -> body_between(Sql, I + 1, D, Start)
    end.

qname() ->
    "(?:public\\.)?(?:\"[^\"]+\"|[a-zA-Z_][a-zA-Z0-9_]*)".

create_re() ->
    "CREATE\\s+(?:UNLOGGED\\s+|TEMP(?:ORARY)?\\s+|GLOBAL\\s+|LOCAL\\s+)*TABLE\\s+"
    "(?:IF\\s+NOT\\s+EXISTS\\s+)?(" ++ qname() ++ ")\\s*\\(".

drop_re() ->
    "DROP\\s+TABLE\\s+(?:IF\\s+EXISTS\\s+)?(" ++ qname() ++ "(?:\\s*,\\s*" ++ qname() ++ ")*)".

alter_pk_re() ->
    "ALTER\\s+TABLE\\s+(?:ONLY\\s+)?(?:IF\\s+EXISTS\\s+)?(" ++ qname() ++
        ")\\s+ADD\\s+(?:CONSTRAINT\\s+\\S+\\s+)?PRIMARY\\s+KEY\\s*"
        "(?:USING\\s+INDEX\\s+TABLESPACE\\s+\\S+\\s*)?\\(([^)]*)\\)".

pk_cols_re() ->
    "PRIMARY\\s+KEY\\s*\\(([^)]*)\\)".

first_type_word(Rest) ->
    case re:run(Rest, "^([a-zA-Z_][a-zA-Z0-9_]*)", [caseless, {capture, [1], binary}]) of
        {match, [W]} -> string:lowercase(W);
        nomatch -> <<>>
    end.

%% 剥 public. 前缀与引号，小写化。
norm_name(B0) ->
    B1 = re:replace(B0, "^public\\.", "", [caseless, {return, binary}]),
    B2 = re:replace(B1, "^\"|\"$", "", [global, {return, binary}]),
    string:lowercase(strip_space(B2)).

strip_space(B) ->
    re:replace(B, "^\\s+|\\s+$", "", [global, {return, binary}]).

bta(B) ->
    binary_to_atom(B, utf8).

is_bigint(T) ->
    lists:member(T, [<<"bigint">>, <<"bigserial">>, <<"int8">>]).
