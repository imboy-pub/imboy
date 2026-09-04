-module(data_disposition_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 数据处置清单（Implementation Plan Task D-02）：
%%%   * 解析器单元测试（受限 YAML 子集：正常/缺字段/坏 action/未知语法）
%%%   * 真 PostgreSQL 覆盖校验：当前 schema 每张表必须被映射或显式排除
%%% 运行：make eunit-local t=data_disposition_tests

-define(MANIFEST, "docs/compliance/data-disposition.yml").

%% ===================================================================
%% 解析器单元测试
%% ===================================================================

parse_minimal_test() ->
    Yml =
        <<"meta:\n  schema_version: 1\n\ntables:\n  msg_c2c:\n    action: delete\n    reason: 用户消息密文\n    review: self-evident\n  trust_audit:\n    action: retain\n    reason: 安全审计留存\n    review: pending-owner\nexclusions:\n  schema_migrations:\n    reason: 迁移版本登记\n"/utf8>>,
    {ok, Dispositions} = data_disposition:parse(Yml),
    3 = length(Dispositions),
    [Msg] = [D || D <- Dispositions, maps:get(table, D) =:= <<"msg_c2c">>],
    delete = maps:get(action, Msg),
    <<"self-evident">> = maps:get(review, Msg),
    [Trust] = [D || D <- Dispositions, maps:get(table, D) =:= <<"trust_audit">>],
    retain = maps:get(action, Trust),
    <<"pending-owner">> = maps:get(review, Trust),
    [Mig] = [D || D <- Dispositions, maps:get(table, D) =:= <<"schema_migrations">>],
    exclude = maps:get(action, Mig).

parse_missing_required_field_test() ->
    Yml = <<
        "tables:\n"
        "  msg_c2c:\n"
        "    action: delete\n"
    >>,
    {error, {missing_fields, <<"msg_c2c">>, [reason, review]}} =
        data_disposition:parse(Yml).

parse_bad_action_test() ->
    Yml = <<
        "tables:\n"
        "  msg_c2c:\n"
        "    action: nuke\n"
        "    reason: x\n"
        "    review: self-evident\n"
    >>,
    {error, {bad_action, <<"msg_c2c">>, <<"nuke">>}} = data_disposition:parse(Yml).

parse_bad_review_test() ->
    Yml = <<
        "tables:\n"
        "  msg_c2c:\n"
        "    action: delete\n"
        "    reason: x\n"
        "    review: whatever\n"
    >>,
    {error, {bad_review, <<"msg_c2c">>, <<"whatever">>}} = data_disposition:parse(Yml).

parse_unsupported_syntax_test() ->
    %% 列表语法等超集用法必须报错（宁严勿漏）
    Yml = <<
        "tables:\n"
        "  - msg_c2c\n"
    >>,
    {error, {unsupported_line, _, _}} = data_disposition:parse(Yml).

parse_exclusions_unknown_field_test() ->
    Yml = <<
        "exclusions:\n"
        "  schema_migrations:\n"
        "    action: retain\n"
    >>,
    {error, {unsupported_line, _, _}} = data_disposition:parse(Yml).

parse_empty_manifest_test() ->
    {error, empty_manifest} = data_disposition:parse(<<>>).

%% ===================================================================
%% 覆盖校验单元测试
%% ===================================================================

coverage_ok_test() ->
    {ok, Dispositions} = data_disposition:parse(
        <<"tables:\n  msg_c2c:\n    action: delete\n    reason: x\n    review: self-evident\nexclusions:\n  schema_migrations:\n    reason: 迁移\n"/utf8>>
    ),
    {ok, Report} = data_disposition:coverage(
        Dispositions, [], [<<"msg_c2c">>, <<"schema_migrations">>]
    ),
    [] = maps:get(unmapped, Report),
    [] = maps:get(stale, Report).

coverage_unmapped_fails_test() ->
    {ok, Dispositions} = data_disposition:parse(
        <<
            "tables:\n"
            "  msg_c2c:\n"
            "    action: delete\n"
            "    reason: x\n"
            "    review: self-evident\n"
        >>
    ),
    %% 真库多出一张未映射表 → error 且报告列出
    {error, Report} = data_disposition:coverage(
        Dispositions, [], [<<"msg_c2c">>, <<"sneaky_new_table">>]
    ),
    [<<"sneaky_new_table">>] = maps:get(unmapped, Report).

coverage_stale_mapping_fails_test() ->
    {ok, Dispositions} = data_disposition:parse(
        <<
            "tables:\n"
            "  dropped_table:\n"
            "    action: delete\n"
            "    reason: x\n"
            "    review: self-evident\n"
        >>
    ),
    %% 映射指向已不存在的表 → 陈旧条目也要清
    {error, Report} = data_disposition:coverage(
        Dispositions, [], [<<"user">>]
    ),
    [<<"dropped_table">>] = maps:get(stale, Report).

%% ===================================================================
%% 真 PostgreSQL：当前 schema 全量覆盖校验（D-02 验收主断言）
%% ===================================================================

live_schema_full_coverage_test_() ->
    ?TEST_WITH_DB(fun() ->
        {ok, Dispositions} =
            data_disposition:parse_file(?MANIFEST),
        {ok, Rows} =
            elib_pg:query(
                <<
                    "SELECT tablename FROM pg_tables"
                    " WHERE schemaname = 'public'"
                >>,
                []
            ),
        LiveTables = [maps:get(<<"tablename">>, R) || R <- Rows],
        true = length(LiveTables) >= 100,
        case data_disposition:coverage(Dispositions, [], LiveTables) of
            {ok, Report} ->
                %% pending-owner 仅报告不失败：owner 确认后翻转 review 字段
                true = is_list(maps:get(pending_owner, Report));
            {error, Report} ->
                erlang:error({coverage_failed, maps:get(unmapped, Report), maps:get(stale, Report)})
        end
    end).

%% ===================================================================
%% 真 YAML 文件可解析（无 DB 依赖的冒烟）
%% ===================================================================

manifest_file_parses_test() ->
    {ok, Dispositions} = data_disposition:parse_file(?MANIFEST),
    true = length(Dispositions) >= 140,
    Actions = [maps:get(action, D) || D <- Dispositions],
    true = lists:member(delete, Actions),
    true = lists:member(retain, Actions),
    true = lists:member(anonymize, Actions),
    true = lists:member(exclude, Actions).
