-module(elib_tsid_registration_guard_tests).

%%%
% 守护测试：扫描 src/ 全部 elib_tsid:generate(<Name>) 调用点，
% 断言每个命名生成器都在 elib_tsid_inventory 静态实体清单中。
%
% 背景：此 bug 类已两次进入生产（a93aa998 补 ai_agent_role_version /
% agent_payment_compensation；2026-08-22 bot_ds 用未注册的 bot 生成器，
% Bot 注册运行时必崩）。命名空间约定（同表必须同生成器）不在本测试范围。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(SRC_DIR, "src").

missing_global_service_rejects_initialization_test_() ->
    {timeout, 20, fun() ->
        Eval =
            "undefined = whereis(global_name_server), "
            "try elib_tsid:init(#{dc_id => 1, node_id => 1}) of "
            "_ -> halt(2) catch "
            "error:{elib_tsid_init_lock_unavailable, global_name_server} -> "
            "not_initialized = elib_tsid:readiness(), halt(0) end.",
        Port = open_port(
            {spawn_executable, os:find_executable("erl")},
            [
                binary,
                exit_status,
                stderr_to_stdout,
                {args, [
                    "+S",
                    "2:2",
                    "+SDcpu",
                    "1",
                    "+SDio",
                    "1",
                    "-noshell",
                    "-kernel",
                    "start_distribution",
                    "false",
                    "-pa",
                    filename:dirname(code:which(elib_tsid)),
                    "-eval",
                    Eval
                ]}
            ]
        ),
        try
            ?assertEqual({0, <<>>}, collect_lock_probe(Port, <<>>))
        after
            case erlang:port_info(Port) of
                undefined -> ok;
                _ -> erlang:port_close(Port)
            end
        end
    end}.

collect_lock_probe(Port, Output) ->
    receive
        {Port, {data, Data}} -> collect_lock_probe(Port, <<Output/binary, Data/binary>>);
        {Port, {exit_status, Status}} -> {Status, Output}
    after 10000 ->
        error({tsid_lock_probe_timeout, Output})
    end.

all_generate_call_sites_registered_test() ->
    CallSites = scan_generate_calls(),
    ?assert(length(CallSites) > 50, "扫描结果异常：调用点过少，扫描逻辑可能失效"),
    Registered = registered_names(),
    Unregistered =
        lists:uniq([
            Name
         || {_File, _Line, Name} <- CallSites,
            not lists:member(Name, Registered)
        ]),
    ?assertEqual([], Unregistered).

%% V10：无参/default 发号入口已删除；禁止再次引入调用。
%% 无显式 namespace 的发号在多游标语义下无法归属实体，禁止回归。
no_argumentless_generate_in_src_test() ->
    Offenders = scan_argumentless_generate(),
    ?assertEqual([], Offenders).

argumentless_function_reference_test() ->
    ?assertMatch({match, _}, argumentless_match(<<"fun elib_tsid:generate/0">>)),
    ?assertMatch({match, _}, argumentless_match(<<"elib_tsid:generate()">>)),
    ?assertEqual(nomatch, argumentless_match(<<"elib_tsid:generate(user)">>)).

argumentless_match(Bin) ->
    re:run(Bin, <<"elib_tsid:generate\\s*(?:\\(\\s*\\)|/\\s*0\\b)">>).

no_runtime_registration_in_src_test() ->
    Offenders = lists:append([
        begin
            {ok, Bin} = file:read_file(Path),
            [
                {Path, Pattern}
             || Pattern <- [<<"elib_tsid:register(">>, <<"elib_tsid:registered(">>],
                binary:match(Bin, Pattern) =/= nomatch
            ]
        end
     || Path <- find_erl_files(?SRC_DIR)
    ]),
    ?assertEqual([], Offenders).

id_port_callsite_scanner_test() ->
    lists:foreach(
        fun({Module, Function}) ->
            Line = iolist_to_binary([Module, ":", Function, "(missing_namespace)"]),
            ?assertEqual(
                [{"fixture.erl", 1, missing_namespace}],
                parse_line("fixture.erl", 1, Line)
            ),
            Variable = iolist_to_binary([Module, ":", Function, "(Kind)"]),
            ?assertEqual([], parse_line("fixture.erl", 1, Variable))
        end,
        [
            {"agent_grant_pg", "next_id"},
            {"agent_run_pg", "next_id"},
            {"eb_tsid", "new_id"},
            {"cs_tsid", "new_id"},
            {"cs_app_support", "new_id"}
        ]
    ).

%% @private 扫描 src/**/*.erl 中的 elib_tsid:generate() （无参）调用
scan_argumentless_generate() ->
    Files = find_erl_files(?SRC_DIR),
    lists:append([
        begin
            {ok, Bin} = file:read_file(F),
            case argumentless_match(Bin) of
                nomatch -> [];
                {match, [{Pos, _}]} -> [{F, line_of(Bin, Pos)}]
            end
        end
     || F <- Files
    ]).

line_of(Bin, Pos) ->
    Head = binary:part(Bin, 0, Pos),
    1 + length(binary:matches(Head, <<10>>)).

%% @doc 返回静态实体清单（含别名，init 前可直读）
-spec registered_names() -> [atom()].
registered_names() ->
    elib_tsid:namespaces() ++ maps:keys(elib_tsid:aliases()).

%% @doc 扫描 src/**/*.erl 中的 elib_tsid:generate(<atom>) 调用
%% 返回 [{File, Line, GeneratorName}]
-spec scan_generate_calls() -> [{string(), pos_integer(), atom()}].
scan_generate_calls() ->
    Files = find_erl_files(?SRC_DIR),
    lists:append([scan_file(F) || F <- Files]).

find_erl_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            lists:append([
                expand(Dir, E)
             || E <- Entries,
                E =/= ".erlang.mk"
            ]);
        {error, Reason} ->
            error({source_inventory_unreadable, Dir, Reason})
    end.

expand(Dir, Entry) ->
    Path = filename:join(Dir, Entry),
    case filelib:is_dir(Path) of
        true ->
            find_erl_files(Path);
        false ->
            case filename:extension(Path) of
                ".erl" -> [Path];
                _ -> []
            end
    end.

scan_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
            lists:append([
                parse_line(Path, N, L)
             || {N, L} <- lists:zip(lists:seq(1, length(Lines)), Lines)
            ]);
        {error, Reason} ->
            error({source_file_unreadable, Path, Reason})
    end.

%% 匹配 elib_tsid:generate(name) / generate_n(name, N)，以及多行调用的首行
parse_line(Path, N, Line) ->
    Re =
        "(?:elib_tsid:generate(?:_n)?|(?:agent_grant_pg|agent_run_pg):next_id|"
        "(?:eb_tsid|cs_tsid|cs_app_support):new_id)\\s*\\(\\s*(\\w+)",
    case re:run(Line, Re, [{capture, all_but_first, list}]) of
        {match, [Name]} ->
            case is_generator_name(Name) of
                true -> [{Path, N, list_to_atom(Name)}];
                false -> []
            end;
        nomatch ->
            []
    end.

%% 排除变量/表达式参数（只接受全小写字母数字下划线的字面原子）
is_generator_name(Name) ->
    Name =/= "" andalso not lists:member(hd(Name), "_") andalso
        lists:all(
            fun(C) ->
                (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_
            end,
            Name
        ).
