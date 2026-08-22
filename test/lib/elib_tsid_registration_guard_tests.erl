-module(elib_tsid_registration_guard_tests).

%%%
% 守护测试：扫描 src/ 全部 elib_tsid:generate(<Name>) 调用点，
% 断言每个命名生成器都已注册进 imboy_app:tsid_generator_names()。
%
% 背景：此 bug 类已两次进入生产（a93aa998 补 ai_agent_role_version /
% agent_payment_compensation；2026-08-22 bot_ds 用未注册的 bot 生成器，
% Bot 注册运行时必崩）。命名空间约定（同表必须同生成器）不在本测试范围。
%%%

-include_lib("eunit/include/eunit.hrl").

-define(SRC_DIR, "src").

all_generate_call_sites_registered_test() ->
    CallSites = scan_generate_calls(),
    ?assert(length(CallSites) > 50, "扫描结果异常：调用点过少，扫描逻辑可能失效"),
    Registered = registered_names(),
    Unregistered =
        lists:uniq([
            Name
         || {_File, _Line, Name} <- CallSites,
            Name =/= default,
            not lists:member(Name, Registered)
        ]),
    ?assertEqual([], Unregistered).

%% @doc 返回注册清单（含 default；init 前也可直接读函数字面量）
-spec registered_names() -> [atom()].
registered_names() ->
    [default | imboy_app:tsid_generator_names()].

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
        {error, _} ->
            []
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
        {error, _} ->
            []
    end.

%% 匹配 elib_tsid:generate(name) / generate_n(name, N)，以及多行调用的首行
parse_line(Path, N, Line) ->
    Re = "elib_tsid:generate(_n)?\\((\\w+)",
    case re:run(Line, Re, [{capture, all_but_first, list}]) of
        {match, [_Opt, Name]} ->
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
