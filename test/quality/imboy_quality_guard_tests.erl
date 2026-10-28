%% IMBOY_ERLANG_QUALITY P1-11 正式 guard 回归（全部通过；进入 U）。
%% 在受控合成 Git 仓库中验证 erlang-ci 规则的 Erlang 移植：
%% GUARD02-GUARD08（上游三类抑制、.eqwalizer 文件规则、基础设施失败
%% 分离、NUL/长行/特殊路径、正则 error/nomatch）。上游 Shell 仅作审计
%% 文本，从不执行（GUARD01 由 provenance 校验承担）。
-module(imboy_quality_guard_tests).
-include_lib("eunit/include/eunit.hrl").

guard_test_() ->
    {timeout, 120, fun guard_all/0}.

guard_all() ->
    Dir = tmp("guard"),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Repo = filename:join(Dir, "repo"),
    ok = file:make_dir(Repo),
    Viol = filename:join(Dir, "viol"),
    try
        git(Repo, ["init", "--initial-branch=synthetic"]),
        %% GUARD02：普通新增（干净）通过
        ok = file:write_file(filename:join(Repo, "clean.erl"), <<"-module(clean).\n">>),
        Base = commit(Repo, "base"),
        {ok, []} = imboy_quality_guard:check(Repo, auto, Base),
        %% GUARD02：空差异（同对象）有依据地通过
        {ok, []} = imboy_quality_guard:check(Repo, Base, Base),
        %% GUARD05：不可达对象 → 基础设施失败，不转空 diff
        ?assertMatch(
            {error, {infrastructure, _, _}},
            imboy_quality_guard:check(
                Repo, "missing-object-0000000000000000000000000000000000000000", Base
            )
        ),
        %% GUARD03+GUARD06：三类上游抑制（含长行 20 万字符不截断）
        ok = filelib:ensure_dir(filename:join(Viol, "x")),
        lists:foreach(
            fun({I, Content}) ->
                R2 = filename:join(Viol, "v" ++ integer_to_list(I)),
                ok = file:make_dir(R2),
                git(R2, ["init", "--initial-branch=synthetic"]),
                ok = file:write_file(filename:join(R2, "base.txt"), <<"base\n">>),
                B2 = commit(R2, "base"),
                ok = file:write_file(filename:join(R2, "bad.erl"), Content),
                H2 = commit(R2, "bad"),
                ?assertMatch(
                    {error, {violations, [_ | _]}},
                    imboy_quality_guard:check(R2, B2, H2)
                )
            end,
            lists:zip(
                lists:seq(1, 5),
                %% 违规样本同样拼接构造：语料字面量不得命中规则自身（自引用）
                [
                    <<"% eqwalizer", ":", "fixme\n">>,
                    <<"-dialyzer", "(nowarn_function).\n">>,
                    <<"-dialyzer", "({nowarn_function, f/0}).\n">>,
                    <<"-dialyzer", "\n(\n{ nowarn_function, f/0}).\n">>,
                    <<(binary:copy(<<"x">>, 200000))/binary, "eqwalizer", ":", "fixme">>
                ]
            )
        ),
        %% GUARD06：含换行特殊路径
        Odd = filename:join(Repo, "odd\npath.erl"),
        ok = file:write_file(Odd, <<"-module(odd).\n% eqwalizer", ":fixme\n">>),
        OddC = commit(Repo, "suppression"),
        ?assertMatch(
            {error, {violations, [_ | _]}},
            imboy_quality_guard:check(Repo, Base, OddC)
        ),
        %% GUARD04：新增 .eqwalizer 违规；删除不误判；重命名回 .eqwalizer 违规
        ok = file:write_file(filename:join(Repo, ".eqwalizer"), <<>>),
        Ignored = commit(Repo, "ignore file"),
        ?assertMatch(
            {error, {violations, [_ | _]}},
            imboy_quality_guard:check(Repo, OddC, Ignored)
        ),
        ok = file:delete(filename:join(Repo, ".eqwalizer")),
        Removed = commit(Repo, "remove ignore"),
        {ok, []} = imboy_quality_guard:check(Repo, Ignored, Removed),
        ok = file:write_file(filename:join(Repo, "renamed"), <<>>),
        git(Repo, ["add", "renamed"]),
        git(Repo, ["commit", "-q", "-m", "add placeholder"]),
        git(Repo, ["mv", "renamed", ".eqwalizer"]),
        Renamed = commit(Repo, "rename to ignore"),
        ?assertMatch(
            {error, {violations, [_ | _]}},
            imboy_quality_guard:check(Repo, Removed, Renamed)
        ),
        %% GUARD02：删除旧抑制行不违规
        ok = file:write_file(Odd, <<"-module(odd).\n">>),
        Cleaned = commit(Repo, "cleaned"),
        {ok, []} = imboy_quality_guard:check(Repo, Renamed, Cleaned),
        %% GUARD07：binary 源码（含 NUL）→ 明确基础设施失败
        ok = file:write_file(filename:join(Repo, "nul.erl"), <<"-module(nul).", 0>>),
        Nul = commit(Repo, "binary source"),
        ?assertMatch(
            {error, {infrastructure, _, _}},
            imboy_quality_guard:check(Repo, Cleaned, Nul)
        ),
        %% GUARD09：root commit（无父）auto 基线语义可用（ok 或结构化失败均可，不崩）
        GuardRoot = imboy_quality_guard:check(Repo, auto, Base),
        ?assert(
            element(1, GuardRoot) =:= ok orelse
                element(1, GuardRoot) =:= error
        )
    after
        %% 只清理本测试创建的合成目录
        [
            begin
                file:del_dir_r(D)
            end
         || D <- [Dir], filelib:is_dir(D)
        ]
    end,
    ok.

%% GUARD03/GUARD08 单元级：三条规则同时命中
scan_added_test() ->
    [] = imboy_quality_guard:scan_added(<<"ok.erl">>, <<"ok() -> ok.">>),
    [_, _, _] = imboy_quality_guard:scan_added(
        <<"b.erl">>,
        <<"% eqwalizer", ":fixme\n", "-dialyzer", "(nowarn_x).\n", "-dialyzer", "({nowarn_y}).\n">>
    ).

git(Repo, Args) ->
    {0, _} = imboy_quality_guard:git(Repo, Args),
    ok.

commit(Repo, Msg) ->
    git(Repo, ["add", "--all"]),
    git(Repo, [
        "-c",
        "user.name=leeyi",
        "-c",
        "user.email=leeyisoft@qq.com",
        "-c",
        "commit.gpgsign=false",
        "commit",
        "-q",
        "-m",
        Msg
    ]),
    {0, Out} = imboy_quality_guard:git(Repo, ["rev-parse", "HEAD"]),
    string:trim(binary_to_list(Out)).

tmp(Name) ->
    filename:join(
        "/private/tmp",
        "imboy-quality-" ++ Name ++ "-" ++ os:getpid()
    ).
