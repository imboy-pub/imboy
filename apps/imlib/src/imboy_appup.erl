-module(imboy_appup).

-export([run/2, first_release/1]).

-define(SHELL(Cmd), os:cmd(Cmd)).

%% 入口：升级模式
run(OldTag, NewTag) when is_list(OldTag), is_list(NewTag) ->
    try
        AppsNew = apps_under_tag(NewTag),
        {ChangedByModules, PerAppUpDown} = per_app_changes(OldTag, NewTag, AppsNew),
        ChangedByVsn = apps_changed_vsn(OldTag, NewTag, AppsNew),
        ChangedByDeps = deps_changes(OldTag, NewTag),
        ChangedApps = ordsets:to_list(
            ordsets:union(
              ordsets:from_list(ChangedByModules),
              ordsets:union(
                ordsets:from_list(ChangedByVsn),
                ordsets:from_list(ChangedByDeps)
              )
            )
        ),
        %% 写入每个子应用 .appup
        ok = write_per_app_appups(OldTag, NewTag, PerAppUpDown),
        %% 顶层
        ok = write_top_appup(OldTag, NewTag, ChangedApps),
        ok
    catch
        C:R:ST ->
            {error, {C, R, ST}}
    end.

%% 入口：首次发布
first_release(NewTag) when is_list(NewTag) ->
    try
        AppsNew = apps_under_tag(NewTag),
        %% 子应用写空指令
        lists:foreach(fun(App) ->
            File = filename:join(["apps", App, "src", App ++ ".appup"]),
            ok = ensure_dir(File),
            NewTerm = {NewTag, [], []},
            CurTerms = consult_terms(File),
            OutTerms = merge_terms(NewTerm, CurTerms, []),
            ok = write_terms(File, OutTerms)
        end, AppsNew),
        %% 顶层写空指令
        ok = ensure_dir("src/imboy.appup"),
        ok = write_terms("src/imboy.appup", [{NewTag, [], []}]),
        ok
    catch
        C:R:ST ->
            {error, {C, R, ST}}
    end.

%% ============== 内部实现 ==============

%% 列出 NEW_TAG 下的 apps/* 名称（以 NEW_TAG 为主进行输出写入）
apps_under_tag(Tag) ->
    %% 通过列举 tag 下 apps 子目录名推导 app 列表，并确认存在 src/*.erl
    Cmd = io_lib:format("git ls-tree -r --name-only ~s -- apps | awk -F/ 'NF>=2 {print $2}' | sort -u", [shq(Tag)]),
    Candidates = [L || L <- split_lines(?SHELL(lists:flatten(Cmd))), L =/= ""],
    [A || A <- Candidates, has_src_erl(Tag, A)].

has_src_erl(Tag, App) ->
    P = io_lib:format("apps/~s/src", [App]),
    Cmd = io_lib:format("git ls-tree -r --name-only ~s -- ~s 2>/dev/null | grep '\\.erl$' | wc -l", [shq(Tag), shq(lists:flatten(P))]),
    NStr = string:trim(lists:flatten(?SHELL(lists:flatten(Cmd)))),
    case string:to_integer(NStr) of
        {Int, _} when Int > 0 -> true;
        _ -> false
    end.

%% 计算每个 app 的变更（add/delete/load），同时返回最终要写入的指令
per_app_changes(OldTag, NewTag, AppsNew) ->
    lists:foldl(
      fun(App, {AccChangedApps, AccPerApp}) ->
          OldMods = modules_of(App, OldTag),
          NewMods = modules_of(App, NewTag),
          {Added, Deleted, Common} = diff_sets(OldMods, NewMods),
          Modified = [M || M <- Common, module_changed(App, M, OldTag, NewTag)],
          HasChanges = (Added =/= []) orelse (Deleted =/= []) orelse (Modified =/= []),
          Up = up_instrs(Added, Deleted, Modified),
          Down = down_instrs(Added, Deleted, Modified),
          AccChangedApps2 = case HasChanges of true -> [App | AccChangedApps]; false -> AccChangedApps end,
          AccPerApp2 = [{App, Up, Down} | AccPerApp],
          {AccChangedApps2, AccPerApp2}
      end, {[], []}, AppsNew).

modules_of(App, Tag) ->
    Path = io_lib:format("apps/~s/src", [App]),
    Cmd = io_lib:format("git ls-tree -r --name-only ~s -- ~s 2>/dev/null | grep '\\.erl$' | xargs -I {} basename {} .erl | sort -u", [shq(Tag), shq(lists:flatten(Path))]),
    [L || L <- split_lines(?SHELL(lists:flatten(Cmd))), L =/= ""].

module_changed(App, Mod, OldTag, NewTag) ->
    P = io_lib:format("apps/~s/src/~s.erl", [App, Mod]),
    Old = git_show(OldTag, lists:flatten(P)),
    New = git_show(NewTag, lists:flatten(P)),
    Old =/= "" andalso New =/= "" andalso Old =/= New.

up_instrs(Added, Deleted, Modified) ->
    A1 = [{add_module, list_to_atom(M)} || M <- Added],
    A2 = [{delete_module, list_to_atom(M)} || M <- Deleted],
    A3 = [{load_module, list_to_atom(M)} || M <- Modified],
    A1 ++ A2 ++ A3.

down_instrs(Added, Deleted, Modified) ->
    D1 = [{delete_module, list_to_atom(M)} || M <- Added],
    D2 = [{add_module, list_to_atom(M)} || M <- Deleted],
    D3 = [{load_module, list_to_atom(M)} || M <- Modified],
    D1 ++ D2 ++ D3.

write_per_app_appups(OldTag, NewTag, PerAppUpDown) ->
    lists:foreach(fun({App, Up, Down}) ->
        File = filename:join(["apps", App, "src", App ++ ".appup"]),
        ok = ensure_dir(File),
        NewTerm = {NewTag, [{OldTag, Up}], [{OldTag, Down}]},
        CurTerms = consult_terms(File),
        OldTagTerms = consult_terms_from_git(OldTag, filename:join(["apps", App, "src", App ++ ".appup"])),
        OutTerms = merge_terms(NewTerm, CurTerms, OldTagTerms),
        ok = write_terms(File, OutTerms)
    end, PerAppUpDown),
    ok.

%% 顶层：changed apps + deps + vsn，且 imboy_sup 变化注入 supervisor update
write_top_appup(OldTag, NewTag, ChangedApps0) ->
    %% imboy_sup 变化检测
    OldSup = git_show(OldTag, "src/imboy_sup.erl"),
    NewSup = git_show(NewTag, "src/imboy_sup.erl"),
    SupChanged = (OldSup =/= "") andalso (NewSup =/= "") andalso (OldSup =/= NewSup),

    %% 指令
    UpBase = case SupChanged of true -> [{update, imboy_sup, supervisor}]; false -> [] end,
    DownBase = UpBase,
    ChangedApps = ordsets:to_list(ordsets:from_list(ChangedApps0)),
    Up = UpBase ++ [{restart_application, list_to_atom(A)} || A <- ChangedApps],
    Down = DownBase ++ [{restart_application, list_to_atom(A)} || A <- ChangedApps],

    %% 组装 term 并与历史合并（当前文件 + OLD_TAG 文件），同版本覆盖，新版置顶
    File = "src/imboy.appup",
    ok = ensure_dir(File),
    NewTerm = {NewTag, [{OldTag, Up}], [{OldTag, Down}]},
    CurTerms = consult_terms(File),
    OldTagTerms = consult_terms_from_git(OldTag, "src/imboy.appup"),
    OutTerms = merge_terms(NewTerm, CurTerms, OldTagTerms),
    ok = write_terms(File, OutTerms),
    ok.

%% vsn 变化集合（app 名称列表）
apps_changed_vsn(OldTag, NewTag, AppsNew) ->
    [App || App <- AppsNew, vsn_changed(OldTag, NewTag, App)].

vsn_changed(OldTag, NewTag, App) ->
    Old = app_vsn_of(OldTag, App),
    New = app_vsn_of(NewTag, App),
    Old =/= "" andalso New =/= "" andalso Old =/= New.

app_vsn_of(Tag, App) ->
    Path = io_lib:format("apps/~s/ebin/~s.app", [App, App]),
    Cont = git_show(Tag, lists:flatten(Path)),
    case re:run(Cont, "\\{vsn,\\s*\"([^\"]+)\"\\}", [unicode, {capture, [1], list}]) of
        {match, [V]} -> V;
        _ -> ""
    end.

%% 依赖变化：Makefile DEPS 成员变化 + include/deps.mk 的 dep_* 规格变化
deps_changes(OldTag, NewTag) ->
    MakeCur = git_show(NewTag, "Makefile"),
    MakeOld = git_show(OldTag, "Makefile"),
    DepsCur = parse_makefile_deps(MakeCur),
    DepsOld = parse_makefile_deps(MakeOld),
    DepsUnion = ordsets:to_list(ordsets:union(ordsets:from_list(DepsCur), ordsets:from_list(DepsOld))),

    SpecsCur = parse_deps_specs(git_show(NewTag, "include/deps.mk")),
    SpecsOld = parse_deps_specs(git_show(OldTag, "include/deps.mk")),

    [Dep || Dep <- DepsUnion,
        begin
            InCur = lists:member(Dep, DepsCur),
            InOld = lists:member(Dep, DepsOld),
            case InCur =/= InOld of
                true -> true;
                false ->
                    CurSpec = proplists:get_value(Dep, SpecsCur, ""),
                    OldSpec = proplists:get_value(Dep, SpecsOld, ""),
                    CurSpec =/= OldSpec
            end
        end
    ].

parse_makefile_deps(Content) ->
    %% 收集所有 DEPS 行右侧 token
    %% 支持 DEPS:=, DEPS+= 等
    {ok, RE} = re:compile("^[[:space:]]*DEPS[[:space:]]*(\\+=|:=)[[:space:]]*(.*)$", [multiline, unicode]),
    case re:run(Content, RE, [{capture, all, list}, global]) of
        {match, Ms} ->
            Tokens = lists:flatmap(
              fun([_, _, RHS]) ->
                  [T || T <- string:tokens(RHS, " \t\r\n"), T =/= ""]
              end, Ms),
            lists:usort(Tokens);
        nomatch -> []
    end.

parse_deps_specs(Content) ->
    %% dep_name[:]?= spec
    {ok, RE} = re:compile("^[[:space:]]*dep_([A-Za-z0-9_]+)[[:space:]]*[:]?=[[:space:]]*(.*)$", [multiline, unicode]),
    case re:run(Content, RE, [{capture, all_but_first, list}, global]) of
        {match, Ms} ->
            lists:usort([{Name, string:trim(Spec)} || [Name, Spec] <- Ms]);
        nomatch -> []
    end.

%% ============== 通用辅助 ==============

git_show(Tag, Path) ->
    Cmd = io_lib:format("git show ~s:~s 2>/dev/null", [shq(Tag), shq(Path)]),
    lists:flatten(?SHELL(lists:flatten(Cmd))).

diff_sets(A, B) ->
    SA = ordsets:from_list(A),
    SB = ordsets:from_list(B),
    Added = ordsets:to_list(ordsets:subtract(SB, SA)),
    Deleted = ordsets:to_list(ordsets:subtract(SA, SB)),
    Common = ordsets:to_list(ordsets:intersection(SA, SB)),
    {Added, Deleted, Common}.

split_lines(BinOrList) ->
    Str = case BinOrList of
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        L when is_list(L) -> L
    end,
    Lines0 = string:tokens(Str, "\n"),
    [string:trim(L) || L <- Lines0].

ensure_dir(Path) ->
    filelib:ensure_dir(Path).

consult_terms(File) ->
    case file:consult(File) of
        {ok, Terms} -> Terms;
        _ -> []
    end.

consult_terms_from_git(Tag, Path) ->
    Tmp = filename:join(["/tmp", "imboy_appup_old_" ++ integer_to_list(erlang:phash2({Tag, Path})) ++ ".appup"]),
    ok = file:write_file(Tmp, git_show(Tag, Path)),
    case file:consult(Tmp) of
        {ok, Terms} -> Terms;
        _ -> []
    end.

merge_terms(NewTerm, CurTerms, OldTagTerms) ->
    NewV = element(1, NewTerm),
    ToMap = fun(Ts) ->
        lists:foldl(fun({V,_,_}=T, M) -> maps:put(V, T, M) end, #{}, Ts)
    end,
    M0 = ToMap(CurTerms),
    M1 = ToMap(OldTagTerms),
    M2 = maps:merge(M0, M1),
    M3 = maps:remove(NewV, M2),
    M4 = maps:put(NewV, NewTerm, M3),
    Keys = maps:keys(M4),
    Others = [K || K <- Keys, K =/= NewV],
    Sorted = lists:reverse(lists:sort(Others)),
    [NewTerm | [maps:get(K, M4) || K <- Sorted]].

write_terms(File, Terms) ->
    Bin = iolist_to_binary(
      lists:flatmap(fun(T) -> io_lib:format("~p.~n~n", [T]) end, Terms)
    ),
    file:write_file(File, Bin).

shq(Str) when is_list(Str) ->
    %% 简易 shell quote
    [$\',$\'] ++ escape_single_quotes(Str) ++ [$\',$\'];
shq(Str) when is_binary(Str) ->
    shq(binary_to_list(Str)).

escape_single_quotes([]) -> [];
escape_single_quotes([$' | T]) -> [$\\,$'] ++ escape_single_quotes(T);
escape_single_quotes([H | T]) -> [H | escape_single_quotes(T)].