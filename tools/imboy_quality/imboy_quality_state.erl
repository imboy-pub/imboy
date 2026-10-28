%% IMBOY_ERLANG_QUALITY P1-03：原子状态读写与安全新路径（纯 OTP，OTP29 json）。
%% 草稿（LP-03）正式化：write 临时文件 + sync + rename；拒绝 symlink/hardlink 目标。
-module(imboy_quality_state).
-export([write/2, read/1, new_path/2, trusted_context/1]).
-include_lib("kernel/include/file.hrl").

write(Path, State) when is_map(State) ->
    safe_parent(filename:dirname(filename:absname(Path))),
    validate_destination(Path),
    Bytes = iolist_to_binary(json:encode(State)),
    Temp = Path ++ ".tmp." ++ os:getpid(),
    {ok, Handle} = file:open(Temp, [write, binary, raw, exclusive]),
    try
        ok = file:write(Handle, Bytes),
        ok = file:sync(Handle)
    after
        ok = file:close(Handle)
    end,
    ok = file:rename(Temp, Path),
    {ok, crypto:hash(sha256, Bytes)}.

read(Path) ->
    validate_destination(Path),
    {ok, Bytes} = file:read_file(Path),
    State = json:decode(Bytes),
    true = is_map(State),
    {ok, State, crypto:hash(sha256, Bytes)}.

%% 从 STATE.json 提取可信执行上下文（cli:permit 的输入；缺失字段不默认放行）。
trusted_context(State) ->
    #{
        task_id => maps:get(<<"task_id">>, State, <<>>),
        execution_mode => map_atom(State, <<"execution_mode">>),
        execution_goal => map_atom(State, <<"execution_goal">>),
        owner_verified => owner_verified(State),
        heavy_admitted => heavy_admitted(State)
    }.

owner_verified(State) ->
    case maps:get(<<"owner">>, State, null) of
        #{<<"token">> := T} when is_binary(T), byte_size(T) > 0 -> true;
        _ -> false
    end.

map_atom(State, Key) ->
    case maps:get(Key, State, undefined) of
        <<"LIMITED_PARALLEL">> -> limited_parallel;
        <<"INTEGRATED_EXECUTION">> -> integrated_execution;
        <<"PHASE1_COMPLETE">> -> phase1_complete;
        <<"ALL_PHASES">> -> all_phases;
        Other -> Other
    end.

heavy_admitted(State) ->
    case maps:get(<<"resource_admission">>, State, undefined) of
        #{<<"status">> := <<"ADMITTED_FOR_HEAVY">>} -> true;
        #{<<"status">> := <<"ADMITTED">>} -> true;
        _ -> false
    end.

new_path(Root, Relative) ->
    try
        absolute = filename:pathtype(Root),
        relative = filename:pathtype(Relative),
        Parts = filename:split(Relative),
        true = Parts =/= [],
        false = lists:any(fun(Part) -> Part =:= ".." orelse Part =:= "." end, Parts),
        Path = filename:join(Root, Relative),
        safe_parent(filename:dirname(Path)),
        {error, enoent} = file:read_link_info(Path),
        {ok, Path}
    catch
        _:_ -> {error, unsafe_or_existing_path}
    end.

safe_parent("/") ->
    ok;
safe_parent(Path) ->
    absolute = filename:pathtype(Path),
    safe_parent(filename:dirname(Path)),
    case file:read_link_info(Path) of
        {ok, #file_info{type = directory}} -> ok;
        {error, enoent} -> ok;
        Other -> error({unsafe_parent, Path, Other})
    end.

validate_destination(Path) ->
    case file:read_link_info(Path) of
        {error, enoent} -> ok;
        {ok, #file_info{type = regular, links = 1}} -> ok;
        Other -> error({unsafe_destination, Path, Other})
    end.
