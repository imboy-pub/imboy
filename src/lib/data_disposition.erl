-module(data_disposition).
%%%===================================================================
%%% @doc 数据处置清单（data-disposition.yml）解析与覆盖校验
%%%
%%% Implementation Plan Task D-02。清单是账号删除链的处置真源：
%%% D-03 编排器按 tables 的 action 执行；校验测试断言当前 schema
%%% 的每张表都被映射或显式排除。
%%%
%%% 受限 YAML 子集（见 yml 文件头约定）：
%%%   * 顶层段：meta / tables / exclusions
%%%   * tables/exclusions 每表一块：两空格缩进表名，四空格缩进字段行
%%%   * value 内禁止出现 ": "（冒号+空格）
%%% 超集语法一律 {error, {unsupported_line, ...}} 报错——宁严勿漏。
%%%===================================================================

-export([parse/1, parse_file/1, coverage/3]).

-define(TABLE_FIELDS, [action, owner, reason, mechanism, review, retention]).
-define(REQUIRED_TABLE_FIELDS, [action, reason, review]).
-define(ACTIONS, [delete, anonymize, retain]).
-define(REVIEWS, [<<"self-evident">>, <<"pending-owner">>, <<"owner-approved">>]).

-export_type([disposition/0]).

-opaque disposition() :: #{
    table := binary(),
    action := delete | anonymize | retain | exclude,
    reason := binary(),
    review => binary(),
    owner => binary(),
    mechanism => binary(),
    retention => binary()
}.

%% @doc 从文件解析
-spec parse_file(file:name_all()) -> {ok, [disposition()]} | {error, term()}.
parse_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> parse(Bin);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 解析受限 YAML 子集，返回逐表处置记录
-spec parse(binary()) -> {ok, [disposition()]} | {error, term()}.
parse(Bin) ->
    Lines = [trim_tailing_cr(L) || L <- binary:split(Bin, <<"\n">>, [global, trim_all])],
    try
        {ok, finalize(parse_lines(Lines, none, none, #{}))}
    catch
        throw:{parse_error, Reason} -> {error, Reason}
    end.

%% ---- 解析主循环 ------------------------------------------------------
%% Current 携带"正在收集的表块"，在下一段头/下一表名/输入结束时冲入 Acc

parse_lines([], _Section, none, Acc) ->
    Acc;
parse_lines([], _Section, {Name, Fields}, Acc) ->
    Acc#{Name => Fields};
parse_lines([Line | Rest], Section, Current, Acc) ->
    case Line of
        <<>> ->
            parse_lines(Rest, Section, Current, Acc);
        <<"#", _/binary>> ->
            parse_lines(Rest, Section, Current, Acc);
        <<"meta:">> ->
            parse_lines(Rest, meta, none, Acc);
        <<"tables:">> ->
            parse_lines(Rest, tables, none, flush_current(Current, Acc));
        <<"exclusions:">> ->
            parse_lines(Rest, exclusions, none, flush_current(Current, Acc));
        _ when Section =:= none ->
            parse_error(Line, <<"顶层只允许 meta:/tables:/exclusions: 段头">>);
        _ when Section =:= meta ->
            %% meta 段字段（schema_version 等）：解析器不消费，跳过
            parse_lines(Rest, Section, Current, Acc);
        _ ->
            case classify_line(Line) of
                {entry, Name} ->
                    %% 新表名：冲入上一块，开启新块
                    parse_lines(Rest, Section, {Name, #{}}, flush_current(Current, Acc));
                {field, Key, Value} ->
                    NewCurrent = apply_field(Current, Key, Value, Section, Line),
                    parse_lines(Rest, Section, NewCurrent, Acc);
                unsupported ->
                    parse_error(Line, <<"无法解析的行（受限子集：两空格表名/四空格字段行）">>)
            end
    end.

flush_current(none, Acc) ->
    Acc;
flush_current({Name, Fields}, Acc) ->
    Acc#{Name => Fields}.

%% 行分类：
%%   "  table_name:"       → {entry, 表名}
%%   "    key: value"      → {field, key, value}
%%   其余                  → unsupported
classify_line(Line) ->
    case {Line, binary:split(Line, <<":">>)} of
        {<<"  ", _/binary>>, [Name, <<>>]} ->
            Trimmed = string:trim(Name),
            case is_valid_name(Trimmed) of
                true -> {entry, Trimmed};
                false -> unsupported
            end;
        {<<"    ", _/binary>>, _} ->
            case binary:split(string:trim(Line), <<": ">>) of
                [Key, Value] ->
                    TrimmedKey = string:trim(Key),
                    case is_valid_name(TrimmedKey) of
                        true -> {field, TrimmedKey, string:trim(Value)};
                        false -> unsupported
                    end;
                _ ->
                    unsupported
            end;
        _ ->
            unsupported
    end.

is_valid_name(Name) ->
    Name =/= <<>> andalso
        binary:match(Name, <<" ">>) =:= nomatch andalso
        binary:match(Name, <<":">>) =:= nomatch.

apply_field(none, _Key, _Value, Section, Line) ->
    parse_error(
        Line,
        unicode:characters_to_binary(
            io_lib:format("字段行出现在任何表名之前（~ts 段）", [Section])
        )
    );
apply_field({Name, Fields}, Key, Value, tables, Line) ->
    case lists:member(Key, [atom_to_binary(F, utf8) || F <- ?TABLE_FIELDS]) of
        true ->
            {Name, Fields#{binary_to_atom(Key, utf8) => Value}};
        false ->
            parse_error(
                Line,
                unicode:characters_to_binary(io_lib:format("tables 未知字段 ~ts", [Key]))
            )
    end;
apply_field({Name, Fields}, <<"reason">>, Value, exclusions, _Line) ->
    {Name, Fields#{reason => Value}};
apply_field({_Name, _Fields}, Key, _Value, exclusions, Line) ->
    parse_error(
        Line,
        unicode:characters_to_binary(io_lib:format("exclusions 只允许 reason，收到 ~ts", [Key]))
    ).

%% ---- 收口与校验 ------------------------------------------------------

finalize(_Entries = Entries) when map_size(Entries) =:= 0 ->
    throw({parse_error, empty_manifest});
finalize(Entries) ->
    maps:values(maps:map(fun validate_entry/2, Entries)).

validate_entry(Name, Fields) ->
    IsTable = maps:is_key(action, Fields),
    HasReason = maps:is_key(reason, Fields),
    if
        IsTable ->
            validate_table_entry(Name, Fields);
        HasReason ->
            %% exclusions 块：只有 reason
            #{
                table => Name,
                action => exclude,
                reason => maps:get(reason, Fields)
            };
        true ->
            throw({parse_error, {missing_fields, Name, [action, reason]}})
    end.

validate_table_entry(Name, Fields) ->
    Missing = [F || F <- ?REQUIRED_TABLE_FIELDS, not maps:is_key(F, Fields)],
    case Missing of
        [] -> ok;
        _ -> throw({parse_error, {missing_fields, Name, Missing}})
    end,
    Action = binary_to_atom(maps:get(action, Fields), utf8),
    ok = ensure(lists:member(Action, ?ACTIONS), {bad_action, Name, maps:get(action, Fields)}),
    Review = maps:get(review, Fields),
    ok = ensure(lists:member(Review, ?REVIEWS), {bad_review, Name, Review}),
    #{
        table => Name,
        action => Action,
        reason => maps:get(reason, Fields),
        review => Review,
        owner => maps:get(owner, Fields, <<>>),
        mechanism => maps:get(mechanism, Fields, <<>>),
        retention => maps:get(retention, Fields, <<>>)
    }.

ensure(true, _Reason) -> ok;
ensure(false, Reason) -> throw({parse_error, Reason}).

parse_error(Line, Msg) ->
    throw({parse_error, {unsupported_line, Line, Msg}}).

%% ---- 覆盖校验 ------------------------------------------------------

%% @doc 覆盖校验：当前 schema 的每张表必须被映射（tables）或显式排除
%%（exclusions）。反方向也查：映射/排除指向不存在表 = 陈旧条目。
%% Dispositions 为 parse/1 的结果；LiveTables 为当前 schema 表名。
-spec coverage([disposition()], [binary()], [binary()]) ->
    {ok, map()} | {error, map()}.
coverage(Dispositions, _ExcludedTables, LiveTables) ->
    Live = lists:usort(LiveTables),
    MappedNames = [
        maps:get(table, D)
     || D <- Dispositions,
        maps:get(action, D) =/= exclude
    ],
    ExclNames = [
        maps:get(table, D)
     || D <- Dispositions,
        maps:get(action, D) =:= exclude
    ],
    Known = lists:usort(MappedNames ++ ExclNames),
    Unmapped = [T || T <- Live, not lists:member(T, Known)],
    Stale = [T || T <- Known, not lists:member(T, Live)],
    PendingOwner = [
        maps:get(table, D)
     || D <- Dispositions,
        maps:get(review, D, <<>>) =:= <<"pending-owner">>
    ],
    Report = #{
        live_tables => length(Live),
        mapped => length(MappedNames),
        excluded => length(ExclNames),
        unmapped => Unmapped,
        stale => Stale,
        pending_owner => PendingOwner
    },
    case {Unmapped, Stale} of
        {[], []} -> {ok, Report};
        _ -> {error, Report}
    end.

trim_tailing_cr(Line) ->
    binary:replace(Line, <<"\r">>, <<>>).
