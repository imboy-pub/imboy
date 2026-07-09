%%%-------------------------------------------------------------------
%%% @doc Minimal JSON Schema validator for MCP tool inputs.
%%%
%%% Implements the subset of JSON Schema that real MCP tools use, in
%%% pure Erlang with no extra dependencies. Covers:
%%%
%%% <ul>
%%%   <li>`type': `object', `array', `string', `number', `integer',
%%%       `boolean', `null', or a list of those for unions.</li>
%%%   <li>`properties' + `required' for objects.</li>
%%%   <li>`additionalProperties: false' rejection.</li>
%%%   <li>`items' for arrays (single schema applied to every item).</li>
%%%   <li>`enum' for string/number/integer/boolean.</li>
%%%   <li>`oneOf' / `anyOf' / `allOf' (allOf = each must validate).</li>
%%%   <li>`minimum'/`maximum'/`exclusiveMinimum'/`exclusiveMaximum'
%%%       for numbers and integers.</li>
%%%   <li>`minLength'/`maxLength'/`pattern' for strings.</li>
%%%   <li>`minItems'/`maxItems'/`uniqueItems' for arrays.</li>
%%% </ul>
%%%
%%% Returns `ok' or `{error, [Error]}' where each error is a tuple
%%% `{Path, Reason}' (Path is a list of keys / indices from the root,
%%% Reason an atom or `{atom, Detail}').
%%%
%%% Anything outside the supported subset is silently accepted; this
%%% is a permissive validator by design — its job is to catch obvious
%%% bugs before sending an LLM-generated arg map to a remote tool, not
%%% to be a JSON Schema reference implementation.
%%% @end
%%%-------------------------------------------------------------------
-module(barrel_mcp_schema).

-export([validate/2]).

-type path() :: [binary() | non_neg_integer()].
-type error() :: {path(), atom() | {atom(), term()}}.

-export_type([error/0]).

%% @doc Validate `Value' against `Schema'. Returns `ok' or
%% `{error, [Error]}'.
-spec validate(term(), map()) -> ok | {error, [error()]}.
validate(Value, Schema) when is_map(Schema) ->
    case do_validate(Value, Schema, []) of
        [] -> ok;
        Errors -> {error, lists:reverse(Errors)}
    end;
validate(_, _) ->
    {error, [{[], invalid_schema}]}.

%%====================================================================
%% Core dispatch
%%====================================================================

do_validate(Value, Schema, Path) ->
    Errs0 = check_type(Value, maps:get(<<"type">>, Schema, undefined), Path),
    Errs1 =
        case Errs0 of
            [_ | _] -> Errs0;
            [] -> check_constraints(Value, Schema, Path)
        end,
    Errs1.

check_constraints(Value, Schema, Path) ->
    lists:foldl(
        fun(Check, Acc) ->
            Check(Value, Schema, Path) ++ Acc
        end,
        [],
        [
            fun check_enum/3,
            fun check_object/3,
            fun check_array/3,
            fun check_string/3,
            fun check_number/3,
            fun check_combinators/3
        ]
    ).

%%====================================================================
%% Type
%%====================================================================

check_type(_Value, undefined, _Path) ->
    [];
check_type(Value, Types, Path) when is_list(Types) ->
    case lists:any(fun(T) -> matches_type(Value, T) end, Types) of
        true -> [];
        false -> [{Path, {type_mismatch, Types}}]
    end;
check_type(Value, Type, Path) when is_binary(Type) ->
    case matches_type(Value, Type) of
        true -> [];
        false -> [{Path, {type_mismatch, Type}}]
    end.

matches_type(V, <<"object">>) -> is_map(V);
matches_type(V, <<"array">>) -> is_list(V);
matches_type(V, <<"string">>) -> is_binary(V);
matches_type(V, <<"integer">>) -> is_integer(V);
matches_type(V, <<"number">>) -> is_integer(V) orelse is_float(V);
matches_type(V, <<"boolean">>) -> is_boolean(V);
matches_type(null, <<"null">>) -> true;
matches_type(_, <<"null">>) -> false;
matches_type(_, _) -> true.

%%====================================================================
%% enum
%%====================================================================

check_enum(Value, #{<<"enum">> := Allowed}, Path) when is_list(Allowed) ->
    case lists:member(Value, Allowed) of
        true -> [];
        false -> [{Path, {not_in_enum, Allowed}}]
    end;
check_enum(_, _, _) ->
    [].

%%====================================================================
%% Object
%%====================================================================

check_object(Value, Schema, Path) when is_map(Value) ->
    Required = maps:get(<<"required">>, Schema, []),
    Properties = maps:get(<<"properties">>, Schema, #{}),
    AdditionalAllowed = maps:get(<<"additionalProperties">>, Schema, true),
    MissingErrs = [
        {Path, {missing_required, K}}
     || K <- Required, not maps:is_key(K, Value)
    ],
    PropErrs = lists:flatten(
        maps:fold(
            fun(K, V, Acc) ->
                case maps:find(K, Properties) of
                    {ok, Sub} ->
                        [do_validate(V, Sub, Path ++ [K]) | Acc];
                    error ->
                        case AdditionalAllowed of
                            false -> [[{Path, {unexpected_property, K}}] | Acc];
                            _ -> Acc
                        end
                end
            end,
            [],
            Value
        )
    ),
    MissingErrs ++ PropErrs;
check_object(_, _, _) ->
    [].

%%====================================================================
%% Array
%%====================================================================

check_array(Value, Schema, Path) when is_list(Value) ->
    LenErrs = length_errors(Value, Schema, Path),
    UniqErrs =
        case maps:get(<<"uniqueItems">>, Schema, false) of
            true ->
                case length(lists:usort(Value)) =:= length(Value) of
                    true -> [];
                    false -> [{Path, items_not_unique}]
                end;
            _ ->
                []
        end,
    ItemErrs =
        case maps:get(<<"items">>, Schema, undefined) of
            undefined ->
                [];
            Sub when is_map(Sub) ->
                {Errs, _} = lists:foldl(
                    fun(V, {Acc, I}) ->
                        {do_validate(V, Sub, Path ++ [I]) ++ Acc, I + 1}
                    end,
                    {[], 0},
                    Value
                ),
                Errs;
            _ ->
                []
        end,
    LenErrs ++ UniqErrs ++ ItemErrs;
check_array(_, _, _) ->
    [].

length_errors(List, Schema, Path) ->
    Len = length(List),
    Min = maps:get(<<"minItems">>, Schema, undefined),
    Max = maps:get(<<"maxItems">>, Schema, undefined),
    [
        {Path, {too_short, {min, Min}, {got, Len}}}
     || Min =/= undefined, Len < Min
    ] ++
        [
            {Path, {too_long, {max, Max}, {got, Len}}}
         || Max =/= undefined, Len > Max
        ].

%%====================================================================
%% String
%%====================================================================

check_string(Value, Schema, Path) when is_binary(Value) ->
    MinL = maps:get(<<"minLength">>, Schema, undefined),
    MaxL = maps:get(<<"maxLength">>, Schema, undefined),
    Len = byte_size(Value),
    [
        {Path, {too_short, {min, MinL}, {got, Len}}}
     || MinL =/= undefined, Len < MinL
    ] ++
        [
            {Path, {too_long, {max, MaxL}, {got, Len}}}
         || MaxL =/= undefined, Len > MaxL
        ] ++
        pattern_errors(Value, maps:get(<<"pattern">>, Schema, undefined), Path);
check_string(_, _, _) ->
    [].

pattern_errors(_, undefined, _) ->
    [];
pattern_errors(Value, Pattern, Path) when is_binary(Pattern) ->
    case re:run(Value, Pattern, [{capture, none}, unicode]) of
        match -> [];
        nomatch -> [{Path, {pattern_mismatch, Pattern}}];
        {error, _} -> []
    end.

%%====================================================================
%% Number
%%====================================================================

check_number(Value, Schema, Path) when is_integer(Value); is_float(Value) ->
    Bounds = [
        {<<"minimum">>, fun(V, B) -> V >= B end, too_small},
        {<<"maximum">>, fun(V, B) -> V =< B end, too_large},
        {<<"exclusiveMinimum">>, fun(V, B) -> V > B end, too_small},
        {<<"exclusiveMaximum">>, fun(V, B) -> V < B end, too_large}
    ],
    lists:foldl(
        fun({Key, Pred, ErrTag}, Acc) ->
            case maps:get(Key, Schema, undefined) of
                undefined ->
                    Acc;
                Bound ->
                    case Pred(Value, Bound) of
                        true -> Acc;
                        false -> [{Path, {ErrTag, {Key, Bound}, {got, Value}}} | Acc]
                    end
            end
        end,
        [],
        Bounds
    );
check_number(_, _, _) ->
    [].

%%====================================================================
%% allOf / anyOf / oneOf
%%====================================================================

check_combinators(Value, Schema, Path) ->
    AllOf = check_all_of(Value, maps:get(<<"allOf">>, Schema, undefined), Path),
    AnyOf = check_any_of(Value, maps:get(<<"anyOf">>, Schema, undefined), Path),
    OneOf = check_one_of(Value, maps:get(<<"oneOf">>, Schema, undefined), Path),
    AllOf ++ AnyOf ++ OneOf.

check_all_of(_, undefined, _) ->
    [];
check_all_of(Value, Schemas, Path) when is_list(Schemas) ->
    lists:flatten([do_validate(Value, S, Path) || S <- Schemas]).

check_any_of(_, undefined, _) ->
    [];
check_any_of(Value, Schemas, Path) when is_list(Schemas) ->
    case lists:any(fun(S) -> do_validate(Value, S, Path) =:= [] end, Schemas) of
        true -> [];
        false -> [{Path, no_anyof_match}]
    end.

check_one_of(_, undefined, _) ->
    [];
check_one_of(Value, Schemas, Path) when is_list(Schemas) ->
    Matches = length([1 || S <- Schemas, do_validate(Value, S, Path) =:= []]),
    case Matches of
        1 -> [];
        0 -> [{Path, no_oneof_match}];
        N -> [{Path, {multiple_oneof_matches, N}}]
    end.
