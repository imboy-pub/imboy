%%%-------------------------------------------------------------------
%%% @doc Minimal RFC 6570 Level-1 URI Template matcher and expander.
%%%
%%% Used by the server's `resources/read' handler to route a
%%% concrete URI to a registered `resource_template'. We only
%%% implement Level 1 (`{var}' simple expansion) — that covers
%%% every template the spec's reference examples use, and it's
%%% the level the MCP server registry actually accepts.
%%%
%%% Examples:
%%% ```
%%% match(<<"file:///etc/hosts">>, <<"file:///{path}">>).
%%%   %% => {ok, #{<<"path">> => <<"etc/hosts">>}}
%%%
%%% match(<<"https://api.x/v1/users/42">>,
%%%       <<"https://api.x/v1/{kind}/{id}">>).
%%%   %% => {ok, #{<<"kind">> => <<"users">>,
%%%   %%          <<"id">>   => <<"42">>}}
%%%
%%% expand(<<"file:///{path}">>, #{<<"path">> => <<"etc/hosts">>}).
%%%   %% => {ok, <<"file:///etc/hosts">>}
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(barrel_mcp_uri_template).

-export([match/2, expand/2]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Match `Uri' against `Template'. Returns `{ok, Vars}' where
%% `Vars' is a binary-keyed map of substituted values, or `nomatch'
%% if the URI doesn't fit the template. Variables are matched
%% greedily but stop at literal segments (a value never crosses a
%% literal `/' that the template requires after the variable).
-spec match(binary(), binary()) -> {ok, map()} | nomatch.
match(Uri, Template) when is_binary(Uri), is_binary(Template) ->
    case parse(Template) of
        {ok, Tokens} ->
            do_match(Uri, Tokens, #{});
        {error, _} ->
            nomatch
    end.

%% @doc Expand `Template' by substituting `Vars'. Returns the
%% concrete URI or `{error, {missing_var, Name}}' if a variable
%% in the template has no value in `Vars'.
-spec expand(binary(), map()) -> {ok, binary()} | {error, term()}.
expand(Template, Vars) when is_binary(Template), is_map(Vars) ->
    case parse(Template) of
        {ok, Tokens} ->
            do_expand(Tokens, Vars, []);
        {error, _} = Err ->
            Err
    end.

%%====================================================================
%% Internal — template parser
%%====================================================================

%% Parse a template into a list of `{literal, Bin} | {var, Name}'
%% tokens.
parse(Template) ->
    parse(Template, <<>>, []).

parse(<<>>, Acc, Tokens) ->
    {ok, lists:reverse(emit_literal(Acc, Tokens))};
parse(<<"{", Rest/binary>>, Acc, Tokens) ->
    case binary:split(Rest, <<"}">>) of
        [Name, More] when Name =/= <<>> ->
            Tokens1 = emit_literal(Acc, Tokens),
            parse(More, <<>>, [{var, Name} | Tokens1]);
        _ ->
            {error, {malformed_template, template_for_error(Acc, Rest)}}
    end;
parse(<<C, Rest/binary>>, Acc, Tokens) ->
    parse(Rest, <<Acc/binary, C>>, Tokens).

emit_literal(<<>>, Tokens) -> Tokens;
emit_literal(Bin, Tokens) -> [{literal, Bin} | Tokens].

template_for_error(Acc, Rest) ->
    <<Acc/binary, Rest/binary>>.

%%====================================================================
%% Internal — match
%%====================================================================

do_match(<<>>, [], Vars) ->
    {ok, Vars};
do_match(_, [], _) ->
    nomatch;
do_match(Uri, [{literal, Lit} | Rest], Vars) ->
    case starts_with(Uri, Lit) of
        {true, Tail} -> do_match(Tail, Rest, Vars);
        false -> nomatch
    end;
do_match(Uri, [{var, Name}], Vars) ->
    %% Trailing variable — consume the rest. Empty value is
    %% allowed only if the template clearly expected an empty
    %% suffix; here we require at least one character.
    case Uri of
        <<>> -> nomatch;
        _ -> {ok, Vars#{Name => Uri}}
    end;
do_match(Uri, [{var, Name}, {literal, NextLit} | Rest], Vars) ->
    %% Variable followed by a literal: consume up to the next
    %% occurrence of NextLit.
    case binary:match(Uri, NextLit) of
        nomatch ->
            nomatch;
        {Pos, Len} when Pos > 0 ->
            <<Value:Pos/binary, _:Len/binary, Tail/binary>> = Uri,
            do_match(Tail, Rest, Vars#{Name => Value});
        {0, _} ->
            %% Empty variable value not allowed here.
            nomatch
    end;
do_match(Uri, [{var, Name1}, {var, Name2} | Rest], Vars) ->
    %% Two variables in a row with no literal between them —
    %% ambiguous. RFC 6570 doesn't require us to support this; we
    %% treat the first as everything up to the last separator and
    %% the second as the remainder, but most MCP templates avoid
    %% this shape. Implement simply: split on the next `/'.
    case binary:split(Uri, <<"/">>) of
        [V1, V2] when V1 =/= <<>>, V2 =/= <<>> ->
            do_match(
                <<>>,
                Rest,
                Vars#{Name1 => V1, Name2 => V2}
            );
        _ ->
            nomatch
    end.

starts_with(Bin, Prefix) ->
    case byte_size(Bin) >= byte_size(Prefix) of
        false ->
            false;
        true ->
            PSize = byte_size(Prefix),
            <<Head:PSize/binary, Tail/binary>> = Bin,
            case Head =:= Prefix of
                true -> {true, Tail};
                false -> false
            end
    end.

%%====================================================================
%% Internal — expand
%%====================================================================

do_expand([], _Vars, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
do_expand([{literal, Lit} | Rest], Vars, Acc) ->
    do_expand(Rest, Vars, [Lit | Acc]);
do_expand([{var, Name} | Rest], Vars, Acc) ->
    case maps:find(Name, Vars) of
        {ok, V} when is_binary(V) ->
            do_expand(Rest, Vars, [V | Acc]);
        {ok, V} when is_list(V) ->
            do_expand(Rest, Vars, [iolist_to_binary(V) | Acc]);
        error ->
            {error, {missing_var, Name}}
    end.
