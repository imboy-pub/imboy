%% Observations are evidence, never an automatically frozen expected universe.
%% Context digests must describe the candidate, compiled inputs and provisioning.
%% otp/erts bind the runtime; root_contract binds the root selection, generator
%% and fixture wrapper contract. Callers supply these independently of results.
-module(imboy_quality_leaf_identity).
-export([observed_manifest/3, compare/2, global_id/2]).

observed_manifest(Context, Roots, PerRoot) ->
    try
        ok = context(Context),
        true = Roots =/= [],
        true = length(Roots) =:= length(lists:usort(Roots)),
        true = lists:sort(Roots) =:= lists:sort(maps:keys(PerRoot)),
        Leaves = lists:sort(lists:append([root(R, maps:get(R, PerRoot)) || R <- Roots])),
        {ok, #{
            schema => 1,
            kind => observed,
            context => Context,
            roots => lists:sort(Roots),
            leaves => Leaves
        }}
    catch
        Class:Reason -> {error, {invalid_observation, Class, Reason}}
    end.

%% Expected must be supplied by a separately reviewed obligation manifest.
%% A caller cannot pass the observed artifact as expected by accident.
compare(undefined, _) ->
    {error, expected_manifest_unavailable};
compare(
    #{schema := 1, kind := expected} = Expected,
    #{schema := 1, kind := observed} = Observed
) ->
    try
        ok = manifest(Expected),
        ok = manifest(Observed),
        true = maps:get(context, Expected) =:= maps:get(context, Observed),
        true = maps:get(roots, Expected) =:= maps:get(roots, Observed),
        E = maps:get(leaves, Expected),
        O = maps:get(leaves, Observed),
        case {ordsets:subtract(E, O), ordsets:subtract(O, E)} of
            {[], []} -> ok;
            {Missing, Unexpected} -> {error, #{missing => Missing, unexpected => Unexpected}}
        end
    catch
        Class:Reason -> {error, {invalid_or_unbound_manifest, Class, Reason}}
    end;
compare(_, _) ->
    {error, invalid_manifest_kind}.

context(C) when is_map(C) ->
    lists:foreach(
        fun(K) ->
            V = maps:get(K, C),
            true = is_binary(V) andalso byte_size(V) > 0
        end,
        [candidate, build, provision, otp, erts, root_contract]
    ),
    ok.

manifest(#{context := Context, roots := Roots, leaves := Leaves}) ->
    ok = context(Context),
    true = Roots =/= [],
    true = Roots =:= lists:usort(Roots),
    true = Leaves =/= [],
    true = Leaves =:= lists:sort(Leaves),
    Ids = [
        begin
            #{id := Id, root := R, path := P, metadata := M} = L,
            true = lists:member(R, Roots),
            true = Id =:= global_id(R, P),
            ok = metadata(M),
            Id
        end
     || L <- Leaves
    ],
    true = length(Ids) =:= length(lists:usort(Ids)),
    true = Roots =:= lists:usort([maps:get(root, L) || L <- Leaves]),
    ok.

root(Root, #{exit := ok, counts := Counts, events := Events}) ->
    #{pass := Pass, fail := 0, skip := 0, cancel := 0} = Counts,
    true = is_integer(Pass) andalso Pass > 0,
    {Open, Done, closed, true} = lists:foldl(
        fun event/2,
        {#{}, #{}, open, false},
        Events
    ),
    true = map_size(Open) =:= 0,
    true = map_size(Done) =:= Pass,
    {result, {ok, Summary}} = lists:last(Events),
    true = lists:sort(Summary) =:= lists:sort(maps:to_list(Counts)),
    [
        #{id => global_id(Root, P), root => Root, path => P, metadata => M}
     || {P, M} <- maps:to_list(Done)
    ].

event({test_begin, P}, {Open, Done, open, false}) ->
    false = maps:is_key(P, Open) orelse maps:is_key(P, Done),
    {Open#{P => pending}, Done, open, false};
event({leaf_metadata, P, M}, {Open, Done, open, false}) ->
    pending = maps:get(P, Open),
    ok = metadata(M),
    {Open#{P => M}, Done, open, false};
event({test_end, P, ok, _}, {Open, Done, open, false}) ->
    M = maps:get(P, Open),
    ok = metadata(M),
    {maps:remove(P, Open), Done#{P => M}, open, false};
event({group_end, [], _}, {Open, Done, open, false}) ->
    {Open, Done, closed, false};
event({group_end, [_ | _], _}, {Open, Done, open, false}) ->
    {Open, Done, open, false};
event({result, {ok, _}}, {Open, Done, closed, false}) ->
    {Open, Done, closed, true}.

metadata(#{source := {M, F, A}, line := Line, desc := Desc}) ->
    true = is_atom(M) andalso is_atom(F) andalso is_integer(A) andalso A >= 0,
    true = is_integer(Line) andalso Line >= 0,
    true = Desc =:= undefined orelse is_binary(Desc),
    ok.

%% Root is the namespace; positional path stays stable across worker assignment.
%% Delimiters cannot collide even for a quoted module atom containing ':' or '.'.
global_id(Root, [_ | _] = Path) when is_atom(Root) ->
    true = lists:all(fun(N) -> is_integer(N) andalso N > 0 end, Path),
    R = atom_to_binary(Root, utf8),
    P = lists:join($., [integer_to_binary(N) || N <- Path]),
    iolist_to_binary([integer_to_binary(byte_size(R)), $:, R, $:, P]).
