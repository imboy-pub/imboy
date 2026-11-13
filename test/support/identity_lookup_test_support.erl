-module(identity_lookup_test_support).
-export([mocks/1]).

%% Keep existing fixture rows and dynamic overrides while migrating to tagged queries.
mocks(Configs) ->
    [migrate(Module, Expectations) || {Module, Expectations} <- Configs].

migrate(Module, Expectations) when
    Module =:= user_ds; Module =:= adm_user_ds; Module =:= user_repo; Module =:= adm_user_repo
->
    HasLegacy = lists:any(
        fun({Function, Arity, _}) ->
            lists:member({Function, Arity}, [
                {find_by_id, 2},
                {find_by_account, 2},
                {find_by_email, 2},
                {find_by_mobile, 2},
                {find_id_by_email, 1},
                {find_id_by_mobile, 1}
            ])
        end,
        Expectations
    ),
    case HasLegacy andalso not lists:keymember(lookup, 1, Expectations) of
        true ->
            Lookup = fun(Field, Value, Columns) ->
                Function = field_function(Field),
                case lists:keymember(Function, 1, Expectations) of
                    true ->
                        {ok, apply(Module, Function, [Value, Columns])};
                    false ->
                        IdFunction = id_function(Field),
                        case apply(Module, IdFunction, [Value]) of
                            0 -> {ok, #{}};
                            Id when is_integer(Id), Id > 0 -> {ok, #{<<"id">> => Id}}
                        end
                end
            end,
            {Module, [{lookup, 3, Lookup} | Expectations]};
        false ->
            {Module, Expectations}
    end;
migrate(Module, Expectations) ->
    {Module, Expectations}.

field_function(id) -> find_by_id;
field_function(account) -> find_by_account;
field_function(email) -> find_by_email;
field_function(mobile) -> find_by_mobile.

id_function(email) -> find_id_by_email;
id_function(mobile) -> find_id_by_mobile.
