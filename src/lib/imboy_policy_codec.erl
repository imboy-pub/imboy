-module(imboy_policy_codec).

%% @doc Pure codec / parsers extracted from imboy_policy.erl §7.
%% No side effects, no state dependencies. Called via imboy_policy_codec:XXX.

-export([
    parse_toggle_payload/1,
    parse_boolean_value/1,
    is_charlist/1,
    parse_storage_mode/1,
    parse_e2ee_mode/1,
    parse_audit_mode/1,
    normalize_retention_policy_payload/1,
    policy_error_result/4,
    policy_error_detail/4,
    policy_error_message/1,
    public_policy_error_detail/1,
    public_term/1,
    public_key/1,
    public_plugin_manifest/1,
    normalize_map/1
]).

-spec parse_toggle_payload(term()) -> {ok, boolean()} | error.
parse_toggle_payload(#{enabled := Enabled}) ->
    parse_boolean_value(Enabled);
parse_toggle_payload(#{<<"enabled">> := Enabled}) ->
    parse_boolean_value(Enabled);
parse_toggle_payload(Options) when is_list(Options) ->
    case is_charlist(Options) of
        true ->
            parse_boolean_value(Options);
        false ->
            case proplists:get_value(enabled, Options, undefined) of
                undefined ->
                    case proplists:get_value(<<"enabled">>, Options, undefined) of
                        undefined ->
                            error;
                        Enabled ->
                            parse_boolean_value(Enabled)
                    end;
                Enabled ->
                    parse_boolean_value(Enabled)
            end
    end;
parse_toggle_payload(Value) ->
    parse_boolean_value(Value).

-spec parse_boolean_value(term()) -> {ok, boolean()} | error.
parse_boolean_value(true) ->
    {ok, true};
parse_boolean_value(false) ->
    {ok, false};
parse_boolean_value(1) ->
    {ok, true};
parse_boolean_value(0) ->
    {ok, false};
parse_boolean_value(<<"true">>) ->
    {ok, true};
parse_boolean_value(<<"false">>) ->
    {ok, false};
parse_boolean_value("true") ->
    {ok, true};
parse_boolean_value("false") ->
    {ok, false};
parse_boolean_value(_) ->
    error.

-spec is_charlist(list()) -> boolean().
is_charlist([]) ->
    true;
is_charlist([H | T]) when is_integer(H), H >= 0, H =< 16#10FFFF ->
    is_charlist(T);
is_charlist(_) ->
    false.

-spec parse_storage_mode(term()) -> {ok, archived | compliance_e2ee | secure_e2ee} | error.
parse_storage_mode(archived) ->
    {ok, archived};
parse_storage_mode(compliance_e2ee) ->
    {ok, compliance_e2ee};
parse_storage_mode(secure_e2ee) ->
    {ok, secure_e2ee};
parse_storage_mode(<<"archived">>) ->
    {ok, archived};
parse_storage_mode(<<"compliance_e2ee">>) ->
    {ok, compliance_e2ee};
parse_storage_mode(<<"secure_e2ee">>) ->
    {ok, secure_e2ee};
parse_storage_mode("archived") ->
    {ok, archived};
parse_storage_mode("compliance_e2ee") ->
    {ok, compliance_e2ee};
parse_storage_mode("secure_e2ee") ->
    {ok, secure_e2ee};
parse_storage_mode(_) ->
    error.

-spec parse_e2ee_mode(term()) -> {ok, disabled | optional | compliance | required} | error.
parse_e2ee_mode(disabled) ->
    {ok, disabled};
parse_e2ee_mode(optional) ->
    {ok, optional};
parse_e2ee_mode(compliance) ->
    {ok, compliance};
parse_e2ee_mode(required) ->
    {ok, required};
parse_e2ee_mode(<<"disabled">>) ->
    {ok, disabled};
parse_e2ee_mode(<<"optional">>) ->
    {ok, optional};
parse_e2ee_mode(<<"compliance">>) ->
    {ok, compliance};
parse_e2ee_mode(<<"required">>) ->
    {ok, required};
parse_e2ee_mode("disabled") ->
    {ok, disabled};
parse_e2ee_mode("optional") ->
    {ok, optional};
parse_e2ee_mode("compliance") ->
    {ok, compliance};
parse_e2ee_mode("required") ->
    {ok, required};
parse_e2ee_mode(_) ->
    error.

-spec parse_audit_mode(term()) -> {ok, none | metadata | full} | error.
parse_audit_mode(none) ->
    {ok, none};
parse_audit_mode(metadata) ->
    {ok, metadata};
parse_audit_mode(full) ->
    {ok, full};
parse_audit_mode(<<"none">>) ->
    {ok, none};
parse_audit_mode(<<"metadata">>) ->
    {ok, metadata};
parse_audit_mode(<<"full">>) ->
    {ok, full};
parse_audit_mode("none") ->
    {ok, none};
parse_audit_mode("metadata") ->
    {ok, metadata};
parse_audit_mode("full") ->
    {ok, full};
parse_audit_mode(_) ->
    error.

-spec normalize_retention_policy_payload(term()) -> {ok, map()} | {error, map()}.
normalize_retention_policy_payload(Value) ->
    Policy = normalize_map(Value),
    case maps:size(Policy) of
        0 ->
            {error,
                policy_error_detail(
                    capabilities,
                    retention_policy,
                    invalid_object,
                    <<"invalid retention_policy value">>
                )};
        _ ->
            {ok, Policy}
    end.

-spec policy_error_result(atom() | undefined, atom() | undefined, atom(), binary()) ->
    {error, binary(), map()}.
policy_error_result(Section, Field, Reason, Message) ->
    Detail = policy_error_detail(Section, Field, Reason, Message),
    {error, Message, public_policy_error_detail(Detail)}.

-spec policy_error_detail(atom() | undefined, atom() | undefined, atom(), binary()) -> map().
policy_error_detail(Section, Field, Reason, Message) ->
    Detail0 = imboy_policy:maybe_put_saved_section(#{}, section, Section),
    Detail1 = imboy_policy:maybe_put_saved_section(Detail0, field, Field),
    Detail1#{
        reason => Reason,
        message => Message
    }.

-spec policy_error_message(map()) -> binary().
policy_error_message(Detail) ->
    maps:get(message, Detail).

-spec public_policy_error_detail(map()) -> map().
public_policy_error_detail(Detail) ->
    public_term(maps:remove(message, Detail)).

-spec public_term(term()) -> term().
public_term(Map) when is_map(Map) ->
    maps:from_list([
        {public_key(Key), public_term(Value)}
     || {Key, Value} <- maps:to_list(Map)
    ]);
public_term(List) when is_list(List) ->
    [public_term(Value) || Value <- List];
public_term(true) ->
    true;
public_term(false) ->
    false;
public_term(null) ->
    null;
public_term(undefined) ->
    null;
public_term(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
public_term(Value) ->
    Value.

-spec public_key(term()) -> binary().
public_key(Key) when is_binary(Key) ->
    Key;
public_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
public_key(Key) when is_list(Key) ->
    unicode:characters_to_binary(Key);
public_key(Key) ->
    ec_cnv:to_binary(Key).

-spec public_plugin_manifest(map()) -> map().
public_plugin_manifest(Manifest) ->
    AllowedKeys = [
        kind,
        feature_keys,
        requires_capabilities,
        depends_on_plugins,
        app_entries,
        admin_entries,
        api_handlers,
        children,
        enabled
    ],
    maps:with(AllowedKeys, Manifest).

-spec normalize_map(term()) -> map().
normalize_map(undefined) -> #{};
normalize_map(Value) when is_map(Value) -> Value;
normalize_map(Value) when is_list(Value) -> maps:from_list(Value);
normalize_map(_) -> #{}.
