-module(ai_agent_role_ds).

%% AI Agent 角色领域服务：校验配置、规范化策略，并提供运行时继承结果。

-export([
    page/3,
    find/1,
    create/1,
    update_metadata/2,
    save_draft/2,
    publish/3,
    set_status/2,
    normalize_policy/1,
    validate_config/1,
    effective_config/2
]).

-spec page(pos_integer(), pos_integer(), map()) -> {ok, map()} | {error, term()}.
page(Page, Size, Filters) ->
    ai_agent_role_repo:page(Page, Size, Filters).

-spec find(binary()) -> {ok, map()} | {error, term()}.
find(Code) ->
    ai_agent_role_repo:find(Code).

-spec create(map()) -> {ok, map()} | {error, term()}.
create(Data) ->
    case validate_role_identity(Data) of
        {ok, Config} ->
            ai_agent_role_repo:create(Config);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_metadata(binary(), map()) -> {ok, term()} | {error, term()}.
update_metadata(Code, Patch) ->
    case validate_metadata(Patch) of
        {ok, Normalized} -> ai_agent_role_repo:update_metadata(Code, Normalized);
        {error, Reason} -> {error, Reason}
    end.

-spec save_draft(binary(), map()) -> {ok, [map()]} | {error, term()}.
save_draft(Code, Data) ->
    case validate_config(Data#{<<"code">> => Code}) of
        {ok, Config} ->
            case ai_agent_role_repo:update_metadata(Code, Config) of
                {ok, _} -> ai_agent_role_repo:save_draft(Code, Config);
                {error, _} = Error -> Error
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec publish(binary(), pos_integer(), integer()) -> {ok, term()} | {error, term()}.
publish(Code, Version, PublishedBy) ->
    ai_agent_role_repo:publish(Code, Version, PublishedBy).

-spec set_status(binary(), 0 | 1) -> {ok, term()} | {error, term()}.
set_status(Code, Status) ->
    ai_agent_role_repo:set_status(Code, Status).

-spec normalize_policy(map()) -> {ok, map()} | {error, term()}.
normalize_policy(Policy) when is_map(Policy) ->
    case unknown_policy_key(Policy) of
        none ->
            Knowledge0 = maps:get(<<"knowledge">>, Policy, #{}),
            GroupReply0 = maps:get(<<"group_reply">>, Policy, #{}),
            Proactive0 = maps:get(<<"proactive">>, Policy, #{}),
            with_policy(
                normalize_knowledge(Knowledge0),
                normalize_group_reply(GroupReply0),
                normalize_proactive(Proactive0)
            );
        Key ->
            {error, {unknown_capability, Key}}
    end;
normalize_policy(_) ->
    {error, policy_must_be_map}.

-spec validate_config(map()) -> {ok, map()} | {error, term()}.
validate_config(Data) when is_map(Data) ->
    case required_binary(Data, <<"code">>, code_required) of
        ok ->
            case required_binary(Data, <<"name">>, name_required) of
                ok ->
                    case required_binary(Data, <<"system_prompt">>, system_prompt_required) of
                        ok ->
                            Capabilities = maps:get(<<"capabilities">>, Data, #{}),
                            Policy = maps:get(<<"knowledge_policy">>, Data, #{}),
                            case {normalize_capabilities(Capabilities), normalize_policy(Policy)} of
                                {{error, _} = Error, _} ->
                                    Error;
                                {_, {error, _} = Error} ->
                                    Error;
                                {{ok, NormalizedCapabilities}, {ok, NormalizedPolicy}} ->
                                    {ok, #{
                                        code => maps:get(<<"code">>, Data),
                                        name => maps:get(<<"name">>, Data),
                                        description => maps:get(<<"description">>, Data, <<>>),
                                        status => maps:get(<<"status">>, Data, 1),
                                        created_by => maps:get(<<"created_by">>, Data, 0),
                                        version => maps:get(<<"version">>, Data, 1),
                                        system_prompt => maps:get(<<"system_prompt">>, Data),
                                        capabilities => jsone:encode(
                                            NormalizedCapabilities, [native_utf8]
                                        ),
                                        knowledge_policy => jsone:encode(
                                            NormalizedPolicy, [native_utf8]
                                        )
                                    }}
                            end;
                        error ->
                            {error, system_prompt_required}
                    end;
                error ->
                    {error, name_required}
            end;
        error ->
            {error, code_required}
    end;
validate_config(_) ->
    {error, config_must_be_map}.

-spec normalize_capabilities(map()) -> {ok, map()} | {error, term()}.
normalize_capabilities(Capabilities) when is_map(Capabilities) ->
    Allowed = [<<"knowledge">>, <<"group_reply">>, <<"proactive">>],
    case [Key || Key <- maps:keys(Capabilities), not lists:member(Key, Allowed)] of
        [Key | _] ->
            {error, {unknown_capability, Key}};
        [] ->
            case
                [
                    Key
                 || {Key, Value} <- maps:to_list(Capabilities),
                    not is_boolean(Value)
                ]
            of
                [BadKey | _] -> {error, {capability_must_be_boolean, BadKey}};
                [] -> {ok, Capabilities}
            end
    end;
normalize_capabilities(_) ->
    {error, capabilities_must_be_map}.

validate_role_identity(Data) when is_map(Data) ->
    case required_binary(Data, <<"code">>, code_required) of
        ok ->
            case required_binary(Data, <<"name">>, name_required) of
                ok ->
                    {ok, #{
                        code => maps:get(<<"code">>, Data),
                        name => maps:get(<<"name">>, Data),
                        description => maps:get(<<"description">>, Data, <<>>),
                        status => maps:get(<<"status">>, Data, 1),
                        created_by => maps:get(<<"created_by">>, Data, 0)
                    }};
                error ->
                    {error, name_required}
            end;
        error ->
            {error, code_required}
    end;
validate_role_identity(_) ->
    {error, config_must_be_map}.

validate_metadata(Data) when is_map(Data) ->
    Name = maps:get(<<"name">>, Data, maps:get(name, Data, <<>>)),
    Description = maps:get(<<"description">>, Data, maps:get(description, Data, <<>>)),
    case {valid_text(Name), is_binary(Description)} of
        {true, true} -> {ok, #{name => Name, description => Description}};
        {false, _} -> {error, name_required};
        {_, false} -> {error, description_must_be_binary}
    end;
validate_metadata(_) ->
    {error, metadata_must_be_map}.

valid_text(Value) ->
    is_binary(Value) andalso byte_size(Value) > 0.

-spec effective_config(map(), map()) -> {ok, map()} | {error, term()}.
effective_config(Agent, Role) when is_map(Agent), is_map(Role) ->
    Policy = maps:get(<<"knowledge_policy">>, Role, #{}),
    case normalize_policy(Policy) of
        {ok, NormalizedPolicy} ->
            {ok, Agent#{
                <<"role_code">> => maps:get(<<"code">>, Role),
                <<"role_version">> => maps:get(<<"version">>, Role),
                <<"role_status">> => maps:get(<<"status">>, Role, 1),
                <<"system_prompt">> => maps:get(<<"system_prompt">>, Role),
                <<"capabilities">> => maps:get(<<"capabilities">>, Role, #{}),
                <<"knowledge_policy">> => NormalizedPolicy,
                <<"policy_source">> => <<"role">>
            }};
        {error, Reason} ->
            {error, Reason}
    end.

default_policy() ->
    #{
        <<"knowledge">> => #{
            <<"mode">> => <<"on_demand">>,
            <<"source">> => <<"all">>,
            <<"max_context_bytes">> => 2400
        },
        <<"group_reply">> => #{<<"mode">> => <<"off">>},
        <<"proactive">> => #{<<"mode">> => <<"off">>, <<"daily_limit">> => 0}
    }.

unknown_policy_key(Policy) ->
    Allowed = [<<"knowledge">>, <<"group_reply">>, <<"proactive">>],
    case [Key || Key <- maps:keys(Policy), not lists:member(Key, Allowed)] of
        [] -> none;
        [Key | _] -> Key
    end.

normalize_knowledge(Value) when is_map(Value) ->
    Mode = maps:get(<<"mode">>, Value, <<"on_demand">>),
    Source = maps:get(<<"source">>, Value, <<"all">>),
    MaxBytes = maps:get(<<"max_context_bytes">>, Value, 2400),
    case valid_member(Mode, [<<"off">>, <<"on_demand">>, <<"required">>]) of
        false ->
            {error, {invalid_mode, <<"knowledge">>, Mode}};
        true ->
            case valid_member(Source, [<<"all">>, <<"faq">>, <<"group_rule">>]) of
                false ->
                    {error, {invalid_source, Source}};
                true when is_integer(MaxBytes), MaxBytes >= 0, MaxBytes =< 8000 ->
                    {ok, #{
                        <<"mode">> => Mode,
                        <<"source">> => Source,
                        <<"max_context_bytes">> => MaxBytes
                    }};
                true ->
                    {error, {invalid_max_context_bytes, MaxBytes}}
            end
    end;
normalize_knowledge(_) ->
    {error, {invalid_policy, <<"knowledge">>}}.

normalize_group_reply(Value) when is_map(Value) ->
    Mode = maps:get(<<"mode">>, Value, <<"off">>),
    case valid_member(Mode, [<<"off">>, <<"mention_only">>]) of
        true -> {ok, #{<<"mode">> => Mode}};
        false -> {error, {invalid_mode, <<"group_reply">>, Mode}}
    end;
normalize_group_reply(_) ->
    {error, {invalid_policy, <<"group_reply">>}}.

normalize_proactive(Value) when is_map(Value) ->
    Mode = maps:get(<<"mode">>, Value, <<"off">>),
    DailyLimit = maps:get(<<"daily_limit">>, Value, 0),
    case valid_member(Mode, [<<"off">>, <<"welcome_only">>]) of
        false ->
            {error, {invalid_mode, <<"proactive">>, Mode}};
        true when is_integer(DailyLimit), DailyLimit >= 0, DailyLimit =< 1000 ->
            {ok, #{<<"mode">> => Mode, <<"daily_limit">> => DailyLimit}};
        true ->
            {error, {invalid_daily_limit, DailyLimit}}
    end;
normalize_proactive(_) ->
    {error, {invalid_policy, <<"proactive">>}}.

with_policy({ok, Knowledge}, {ok, GroupReply}, {ok, Proactive}) ->
    {ok, #{
        <<"knowledge">> => Knowledge,
        <<"group_reply">> => GroupReply,
        <<"proactive">> => Proactive
    }};
with_policy({error, Reason}, _, _) ->
    {error, Reason};
with_policy(_, {error, Reason}, _) ->
    {error, Reason};
with_policy(_, _, {error, Reason}) ->
    {error, Reason}.

valid_member(Value, Values) ->
    lists:member(Value, Values).

required_binary(Data, Key, _Error) ->
    case maps:get(Key, Data, <<>>) of
        Value when is_binary(Value), byte_size(Value) > 0 -> ok;
        _ -> error
    end.
