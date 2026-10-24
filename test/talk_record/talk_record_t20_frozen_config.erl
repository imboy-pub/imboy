%% Only a controller-reviewed configuration hash compiled into this module is trusted.
-module(talk_record_t20_frozen_config).
-export([load/0, verify/2, binding_keys/0, same/3, lease_window/3, controller_identity/3]).
-include_lib("kernel/include/file.hrl").
-ifndef(T20_FROZEN_CONFIG_SHA256).
-define(T20_FROZEN_CONFIG_SHA256, undefined).
-endif.

load() ->
    Pin = compiled_pin(),
    true = is_list(Pin) andalso length(Pin) =:= 64,
    Path = os:getenv("T20_FROZEN_CONFIG"),
    true = is_list(Path),
    absolute = filename:pathtype(Path),
    {ok, #file_info{type = regular, mode = Mode}} = file:read_link_info(Path),
    true = Mode band 8#077 =:= 0,
    {ok, Bytes} = file:read_file(Path),
    Hash = binary:encode_hex(crypto:hash(sha256, Bytes), lowercase),
    Hash = list_to_binary(Pin),
    Config = jsone:decode(Bytes),
    ok = verify(Config, erlang:system_time(second)),
    {Config, Hash}.

compiled_pin() -> ?T20_FROZEN_CONFIG_SHA256.

verify(C, Now) ->
    #{
        <<"approved">> := true,
        <<"purpose">> := <<"t20_case_execution">>,
        <<"valid_from_unix">> := From,
        <<"expires_at_unix">> := Until,
        <<"max_lease_seconds">> := Max,
        <<"controller_epoch">> := Epoch,
        <<"controller_pid">> := Pid,
        <<"host">> := <<"127.0.0.1">>,
        <<"connection_port">> := Port,
        <<"server_port">> := ServerPort,
        <<"migration_head">> := Head,
        <<"migration_count">> := Count,
        <<"migration_versions">> := Versions
    } = C,
    true = is_integer(From) andalso is_integer(Until) andalso From =< Now,
    true = Until >= Now + 120,
    true = is_integer(Max) andalso Max >= 120 andalso Max =< 14400,
    true = Until - From =< Max,
    true = is_integer(Epoch) andalso Epoch > 0,
    true = is_integer(Pid) andalso Pid > 0,
    true = is_integer(Port) andalso Port > 0 andalso Port =< 65535,
    true = is_integer(ServerPort) andalso ServerPort > 0 andalso ServerPort =< 65535,
    true =
        is_list(Versions) andalso Versions =/= [] andalso
            lists:all(fun(V) -> is_integer(V) andalso V > 0 end, Versions),
    Versions = lists:usort(Versions),
    Count = length(Versions),
    Head = lists:last(Versions),
    lists:foreach(
        fun(K) ->
            V = maps:get(K, C),
            true = is_binary(V) andalso byte_size(V) > 0
        end,
        [
            <<"owner">>,
            <<"controller_process_start">>,
            <<"controller_thread_id">>,
            <<"controller_host">>,
            <<"system_identifier">>,
            <<"user">>,
            <<"candidate_commit">>
        ]
    ),
    lists:foreach(
        fun(K) -> absolute = filename:pathtype(binary_to_list(maps:get(K, C))) end,
        [
            <<"repo">>,
            <<"runtime_root">>,
            <<"registry_path">>,
            <<"lease_path">>,
            <<"controller_lock_path">>,
            <<"controller_state_path">>
        ]
    ),
    true = is_map(maps:get(<<"source_sha256">>, C)),
    true = is_map(maps:get(<<"case_authorizations">>, C)),
    ok = verify_controller_schema(C),
    ok.

verify_controller_schema(C) ->
    case maps:get(<<"controller_schema">>, C, <<"legacy">>) of
        <<"legacy">> ->
            ok;
        <<"imboy_delivery_v3">> ->
            Run = maps:get(<<"controller_run_id">>, C),
            true = is_binary(Run) andalso byte_size(Run) > 0,
            ok;
        Other ->
            error({unsupported_controller_schema, Other})
    end.

binding_keys() ->
    [
        <<"owner">>,
        <<"controller_epoch">>,
        <<"controller_pid">>,
        <<"controller_process_start">>,
        <<"controller_thread_id">>,
        <<"controller_host">>,
        <<"controller_lock_path">>,
        <<"controller_state_path">>,
        <<"host">>,
        <<"connection_port">>,
        <<"server_port">>,
        <<"system_identifier">>,
        <<"user">>,
        <<"migration_head">>,
        <<"migration_count">>,
        <<"migration_versions">>,
        <<"candidate_commit">>,
        <<"source_sha256">>,
        <<"runtime_root">>,
        <<"valid_from_unix">>,
        <<"expires_at_unix">>
    ].

same(A, B, Keys) ->
    lists:foreach(fun(K) -> true = maps:get(K, A) =:= maps:get(K, B) end, Keys),
    ok.

lease_window(Lease, Config, Now) ->
    #{<<"issued_at_unix">> := Issued, <<"expires_at_unix">> := Until} = Lease,
    true = is_integer(Issued) andalso is_integer(Until),
    true = Issued >= maps:get(<<"valid_from_unix">>, Config) andalso Issued =< Now,
    true = Until >= Now + 120 andalso Until =< maps:get(<<"expires_at_unix">>, Config),
    true = Until - Issued =< maps:get(<<"max_lease_seconds">>, Config),
    ok.

controller_identity(R, Owner, State) ->
    case maps:get(<<"controller_schema">>, R, <<"legacy">>) of
        <<"legacy">> -> legacy_controller_identity(R, Owner, State);
        <<"imboy_delivery_v3">> -> v3_controller_identity(R, Owner, State);
        Other -> error({unsupported_controller_schema, Other})
    end.

%% Owner is a read-only lock-file observation, not a fabricated legacy owner.
%% The real V3 state is the fence; ACTIVE does not grant an exclusive test window.
v3_controller_identity(R, LockInfo, State) ->
    #{<<"type">> := <<"regular">>, <<"path">> := Lock} = LockInfo,
    Lock = maps:get(<<"controller_lock_path">>, R),
    Run = maps:get(<<"controller_run_id">>, R),
    Epoch = maps:get(<<"controller_epoch">>, R),
    Pid = maps:get(<<"controller_pid">>, R),
    Start = maps:get(<<"controller_process_start">>, R),
    #{
        <<"run_id">> := Run,
        <<"status">> := <<"ACTIVE">>,
        <<"controller">> := Controller
    } = State,
    #{<<"epoch">> := Epoch, <<"pid">> := Pid, <<"started">> := Start} = Controller,
    false = maps:get(<<"released">>, Controller, false),
    true = is_integer(Epoch) andalso Epoch > 0,
    true = is_integer(Pid) andalso Pid > 0,
    true = is_binary(Start) andalso byte_size(Start) > 0,
    ok.

legacy_controller_identity(R, Owner, State) ->
    Epoch = maps:get(<<"controller_epoch">>, R),
    Pid = maps:get(<<"controller_pid">>, R),
    Start = maps:get(<<"controller_process_start">>, R),
    Session = maps:get(<<"owner">>, R),
    Thread = maps:get(<<"controller_thread_id">>, R),
    Host = maps:get(<<"controller_host">>, R),
    Lock = maps:get(<<"controller_lock_path">>, R),
    #{
        <<"status">> := <<"RUNNING">>,
        <<"thread_id">> := Thread,
        <<"controller_epoch">> := Epoch,
        <<"pid">> := Pid,
        <<"process_start">> := Start,
        <<"session">> := Session,
        <<"host">> := Host
    } = Owner,
    #{
        <<"controller_epoch">> := Epoch,
        <<"status">> := <<"RUNNING">>,
        <<"controller">> := #{
            <<"pid">> := Pid,
            <<"process_start">> := Start,
            <<"thread_id">> := Thread,
            <<"session">> := Session,
            <<"host">> := Host,
            <<"lock_path">> := Lock
        }
    } = State,
    ok.
