-module(talk_record_capture_input).
-moduledoc "面聊采集输入的纯校验与规范化；不访问DS、Repo或运行时资源。".
-export([
    parse_capture_create/1,
    parse_event/1,
    parse_audio_part/1,
    parse_confirm_receipt/1,
    parse_device_session/1,
    parse_abandon/1,
    valid_events/0,
    run_checks/3,
    is_uuid/1,
    is_printable_ascii/1,
    valid_part_codec/1
]).

-spec run_checks([function()], map(), map()) -> {ok, map()} | {error, atom()}.
run_checks([], _Body, Acc) ->
    {ok, Acc};
run_checks([Check | Rest], Body, Acc) ->
    case Check(Body) of
        {ok, Extra} -> run_checks(Rest, Body, maps:merge(Acc, Extra));
        {error, Reason} -> {error, Reason}
    end.

-spec is_uuid(binary()) -> boolean().
is_uuid(Bin) ->
    Re = <<"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$">>,
    try
        case re:run(Bin, Re) of
            {match, _} -> true;
            nomatch -> false
        end
    catch
        _:_ -> false
    end.

-spec is_printable_ascii(binary()) -> boolean().
is_printable_ascii(<<C, Rest/binary>>) when C >= 16#20, C =< 16#7E ->
    is_printable_ascii(Rest);
is_printable_ascii(<<>>) ->
    true;
is_printable_ascii(_) ->
    false.

%% ===================================================================
%% T07 采集域入参解析（§4/§8/§11.1）
%% ===================================================================

-define(CAPTURE_EVENTS, [<<"start">>, <<"pause">>, <<"resume">>, <<"interrupt">>, <<"end">>]).
-define(MAX_DEVICE_ID_BYTES, 128).

%% @doc POST /:id/captures 请求体：device_session_id UUID 必填；
%% device_id 1..128 可见 ASCII；reported_started_at 可选 RFC3339 binary。
-spec parse_capture_create(map()) -> {ok, map()} | {error, atom()}.
parse_capture_create(Body) when is_map(Body) ->
    Checks = [
        fun(B) -> check_device_session(B) end,
        fun(B) -> check_device_id(B) end,
        fun(B) -> check_reported_started_at(B) end
    ],
    run_checks(Checks, Body, #{});
parse_capture_create(_) ->
    {error, invalid_body}.

%% @doc 事件请求体（§4）：client_event_id/device_session_id UUID、seq>=0、
%% event ∈ start/pause/resume/interrupt/end、position_ms>=0、reported_at 可选。
-spec parse_event(map()) -> {ok, map()} | {error, atom()}.
parse_event(Body) when is_map(Body) ->
    Checks = [
        fun(B) -> check_client_event_id(B) end,
        fun(B) -> check_device_session(B) end,
        fun(B) -> check_event_seq(B) end,
        fun(B) -> check_event_kind(B) end,
        fun(B) -> check_event_position(B) end,
        fun(B) -> check_reported_started_at(B) end
    ],
    run_checks(Checks, Body, #{});
parse_event(_) ->
    {error, invalid_body}.

%% @doc 分段注册请求体（§4/§11.1）：seq>=0；end_ms>start_ms>=0（暂停不产生
%% 空段）；capture_offset_ms>=0 缺省取 start_ms（有效音频时间轴）；sha256
%% 64 位小写十六进制；byte_size>0；codec 必须显式为 aac；sample_rate/channels 可选。
-spec parse_audio_part(map()) -> {ok, map()} | {error, atom()}.
parse_audio_part(Body) when is_map(Body) ->
    Checks = [
        fun(B) -> check_part_seq(B) end,
        fun(B) -> check_part_time_axis(B) end,
        fun(B) -> check_part_sha(B) end,
        fun(B) -> check_part_size(B) end,
        fun(B) -> check_part_codec(B) end
    ],
    case run_checks(Checks, Body, #{}) of
        {ok, Acc} -> {ok, fill_part_defaults(Body, Acc)};
        {error, Reason} -> {error, Reason}
    end;
parse_audio_part(_) ->
    {error, invalid_body}.

-spec fill_part_defaults(map(), map()) -> map().
fill_part_defaults(Body, Acc) ->
    Acc1 =
        case maps:get(capture_offset_ms, Acc, undefined) of
            undefined -> Acc#{capture_offset_ms => maps:get(start_ms, Acc)};
            _ -> Acc
        end,
    Acc2 =
        case maps:find(<<"sample_rate">>, Body) of
            {ok, SR} when is_integer(SR), SR > 0 -> Acc1#{sample_rate => SR};
            _ -> Acc1
        end,
    case maps:find(<<"channels">>, Body) of
        {ok, Ch} when is_integer(Ch), Ch > 0 -> Acc2#{channels => Ch};
        _ -> Acc2
    end.

%% @doc confirm 回执：可选 sha256/byte_size（客户端上传完成回执，须与注册
%% 一致；缺省信任注册值）。
-spec parse_confirm_receipt(map()) -> {ok, map()} | {error, atom()}.
parse_confirm_receipt(Body) when is_map(Body) ->
    ShaOk =
        case maps:find(<<"sha256">>, Body) of
            {ok, Sha} -> is_sha256_hex(Sha);
            error -> true
        end,
    SizeOk =
        case maps:find(<<"byte_size">>, Body) of
            {ok, S} -> is_integer(S) andalso S > 0;
            error -> true
        end,
    case {ShaOk, SizeOk} of
        {true, true} ->
            Picked = maps:with([<<"sha256">>, <<"byte_size">>], Body),
            {ok, maps:fold(fun(K, V, Acc) -> Acc#{binary_to_atom(K, utf8) => V} end, #{}, Picked)};
        _ ->
            {error, invalid_confirm_receipt}
    end;
parse_confirm_receipt(_) ->
    {error, invalid_body}.

%% @doc recover 请求体：device_session_id UUID 必填（原会话恢复判据）。
-spec parse_device_session(map()) -> {ok, map()} | {error, atom()}.
parse_device_session(Body) when is_map(Body) ->
    check_device_session(Body);
parse_device_session(_) ->
    {error, invalid_body}.

%% @doc abandon 请求体：confirm=true 必填（明确确认，§11.1）。
-spec parse_abandon(map()) -> {ok, map()} | {error, atom()}.
parse_abandon(Body) when is_map(Body) ->
    case maps:get(<<"confirm">>, Body, false) of
        true -> {ok, #{}};
        _ -> {error, abandon_confirm_required}
    end;
parse_abandon(_) ->
    {error, invalid_body}.

%% @doc 事件枚举（测试与 DS 折叠共用）。
-spec valid_events() -> [binary()].
valid_events() ->
    ?CAPTURE_EVENTS.

%% -------------------------------------------------------------------
%% T07 内部校验
%% -------------------------------------------------------------------

%% @private T07 整数解析（T05 的 nonneg_int 返回 boolean，此处采集域需要值）。
-spec int_nonneg(term()) -> {ok, integer()} | error.
int_nonneg(N) when is_integer(N), N >= 0 -> {ok, N};
int_nonneg(N) when is_binary(N) ->
    try
        int_nonneg(binary_to_integer(N))
    catch
        _:_ -> error
    end;
int_nonneg(_) ->
    error.

-spec check_device_session(map()) -> {ok, map()} | {error, invalid_device_session_id}.
check_device_session(Body) ->
    case maps:get(<<"device_session_id">>, Body, <<>>) of
        Bin when is_binary(Bin) ->
            case is_uuid(Bin) of
                true -> {ok, #{device_session_id => Bin}};
                false -> {error, invalid_device_session_id}
            end;
        _ ->
            {error, invalid_device_session_id}
    end.

-spec check_device_id(map()) -> {ok, map()} | {error, invalid_device_id}.
check_device_id(Body) ->
    case maps:get(<<"device_id">>, Body, <<>>) of
        Bin when is_binary(Bin), byte_size(Bin) >= 1, byte_size(Bin) =< ?MAX_DEVICE_ID_BYTES ->
            case is_printable_ascii(Bin) of
                true -> {ok, #{device_id => Bin}};
                false -> {error, invalid_device_id}
            end;
        _ ->
            {error, invalid_device_id}
    end.

-spec check_reported_started_at(map()) -> {ok, map()} | {error, invalid_reported_at}.
check_reported_started_at(Body) ->
    case maps:find(<<"reported_at">>, Body) of
        error ->
            {ok, #{}};
        {ok, null} ->
            {ok, #{}};
        {ok, V} when is_binary(V), byte_size(V) =< 64 ->
            {ok, #{reported_at => V}};
        {ok, _} ->
            {error, invalid_reported_at}
    end.

-spec check_client_event_id(map()) -> {ok, map()} | {error, invalid_client_event_id}.
check_client_event_id(Body) ->
    case maps:get(<<"client_event_id">>, Body, <<>>) of
        Bin when is_binary(Bin) ->
            case is_uuid(Bin) of
                true -> {ok, #{client_event_id => Bin}};
                false -> {error, invalid_client_event_id}
            end;
        _ ->
            {error, invalid_client_event_id}
    end.

-spec check_event_seq(map()) -> {ok, map()} | {error, invalid_event_seq}.
check_event_seq(Body) ->
    case int_nonneg(maps:get(<<"seq">>, Body, -1)) of
        {ok, Seq} -> {ok, #{seq => Seq}};
        _ -> {error, invalid_event_seq}
    end.

-spec check_event_kind(map()) -> {ok, map()} | {error, invalid_event}.
check_event_kind(Body) ->
    case maps:get(<<"event">>, Body, <<>>) of
        E when is_binary(E) ->
            case lists:member(E, ?CAPTURE_EVENTS) of
                true -> {ok, #{event => E}};
                false -> {error, invalid_event}
            end;
        _ ->
            {error, invalid_event}
    end.

-spec check_event_position(map()) -> {ok, map()} | {error, invalid_position_ms}.
check_event_position(Body) ->
    case int_nonneg(maps:get(<<"position_ms">>, Body, -1)) of
        {ok, Pos} -> {ok, #{position_ms => Pos}};
        _ -> {error, invalid_position_ms}
    end.

-spec check_part_seq(map()) -> {ok, map()} | {error, invalid_part_seq}.
check_part_seq(Body) ->
    case int_nonneg(maps:get(<<"seq">>, Body, -1)) of
        {ok, Seq} -> {ok, #{seq => Seq}};
        _ -> {error, invalid_part_seq}
    end.

-spec check_part_time_axis(map()) -> {ok, map()} | {error, atom()}.
check_part_time_axis(Body) ->
    case
        {
            int_nonneg(maps:get(<<"start_ms">>, Body, -1)),
            int_nonneg(maps:get(<<"end_ms">>, Body, -1))
        }
    of
        {{ok, Start}, {ok, End}} when End > Start ->
            Acc0 = #{start_ms => Start, end_ms => End},
            case maps:find(<<"capture_offset_ms">>, Body) of
                {ok, Offset} ->
                    case int_nonneg(Offset) of
                        {ok, O} -> {ok, Acc0#{capture_offset_ms => O}};
                        _ -> {error, invalid_capture_offset}
                    end;
                error ->
                    {ok, Acc0}
            end;
        {{ok, _}, {ok, _}} ->
            %% 空段（end =< start）：暂停期间不得产生空音频段（§11.1）
            {error, invalid_part_time_axis};
        _ ->
            {error, invalid_part_time_axis}
    end.

-spec check_part_sha(map()) -> {ok, map()} | {error, invalid_sha256}.
check_part_sha(Body) ->
    case maps:get(<<"sha256">>, Body, <<>>) of
        Sha when is_binary(Sha) ->
            case is_sha256_hex(Sha) of
                true -> {ok, #{sha256 => string:lowercase(Sha)}};
                false -> {error, invalid_sha256}
            end;
        _ ->
            {error, invalid_sha256}
    end.

-spec is_sha256_hex(binary()) -> boolean().
is_sha256_hex(Sha) ->
    Re = <<"^[0-9a-fA-F]{64}$">>,
    try
        case re:run(Sha, Re) of
            {match, _} -> true;
            nomatch -> false
        end
    catch
        _:_ -> false
    end.

-spec check_part_size(map()) -> {ok, map()} | {error, invalid_byte_size}.
check_part_size(Body) ->
    case maps:get(<<"byte_size">>, Body, maps:get(<<"size">>, Body, 0)) of
        S when is_integer(S), S > 0 -> {ok, #{byte_size => S}};
        _ -> {error, invalid_byte_size}
    end.

-spec check_part_codec(map()) -> {ok, map()} | {error, invalid_codec}.
check_part_codec(Body) ->
    case valid_part_codec(maps:get(<<"codec">>, Body, undefined)) of
        true -> {ok, #{codec => <<"aac">>}};
        false -> {error, invalid_codec}
    end.

%% V1 accepts ADTS AAC only; unknown historical facts are never inferred.
-spec valid_part_codec(term()) -> boolean().
valid_part_codec(<<"aac">>) -> true;
valid_part_codec(_) -> false.
