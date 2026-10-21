-module(talk_record_participant_logic).
-moduledoc "参与者事实清单（不等于授权、通知或说话人认证）；全量PUT走记录revision。".
-export([list/2, replace/4, parse/1]).

-spec list(integer(), integer()) -> {ok, map()} | {error, term()}.
list(Uid, Rid) ->
    run(fun(C) ->
        Row = require(talk_record_participant_repo:record_tx(C, Uid, Rid, read)),
        payload(C, Rid, maps:get(<<"revision">>, Row))
    end).

-spec replace(integer(), integer(), binary() | undefined, term()) -> {ok, map()} | {error, term()}.
replace(Uid, Rid, IfMatch, Body) ->
    case {talk_record_logic:parse_if_match(IfMatch), parse(Body)} of
        {{ok, Rev}, {ok, Entries}} -> run(fun(C) -> replace_tx(C, Uid, Rid, Rev, Entries) end);
        {{error, Reason}, _} -> {error, Reason};
        {_, {error, Reason}} -> {error, Reason}
    end.

replace_tx(C, Uid, Rid, Rev, Entries) ->
    Row = require(talk_record_participant_repo:record_tx(C, Uid, Rid, write)),
    case require(talk_record_participant_repo:editable_tx(C, Uid, Row)) of
        true -> ok;
        false -> rollback(forbidden)
    end,
    case maps:get(<<"revision">>, Row) =:= Rev of
        true -> ok;
        false -> rollback(precondition_failed)
    end,
    Resolved = [resolve(C, Uid, Row, E) || E <- Entries],
    Next = require(talk_record_participant_repo:replace_tx(C, Rid, Resolved)),
    _ = require(
        talk_record_repo:insert_audit_tx(
            C,
            Rid,
            Uid,
            <<"participants_replace">>,
            null,
            #{<<"entries">> => length(Entries), <<"revision">> => Next}
        )
    ),
    payload(C, Rid, Next).

resolve(_C, _Actor, _Row, #{user_id := null} = E) ->
    E;
resolve(C, Actor, Row, #{user_id := Uid}) ->
    case require(talk_record_speaker_repo:target_in_directory_tx(C, Row, Actor, Uid)) of
        true -> ok;
        false -> rollback(target_not_in_directory)
    end,
    Name = require(talk_record_participant_repo:display_name_tx(C, Uid)),
    #{user_id => Uid, display_name => Name, attendance_source => <<"selected">>}.

payload(C, Rid, Rev) ->
    Rows = require(talk_record_participant_repo:list_tx(C, Rid)),
    {ok, #{
        <<"record_id">> => Rid,
        <<"list">> => Rows,
        <<"revision">> => Rev,
        <<"etag">> => integer_to_binary(Rev)
    }}.

%% Every replacement is explicit; omission never means clear all.
-spec parse(term()) -> {ok, [map()]} | {error, invalid_participants}.
parse(#{<<"participants">> := Entries} = Body) when
    is_list(Entries), length(Entries) =< 100, map_size(Body) =:= 1
->
    try
        Parsed = [entry(E) || E <- Entries],
        Keys = [
            case maps:get(user_id, E) of
                null -> {guest, maps:get(display_name, E)};
                U -> {user, U}
            end
         || E <- Parsed
        ],
        case length(Keys) =:= length(lists:usort(Keys)) of
            true -> {ok, Parsed};
            false -> {error, invalid_participants}
        end
    catch
        _:_ -> {error, invalid_participants}
    end;
parse(_) ->
    {error, invalid_participants}.

entry(E) when is_map(E) ->
    case maps:keys(E) -- [<<"user_id">>, <<"display_name">>] of
        [] -> ok;
        _ -> error(invalid)
    end,
    case maps:get(<<"user_id">>, E, null) of
        null ->
            #{
                user_id => null,
                display_name => guest_name(maps:get(<<"display_name">>, E, <<>>)),
                attendance_source => <<"manual">>
            };
        U when is_integer(U), U > 0, U =< 9223372036854775807 -> #{user_id => U};
        _ ->
            error(invalid)
    end;
entry(_) ->
    error(invalid).

guest_name(B) when is_binary(B) ->
    Chars = string:trim(unicode:characters_to_list(B)),
    true = length(Chars) > 0 andalso length(Chars) =< 120,
    false = lists:any(fun(C) -> C < 32 orelse C =:= 127 end, Chars),
    unicode:characters_to_binary(Chars).

run(F) ->
    case elib_pg:with_tx(F, [{reraise, false}]) of
        {rollback, {talk_record_rollback, {error, R}}} -> {error, R};
        {rollback, _} -> {error, db_error};
        Result -> Result
    end.
require({ok, V}) -> V;
require({error, R}) -> rollback(R).
rollback(R) -> throw({talk_record_rollback, {error, R}}).
