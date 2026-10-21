-module(talk_record_processor_result).
-moduledoc "模型输出只作数据：匿名分人、时轴、版本与同源引用严格校验。".
-export([validate/4]).

validate(Kind, Result, Config, Context) ->
    try
        true = maps:get(<<"model_version">>, Result) =:= maps:get(model_version, Config),
        case Kind of
            <<"transcribe">> -> transcript(Result, Context);
            <<"summarize">> -> summary(Result, Context)
        end
    catch
        _:_ -> {error, processor_schema_invalid}
    end.

transcript(R, #{duration_ms := Duration}) ->
    true = allowed(R, [
        <<"model_version">>, <<"status">>, <<"speakers">>, <<"segments">>, <<"language">>
    ]),
    Speakers = maps:get(<<"speakers">>, R),
    Segments = maps:get(<<"segments">>, R),
    true = is_list(Speakers) andalso length(Speakers) =< 200,
    Labels = [speaker(S) || S <- Speakers],
    true = length(Labels) =:= length(lists:usort(Labels)),
    true = is_list(Segments) andalso length(Segments) =< 20000,
    _ = lists:foldl(fun(S, Previous) -> segment(S, Labels, Duration, Previous) end, 0, Segments),
    Status = maps:get(<<"status">>, R),
    true =
        (Status =:= <<"no_speech">> andalso Segments =:= []) orelse
            (Status =:= <<"ready">> andalso Segments =/= []),
    {ok, #{
        status => Status,
        speakers => Speakers,
        segments => Segments,
        model_version => maps:get(<<"model_version">>, R)
    }}.

speaker(S) ->
    true = allowed(S, [<<"label">>, <<"confidence">>]),
    Label = maps:get(<<"label">>, S),
    true = text(Label, 48),
    confidence(S),
    Label.
segment(S, Labels, Duration, Previous) ->
    true = allowed(S, [
        <<"start_ms">>, <<"end_ms">>, <<"speaker_label">>, <<"text">>, <<"confidence">>
    ]),
    Start = maps:get(<<"start_ms">>, S),
    End = maps:get(<<"end_ms">>, S),
    true = is_integer(Start) andalso is_integer(End),
    true = Start >= Previous andalso End >= Start andalso End =< Duration,
    true = lists:member(maps:get(<<"speaker_label">>, S), Labels),
    Text = maps:get(<<"text">>, S),
    true =
        text(Text, 20000) orelse
            (Text =:= <<>> andalso maps:get(<<"speaker_label">>, S) =:= <<"unknown">>),
    confidence(S),
    Start.
confidence(S) ->
    C = maps:get(<<"confidence">>, S, null),
    true = C =:= null orelse (is_number(C) andalso C >= 0 andalso C =< 1).

summary(R, #{segment_ids := Ids}) ->
    true = allowed(R, [<<"model_version">>, <<"overview">>, <<"decisions">>, <<"actions">>]),
    Overview = maps:get(<<"overview">>, R),
    true = text(Overview, 20000),
    Decisions = maps:get(<<"decisions">>, R),
    Actions = maps:get(<<"actions">>, R),
    true = items(Decisions, Ids),
    true = items(Actions, Ids),
    {ok, #{
        overview => Overview,
        decisions => Decisions,
        actions => Actions,
        model_version => maps:get(<<"model_version">>, R)
    }}.
items(Items, Ids) when is_list(Items), length(Items) =< 200 ->
    lists:all(
        fun(I) ->
            Refs = maps:get(<<"segment_ids">>, I, []),
            allowed(I, [<<"text">>, <<"segment_ids">>, <<"owner_user_id">>, <<"due_date">>]) andalso
                text(maps:get(<<"text">>, I, <<>>), 4000) andalso is_list(Refs) andalso Refs =/= [] andalso
                length(Refs) =< 100 andalso lists:all(fun(Id) -> lists:member(Id, Ids) end, Refs) andalso
                maps:get(<<"owner_user_id">>, I, null) =:= null andalso
                maps:get(<<"due_date">>, I, null) =:= null
        end,
        Items
    );
items(_, _) ->
    false.
allowed(M, Keys) -> is_map(M) andalso maps:keys(maps:without(Keys, M)) =:= [].
text(B, Max) -> is_binary(B) andalso byte_size(B) > 0 andalso byte_size(B) =< Max.
