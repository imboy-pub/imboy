-module(report_handler).

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create, Req, State) ->
    create(Req, State, auto);
handle_action(group_create, Req, State) ->
    create(Req, State, {<<"group">>, group_id, [<<"target_id">>, <<"group_id">>, <<"gid">>]});
handle_action(channel_create, Req, State) ->
    create(Req, State, {<<"channel">>, channel_id, [<<"target_id">>, <<"channel_id">>]});
handle_action(user_create, Req, State) ->
    create(Req, State, {<<"user">>, user_id, [<<"target_id">>, <<"user_id">>, <<"uid">>]});
handle_action(moment_create, Req, State) ->
    create(Req, State, {<<"moment">>, moment_id, [<<"target_id">>, <<"moment_id">>]});
handle_action(false, Req, _State) ->
    Req.

-spec create(cowboy_req:req(), map(), auto | {binary(), atom(), [binary()]}) -> cowboy_req:req().
create(Req0, State, Mode) ->
    Uid = maps:get(current_uid, State, 0),
    PostVals = elib_param:post(Req0),
    {TargetType, TargetId} = resolve_target(Req0, PostVals, Mode),
    Reason = maps:get(<<"reason">>, PostVals, <<>>),
    Desc = maps:get(<<"description">>, PostVals, <<>>),
    case Uid > 0 of
        false ->
            elib_response:error(Req0, <<"请先登录"/utf8>>);
        true ->
            case ensure_target_feature(Req0, TargetType) of
                ok ->
                    case report_logic:create(Uid, TargetType, TargetId, Reason, Desc) of
                        {ok, Payload} ->
                            elib_response:success(Req0, Payload);
                        {error, Msg} ->
                            elib_response:error(Req0, Msg)
                    end;
                {error, RespReq} ->
                    RespReq
            end
    end.

-spec resolve_target(cowboy_req:req(), map(), auto | {binary(), atom(), [binary()]}) -> {binary(), binary()}.
resolve_target(_Req0, PostVals, auto) ->
    TargetType = target_type_from_body(PostVals),
    TargetId = pick_first_nonempty([
        maps:get(<<"target_id">>, PostVals, <<>>),
        maps:get(<<"moment_id">>, PostVals, <<>>),
        maps:get(<<"group_id">>, PostVals, <<>>),
        maps:get(<<"channel_id">>, PostVals, <<>>),
        maps:get(<<"user_id">>, PostVals, <<>>),
        maps:get(<<"uid">>, PostVals, <<>>),
        maps:get(<<"gid">>, PostVals, <<>>)
    ]),
    {TargetType, TargetId};
resolve_target(Req0, PostVals, {TargetType, BindingKey, FallbackKeys}) ->
    BoundId =
        case cowboy_req:binding(BindingKey, Req0) of
            undefined ->
                <<>>;
            Value ->
                normalize_binary(Value)
        end,
    BodyTargetId = pick_first_nonempty([maps:get(Key, PostVals, <<>>) || Key <- FallbackKeys]),
    TargetId =
        case BoundId of
            <<>> -> BodyTargetId;
            _ -> BoundId
        end,
    {TargetType, TargetId}.

-spec ensure_target_feature(cowboy_req:req(), binary()) -> ok | {error, cowboy_req:req()}.
ensure_target_feature(Req0, <<"moment">>) ->
    imboy_feature:ensure_enabled(Req0, moment);
ensure_target_feature(_Req0, _TargetType) ->
    ok.

-spec target_type_from_body(map()) -> binary().
target_type_from_body(PostVals) ->
    normalize_target_type(maps:get(<<"target_type">>, PostVals, <<>>)).

-spec normalize_target_type(term()) -> binary().
normalize_target_type(Value) when is_binary(Value) ->
    normalize_target_type_binary(Value);
normalize_target_type(Value) when is_list(Value) ->
    normalize_target_type_binary(unicode:characters_to_binary(Value));
normalize_target_type(_) ->
    <<>>.

-spec normalize_target_type_binary(binary()) -> binary().
normalize_target_type_binary(Value) ->
    Lower = string:lowercase(binary_to_list(Value)),
    case Lower of
        "moment" -> <<"moment">>;
        "moments" -> <<"moment">>;
        "group" -> <<"group">>;
        "groups" -> <<"group">>;
        "channel" -> <<"channel">>;
        "channels" -> <<"channel">>;
        "user" -> <<"user">>;
        "users" -> <<"user">>;
        _ -> <<>>
    end.

-spec pick_first_nonempty([term()]) -> binary().
pick_first_nonempty([]) ->
    <<>>;
pick_first_nonempty([Value | Rest]) ->
    Bin = normalize_binary(Value),
    case Bin of
        <<>> -> pick_first_nonempty(Rest);
        _ -> Bin
    end.

-spec normalize_binary(term()) -> binary().
normalize_binary(Value) when is_binary(Value) ->
    unicode:characters_to_binary(string:trim(ec_cnv:to_list(Value)));
normalize_binary(Value) when is_list(Value) ->
    unicode:characters_to_binary(string:trim(Value));
normalize_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_binary(_) ->
    <<>>.
