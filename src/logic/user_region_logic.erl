-module(user_region_logic).
%%%
%%% user_region_logic —— 用户地区选择簇（自 user_logic 拆出，质量门 ≤800 行）
%%%
%%% geo_regions_ds 的 Logic 门面：authoritative 校验/展示名拼装/profile 投影。
%%% 调用方：user_logic（update_region_selection 权威路径）、user_handler 与
%%% adm_user_handler（profile 投影 / web 展示名）。行为逐字未动。
%%%

-export([authoritative_region/1, web_region_display/1]).
-export([profile_region_selection/2]).

authoritative_region(Selection) ->
    case geo_regions_ds:source_names(Selection, region_release_pin()) of
        {ok, Names} ->
            Trimmed = [string:trim(Name) || Name <- Names],
            Region = iolist_to_binary(lists:join(<<" ">>, Trimmed)),
            case lists:member(<<>>, Trimmed) orelse binary:match(Region, <<0>>) =/= nomatch of
                true -> {error, invalid_artifact};
                false -> check_region_length(Region)
            end;
        {error, _} = Error ->
            Error
    end.

check_region_length(Region) ->
    case length(unicode:characters_to_list(Region)) =< 80 of
        true -> {ok, Region};
        false -> {error, display_too_long}
    end.

%% @doc user_handler web 面的地区展示名拼装（web_region_value 专用口径：
%% 不做 80 长度校验、不走 authoritative 更新流程——那是 update_region_
%% selection 侧 authoritative_region/1 的职责）。Handler→Logic→DS 单向：
%% handler 不得直接触 geo_regions_ds（模块边界守护）。
-spec web_region_display(map()) -> {ok, binary()} | unavailable | error.
web_region_display(Selection) ->
    case geo_regions_ds:source_names(Selection, region_release_pin()) of
        {ok, Names} ->
            Region = iolist_to_binary(lists:join(<<" ">>, [string:trim(Name) || Name <- Names])),
            {ok, Region};
        {error, Error} when Error =:= unavailable; Error =:= invalid_artifact ->
            unavailable;
        {error, _} ->
            error
    end.

profile_region_selection(Region, EncodedSelection) ->
    Selection = decode_region_selection(EncodedSelection),
    Check =
        case Selection of
            null -> absent;
            _ -> validate_region_selection(Selection)
        end,
    #{
        <<"regionSelection">> => Selection,
        <<"regionSelectionStatus">> => user_agg:region_selection_status(Region, Check)
    }.

decode_region_selection(Selection) when is_map(Selection); Selection =:= null ->
    Selection;
decode_region_selection(Selection) when is_binary(Selection) ->
    jsone:decode(Selection).

validate_region_selection(Selection) ->
    geo_regions_ds:validate(Selection, region_release_pin()).

region_release_pin() ->
    config_ds:env(geo_regions_pin, #{}).
