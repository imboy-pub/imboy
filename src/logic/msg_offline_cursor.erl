-module(msg_offline_cursor).
-export([encode/1, decode/1]).

%% Preserve the database timestamp precision; the legacy millisecond cursor
%% cannot distinguish messages sharing the same timestamp.
encode(undefined) ->
    <<>>;
%% R01：v2 版本化 cursor——per-group 服务端投递进度（group_id, conv_seq），
%% 基于权威接受顺序（D05/R02 的 B 边界钳制），与客户端 created_at 无关。
%% 具体子句先于兜底 {At, Id}（后者匹配任意二元组）。
encode({marks, Marks}) when is_list(Marks) ->
    base64:encode(
        jsone:encode(#{
            <<"v">> => 2,
            <<"marks">> => [#{<<"g">> => G, <<"s">> => S} || {G, S} <- Marks]
        })
    );
encode({At, Id}) ->
    base64:encode(jsone:encode(#{<<"v">> => 1, <<"at">> => At, <<"id">> => Id})).

decode(<<>>) ->
    {ok, undefined};
decode(Token) when is_binary(Token), byte_size(Token) =< 512 ->
    try
        decode_data(jsone:decode(base64:decode(Token)))
    catch
        _:_ -> {error, invalid_offline_cursor}
    end;
decode(_) ->
    {error, invalid_offline_cursor}.

decode_data(#{<<"v">> := 1, <<"at">> := At, <<"id">> := Id} = Data) when
    map_size(Data) =:= 3,
    is_binary(At),
    byte_size(At) =< 64,
    is_binary(Id),
    byte_size(Id) > 0,
    byte_size(Id) =< 40
->
    _ = calendar:rfc3339_to_system_time(binary_to_list(At), [{unit, microsecond}]),
    {ok, {At, Id}};
%% R01：旧 token 继续可解码（滚动部署兼容）；新格式仅多出 v2 分支。
decode_data(#{<<"v">> := 2, <<"marks">> := [_ | _] = Marks} = Data) when
    map_size(Data) =:= 2,
    is_list(Marks),
    length(Marks) =< 50
->
    decode_marks(Marks, []);
decode_data(_) ->
    {error, invalid_offline_cursor}.

decode_marks([], Acc) ->
    {ok, {marks, lists:reverse(Acc)}};
decode_marks([#{<<"g">> := G, <<"s">> := S} | Rest], Acc) when
    is_integer(G), G > 0, is_integer(S), S >= 0
->
    decode_marks(Rest, [{G, S} | Acc]);
decode_marks(_, _) ->
    {error, invalid_offline_cursor}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

marks_count_boundary_test() ->
    Mark = #{<<"g">> => 1, <<"s">> => 0},
    Data = #{<<"v">> => 2, <<"marks">> => lists:duplicate(50, Mark)},
    ?assertEqual({ok, {marks, lists:duplicate(50, {1, 0})}}, decode_data(Data)),
    ?assertEqual(
        {error, invalid_offline_cursor},
        decode_data(Data#{<<"marks">> => lists:duplicate(51, Mark)})
    ).
-endif.
