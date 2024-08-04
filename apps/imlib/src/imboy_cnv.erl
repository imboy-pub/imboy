-module(imboy_cnv).

%%%
% cnv 是 convert 的缩写，概念上基础 ec_cnv 模块
% 如果 /deps/erlware_commons/src/ec_cnv.erl 模块有的方法就，使用它
%
% imboy_cnv:json_maybe(12345).
% imboy_cnv:json_maybe(<<"[1,2,3]">>).
%%%

-export([json_maybe/1]).

-export([zipwith_equery/1]).
-export([implode/2]).
-export([remove_dups/1]).
-export([vsn_major/1]).
-export([map_to_query/1]).
-export([list_to_binary_string/1]).


% imboy_cnv:map_to_query(#{d=>4, a => 1, b => 2, c => 3}).
map_to_query(Map) ->
    Pairs = [[ec_cnv:to_list(Key), "=", ec_cnv:to_list(Value)] || {Key, Value} <- maps:to_list(Map)],
    list_to_binary(string:join(Pairs, "&")).


% imboy_cnv:list_to_binary_string([513251,62829,62825]).
list_to_binary_string(IntList) ->
    % 将整数列表转换为字符串列表
    StringList = lists:map(fun(I) -> integer_to_list(I) end, IntList),
    % 使用逗号连接字符串列表
    JoinedString = lists:join(",", StringList),
    % 将字符串转换为二进制
    list_to_binary(JoinedString).

zipwith_equery(Res) ->
    case Res of
        {ok, _, []} ->
            [];
        {ok, ColumnLi, Items0} ->
            Items1 = [tuple_to_list(Item) || Item <- Items0],
            [lists:zipwith(fun(X, Y) -> {X, Y} end, ColumnLi, Row) || Row <- Items1];
        _ ->
            []
    end.

% 如果是json类型的字符串，就decode，否则保持原数据类型
json_maybe(Val) ->
    case jsx:is_json(Val) of
        true ->
            jsx:decode(Val);
        false ->
            Val
    end.


% 用字符串连接数组元素，类似 php 的 implode/2 方法
% imboy_cnv:implode(",", [<<"a">>, "b"]).
% imboy_cnv:implode("','", [<<"a">>, "b"]).
% imboy_cnv:implode(",", [1,2,3.3]).   // <<"1,2,3.3">>
-spec implode([binary() | list() | float() | integer()], list()) -> binary().
implode(S, Li) when is_float(S) ->
    implode(io_lib:format("~p", [S]), Li);
implode(S, Li) when is_integer(S) ->
    implode(integer_to_binary(S), Li);
implode(Separator, Li) ->
    Li2 = [ [Separator, ec_cnv:to_binary(I)] || I <- Li ],
    iolist_to_binary(string:replace(iolist_to_binary(Li2), Separator, "")).


% 从 list 中移除重复的元素
remove_dups([]) ->
    [];
remove_dups([H | T]) ->
    [H | [ X || X <- remove_dups(T), X /= H ]].


vsn_major(Vsn) ->
    Major2 = case ec_semver:parse(Vsn) of
        {{Major, _, _, _}, _} ->
            Major;
        {{Major, _, _}, _} ->
            Major;
        {{Major, _}, _} ->
            Major;
        {Major, _} when is_integer(Major) ->
            Major;
        {Major, _} ->
            Major
    end,
    ec_cnv:to_binary(Major2).
