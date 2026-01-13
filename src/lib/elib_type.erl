-module(elib_type).

-export([is_numeric/1]).
-export([is_proplist/1]).
-export([is_mobile/1]).
-export([is_email/1]).

-on_load(init/0).

init() ->
    % 预编译正则表达式以提高效率
    RegexPattern = "^[+-]?((\\d+(\\.\\d*)?|\\.\\d+)([eE][+-]?\\d+)?)$",
    case re:compile(RegexPattern) of
        {ok, CompiledRegex} ->
            % 存储编译后的正则表达式到persistent_term
            persistent_term:put({?MODULE, numeric_regex}, CompiledRegex),
            ok;
        {error, Reason} ->
            error({regex_compile_failed, Reason})
    end.


% elib_type:is_numeric(Key)
-spec is_numeric(any()) -> boolean().
is_numeric(Value) when is_number(Value) ->
    true;
is_numeric(Value) when is_binary(Value) ->
    is_numeric(binary_to_list(Value));
is_numeric(Value) when is_list(Value) ->
    Regex = persistent_term:get({?MODULE, numeric_regex}),
    re:run(Value, Regex, [{capture, none}]) =:= match;
is_numeric(_) ->
    false.


%% @doc 检查变量是否为属性列表格式
%% @param Var 要检查的变量
%% @returns true 如果是属性列表，否则返回 false
-spec is_proplist(any()) -> boolean().
is_proplist(Var) ->
    is_list(Var) andalso lists:all(fun({_, _}) -> true; (_) -> false end, Var).

%%%%% test

% -include_lib("eunit/include/eunit.hrl").

% % 测试原生数值类型
% is_numeric_native_test() ->
%     ?assertEqual(true, elib_type:is_numeric(123)),
%     ?assertEqual(true, elib_type:is_numeric(123.45)),
%     ?assertEqual(true, elib_type:is_numeric(-123.45)).

% % 测试合法字符串
% is_numeric_string_test() ->
%     ?assertEqual(true, elib_type:is_numeric("123")),
%     ?assertEqual(true, elib_type:is_numeric("-12.3e4")),
%     ?assertEqual(true, elib_type:is_numeric("123e2")),
%     ?assertEqual(true, elib_type:is_numeric(".45")),
%     ?assertEqual(true, elib_type:is_numeric("+123.45")),
%     ?assertEqual(true, elib_type:is_numeric("-.45")),
%     ?assertEqual(true, elib_type:is_numeric("123.45e-6")).

% % 测试非法字符串
% is_numeric_invalid_string_test() ->
%     ?assertEqual(false, elib_type:is_numeric("12a3")),
%     ?assertEqual(false, elib_type:is_numeric("12.3.4")),
%     ?assertEqual(false, elib_type:is_numeric("123e")),
%     ?assertEqual(false, elib_type:is_numeric("e123")),
%     ?assertEqual(false, elib_type:is_numeric("123e4.5")),
%     ?assertEqual(false, elib_type:is_numeric("abc")),
%     ?assertEqual(false, elib_type:is_numeric("123e+")),
%     ?assertEqual(false, elib_type:is_numeric("123e-")),
%     ?assertEqual(false, elib_type:is_numeric("+")),
%     ?assertEqual(false, elib_type:is_numeric("-")),
%     ?assertEqual(false, elib_type:is_numeric("")).

% % 测试其他类型
% is_numeric_other_types_test() ->
%     ?assertEqual(false, elib_type:is_numeric(atom)),
%     ?assertEqual(false, elib_type:is_numeric(<<123>>)),
%     ?assertEqual(false, elib_type:is_numeric({1, 2, 3})),
%     ?assertEqual(false, elib_type:is_numeric([1, 2, 3])),
%     ?assertEqual(false, elib_type:is_numeric(fun() -> ok end)),
%     ?assertEqual(false, elib_type:is_numeric(self())).




%% @doc 验证是否为中国大陆手机号
%% @param Mobile 手机号（binary 或 list）
%% @returns true | false
-spec is_mobile(binary() | list()) -> boolean().
is_mobile(Mobile) ->
    {_, P} = re:compile("^1[0-9]{10}$"),
    case re:run(Mobile, P) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.


%% @doc 验证是否为有效的电子邮件地址
%% @param Email 邮箱地址（binary 或 list）
%% @returns true | false
%% @example
%% elib_type:is_email(<<"test@example.com">>).
-spec is_email(binary() | list()) -> boolean().
is_email(undefined) ->
    false;
is_email(Email) ->
    {_, P} = re:compile("^[a-zA-Z0-9_-]+@[a-zA-Z0-9_-]+(\.[a-zA-Z0-9_-]+)+$"),
    case re:run(Email, P) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.
