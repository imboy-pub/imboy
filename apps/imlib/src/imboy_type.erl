-module(imboy_type).

-export([is_numeric/1]).

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


% imboy_type:is_numeric(Key)
is_numeric(Value) when is_number(Value) ->
    true;
is_numeric(Value) when is_binary(Value) ->
    is_numeric(binary_to_list(Value));
is_numeric(Value) when is_list(Value) ->
    Regex = persistent_term:get({?MODULE, numeric_regex}),
    re:run(Value, Regex, [{capture, none}]) =:= match;
is_numeric(_) ->
    false.

%%%%% test

% -include_lib("eunit/include/eunit.hrl").

% % 测试原生数值类型
% is_numeric_native_test() ->
%     ?assertEqual(true, imboy_type:is_numeric(123)),
%     ?assertEqual(true, imboy_type:is_numeric(123.45)),
%     ?assertEqual(true, imboy_type:is_numeric(-123.45)).

% % 测试合法字符串
% is_numeric_string_test() ->
%     ?assertEqual(true, imboy_type:is_numeric("123")),
%     ?assertEqual(true, imboy_type:is_numeric("-12.3e4")),
%     ?assertEqual(true, imboy_type:is_numeric("123e2")),
%     ?assertEqual(true, imboy_type:is_numeric(".45")),
%     ?assertEqual(true, imboy_type:is_numeric("+123.45")),
%     ?assertEqual(true, imboy_type:is_numeric("-.45")),
%     ?assertEqual(true, imboy_type:is_numeric("123.45e-6")).

% % 测试非法字符串
% is_numeric_invalid_string_test() ->
%     ?assertEqual(false, imboy_type:is_numeric("12a3")),
%     ?assertEqual(false, imboy_type:is_numeric("12.3.4")),
%     ?assertEqual(false, imboy_type:is_numeric("123e")),
%     ?assertEqual(false, imboy_type:is_numeric("e123")),
%     ?assertEqual(false, imboy_type:is_numeric("123e4.5")),
%     ?assertEqual(false, imboy_type:is_numeric("abc")),
%     ?assertEqual(false, imboy_type:is_numeric("123e+")),
%     ?assertEqual(false, imboy_type:is_numeric("123e-")),
%     ?assertEqual(false, imboy_type:is_numeric("+")),
%     ?assertEqual(false, imboy_type:is_numeric("-")),
%     ?assertEqual(false, imboy_type:is_numeric("")).

% % 测试其他类型
% is_numeric_other_types_test() ->
%     ?assertEqual(false, imboy_type:is_numeric(atom)),
%     ?assertEqual(false, imboy_type:is_numeric(<<123>>)),
%     ?assertEqual(false, imboy_type:is_numeric({1, 2, 3})),
%     ?assertEqual(false, imboy_type:is_numeric([1, 2, 3])),
%     ?assertEqual(false, imboy_type:is_numeric(fun() -> ok end)),
%     ?assertEqual(false, imboy_type:is_numeric(self())).
