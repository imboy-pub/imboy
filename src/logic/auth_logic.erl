-module(auth_logic).
%% Stable identity auth domain boundary.
%% API adapters should call this module instead of reaching auth data internals directly.

%%%
% auth 业务逻辑模块
% auth business logic module
%%%

-export([verify_for_open/3]).
-export([verify_for_assets/4]).
-export([logout/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 验证资源访问权限
%% 验证附件资源的访问 Token 是否有效
%% @param Scene 资源场景类型
%% @param Tk 访问 Token
%% @param V 时间戳值
%% @param Path 资源路径
%% @return binary() <<"ok">> 表示成功，<<"fail">> 表示失败
-spec verify_for_assets(binary(), binary() | undefined, integer() | error, binary()) ->
                           binary().
verify_for_assets(undefined, _Tk, _Name, _) ->
    <<"fail">>;
verify_for_assets(_Scene, undefined, _Name, _) ->
    <<"fail">>;
verify_for_assets(_Scene, _Tk, error, _) ->
    <<"fail">>;
% verify_for_assets(Scene, Tk, Val, Path) when is_binary(Path) ->
verify_for_assets(Scene, Tk, V, _Path) ->
    Now = elib_dt:utc(second),
    % V = binary_to_integer(Val),
    Diff = 7200,
    % elib_log:info(io_lib:format("V:~p ~p ~n", [V, Now < (V + Diff) ])),
    if is_integer(V) andalso Now < V + Diff ->
           % V = binary_to_list(<<Path/binary, "?", Val/binary>>),
           % NewTk = auth_ds:get_token(assets, Scene, V),
           NewTk = auth_ds:get_token(assets, Scene, V),
           do_verify_for_assets(NewTk, Tk);
       true ->
           <<"fail">>
    end.

%% @doc 验证开放资源访问权限
%% 验证公开访问资源的 Token 是否有效
%% @param Path 资源路径
%% @param Tk 访问 Token
%% @param Val 时间戳值
%% @return binary() <<"ok">> 表示成功，<<"fail">> 表示失败
-spec verify_for_open(binary(), binary(), binary()) -> binary().
verify_for_open(undefined, _Tk, _Val) ->
    <<"fail">>;
verify_for_open(_, undefined, _Val) ->
    <<"fail">>;
verify_for_open(_Path, _, undefined) ->
    <<"fail">>;
verify_for_open(Path, Tk, Val) ->
    V = binary_to_list(<<Path/binary, "?", Val/binary>>),
    NewTk = auth_ds:get_token(assets, <<"open">>, V),
    % elib_log:info(io_lib:format("auth_logic:verify_for_open/3 new ~p, Tk:~p;~n", [NewTk, Tk])),
    do_verify_for_assets(NewTk, Tk).

%% @doc 兼容旧测试入口：按设备登出
-spec logout(integer(), binary()) -> {ok, binary()}.
logout(Uid, DID) when is_integer(Uid), is_binary(DID) ->
    Devices = imboy_syn:list_by_uid(Uid),
    lists:foreach(fun({Pid, {_DType, DeviceId}}) when DeviceId =:= DID ->
        imboy_syn:leave(Uid, Pid);
       (_) ->
        ok
    end, Devices),
    ok = user_device_ds:delete(Uid, DID),
    {ok, <<"success">>}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================-

%%% 执行验证码
%% @doc 验证资源访问 Token
-spec do_verify_for_assets(binary(), binary()) -> binary().
do_verify_for_assets(NewTk, T) when NewTk == T ->
    <<"ok">>;
do_verify_for_assets(_, _) ->
    <<"fail">>.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
