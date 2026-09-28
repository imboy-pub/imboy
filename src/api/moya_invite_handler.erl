-module(moya_invite_handler).
%%%
% 墨芽班级邀请码 HTTP 适配层（W3：老师邀请码 → 家长加入班级）
% POST /api/v1/moya/classes/:id/invite-code   老师生成/复用班级邀请码（201 语义）
% GET  /api/v1/moya/invite/info?code=x        家长凭码读确认页（班名+学员名单）
% POST /api/v1/moya/invite/join               家长凭码选 learner 绑定监护关系
%
% 守卫在 logic 层（create：班 active class_staff 或 org owner，
% deny-by-default；info/join：码本身即凭证）。
%
% 错误映射（本 handler 内联）：
%   not_authorized       → HTTP 403 + 403（?ERR_FORBIDDEN，通用禁止码）
%   not_found            → HTTP 404 + 404（码不存在/撤销/过期统一折叠 +
%                          班不存在，通用码防探测）
%   learner_not_in_class → HTTP 422 + 5489（?ERR_INVITE_CODE_INVALID）
%   db_error             → HTTP 500 + 1（通用错误）
%
% 隐私（访问日志纪律）：info/join 记访问日志仅 uid + code 指纹
% （moya_invite_logic:code_fingerprint，sha256 前 12 位），不记学员名 /
% 不记完整 code——拿到日志也无法重放凭证或还原名单。
%%%

-behavior(cowboy_rest).

-export([init/2]).
-export([handle_action/3]).

-include("log.hrl").
-include("error_code.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(create_code, Req, State) -> create_code(Req, State);
handle_action(info, Req, State) -> info(Req, State);
handle_action(join, Req, State) -> join(Req, State);
handle_action(false, Req, _State) -> Req.

%%%===================================================================
%%% Actions
%%%===================================================================

%% POST /api/v1/moya/classes/:id/invite-code（body 空）
%% 成功 → success_rfc3339 {code, group_id(string)}（201 语义：已就绪资源）
-spec create_code(cowboy_req:req(), map()) -> cowboy_req:req().
create_code(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case path_group_id(Req0) of
        {ok, GroupId} ->
            case moya_invite_logic:create_or_get_code(Uid, GroupId) of
                {ok, Code} ->
                    elib_response:success_rfc3339(
                        Req0,
                        #{
                            <<"code">> => Code,
                            %% 契约硬规则1（STEP-04）：64-bit ID JSON 一律 string
                            <<"group_id">> => integer_to_binary(GroupId)
                        },
                        <<"邀请码已生成"/utf8>>
                    );
                {error, Reason} ->
                    error_response(Req0, Reason)
            end;
        error ->
            elib_response:error_with_status(
                Req0,
                422,
                <<"班级ID必填"/utf8>>,
                ?ERR_MISSING_PARAM
            )
    end.

%% GET /api/v1/moya/invite/info?code=x
%% 成功 → 200 {org_name, group_name, learners:[{id:string(TSID), display_name}]}
-spec info(cowboy_req:req(), map()) -> cowboy_req:req().
info(Req0, State) ->
    Uid = maps:get(current_uid, State),
    Qs = cowboy_req:parse_qs(Req0),
    %% 与 join 同款归一化（大写+trim）：家长手输小写/带空白也能命中
    case normalize_code(proplists:get_value(<<"code">>, Qs, undefined)) of
        {ok, Code} ->
            %% 访问日志：仅 uid + 指纹（名单对持有效码者可见是产品语义，
            %% 但"谁看过"必须可审计，且不落任何学员名）
            ?LOG_INFO(
                "[moya_invite] info_access uid=~p fp=~p",
                [Uid, moya_invite_logic:code_fingerprint(Code)]
            ),
            case moya_invite_logic:invite_info(Code) of
                {ok, #{org_name := OrgName, group_name := GroupName, learners := Learners}} ->
                    Payload = #{
                        <<"org_name">> => OrgName,
                        <<"group_name">> => GroupName,
                        %% TSID 契约：64-bit ID 一律 string（防 JS 精度丢失）
                        <<"learners">> => [learner_json(L) || L <- Learners]
                    },
                    elib_response:success_rfc3339(Req0, Payload);
                {error, Reason} ->
                    error_response(Req0, Reason)
            end;
        error ->
            elib_response:error_with_status(
                Req0,
                422,
                <<"缺少邀请码 code 参数"/utf8>>,
                ?ERR_MISSING_PARAM
            )
    end.

%% POST /api/v1/moya/invite/join  body {code, learner_id}
%% learner_id 兼容 integer 与数字字符串两种 JSON 形态（TSID 前端常以
%% string 传输）；成功 → {status: joined | already_joined}
-spec join(cowboy_req:req(), map()) -> cowboy_req:req().
join(Req0, State) ->
    Uid = maps:get(current_uid, State),
    case join_params(elib_param:post(Req0)) of
        {ok, Code, LearnerId} ->
            ?LOG_INFO(
                "[moya_invite] join_access uid=~p fp=~p",
                [Uid, moya_invite_logic:code_fingerprint(Code)]
            ),
            case moya_invite_logic:join(Uid, Code, LearnerId) of
                {ok, joined} ->
                    elib_response:success_rfc3339(
                        Req0, #{<<"status">> => <<"joined">>}, <<"加入成功"/utf8>>
                    );
                {ok, already_joined} ->
                    elib_response:success_rfc3339(
                        Req0,
                        #{<<"status">> => <<"already_joined">>},
                        <<"已是该学员监护人，无需重复加入"/utf8>>
                    );
                {error, Reason} ->
                    error_response(Req0, Reason)
            end;
        error ->
            elib_response:error_with_status(
                Req0,
                422,
                <<"缺少邀请码 code 或学员 learner_id"/utf8>>,
                ?ERR_MISSING_PARAM
            )
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec path_group_id(cowboy_req:req()) -> {ok, integer()} | error.
path_group_id(Req) ->
    case cowboy_req:binding(id, Req) of
        undefined ->
            error;
        Bin when is_binary(Bin) ->
            try binary_to_integer(Bin) of
                Int when Int > 0 -> {ok, Int};
                _ -> error
            catch
                _:_ -> error
            end
    end.

%% join body：code（非空 binary）+ learner_id（integer 或数字字符串）
%% spec 随 CI-00 成功类型收紧为 map()：唯一调用点 post body 经 jsone:decode
%% 恒为 map()，list 兜底子句已被 dialyzer 判死代码移除。
-spec join_params(map()) -> {ok, binary(), integer()} | error.
join_params(Body) when is_map(Body) ->
    Code = maps:get(<<"code">>, Body, undefined),
    LearnerId = maps:get(<<"learner_id">>, Body, undefined),
    case {normalize_code(Code), normalize_id(LearnerId)} of
        {{ok, C}, {ok, L}} -> {ok, C, L};
        _ -> error
    end.

-spec normalize_code(term()) -> {ok, binary()} | error.
normalize_code(Code) when is_binary(Code), Code =/= <<>> ->
    {ok, string:uppercase(string:trim(Code))};
normalize_code(_) ->
    error.

-spec normalize_id(term()) -> {ok, integer()} | error.
normalize_id(N) when is_integer(N), N > 0 ->
    {ok, N};
normalize_id(B) when is_binary(B) ->
    try binary_to_integer(B) of
        N when N > 0 -> {ok, N};
        _ -> error
    catch
        _:_ -> error
    end;
normalize_id(_) ->
    error.

%% 学员名单 JSON 投影：id → string（TSID 契约），display_name 原样
-spec learner_json(map()) -> map().
learner_json(#{id := Id, display_name := Name}) ->
    #{<<"id">> => integer_to_binary(Id), <<"display_name">> => Name};
learner_json(#{<<"id">> := Id, <<"display_name">> := Name}) when is_integer(Id) ->
    #{<<"id">> => integer_to_binary(Id), <<"display_name">> => Name};
learner_json(_) ->
    #{<<"id">> => <<>>, <<"display_name">> => <<>>}.

-spec error_response(cowboy_req:req(), atom()) -> cowboy_req:req().
error_response(Req, not_authorized) ->
    elib_response:error_with_status(
        Req,
        403,
        <<"无班级邀请码管理权限"/utf8>>,
        ?ERR_FORBIDDEN
    );
error_response(Req, not_found) ->
    %% 码不存在/撤销/过期统一折叠 + 班不存在（防探测，通用 404 码）
    elib_response:error_with_status(Req, 404, <<"邀请码无效或班级不存在"/utf8>>, ?ERR_NOT_FOUND);
error_response(Req, learner_not_in_class) ->
    elib_response:error_with_status(
        Req,
        422,
        <<"所选学员不在该班级中"/utf8>>,
        ?ERR_INVITE_CODE_INVALID
    );
error_response(Req, _Other) ->
    elib_response:error_with_status(Req, 500, <<"操作失败，请重试"/utf8>>, ?ERR_ERROR).
