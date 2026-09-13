-module(moya_error).
%%%
% 墨芽教学域统一错误映射：Logic reason 原子 → imboy envelope 错误码
% Teaching error reason → error_code mapping（契约 STEP-04/error-codes.md）
%%%

-export([to_response/2]).

-include("error_code.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec to_response(cowboy_req:req(), atom()) -> cowboy_req:req().
to_response(Req, Reason) ->
    {Msg, Code} = map_reason(Reason),
    elib_response:error(Req, Msg, Code).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec map_reason(atom()) -> {binary(), integer()}.
%% 登录域（5400 段）
map_reason(provider_unconfigured) -> {<<"登录服务未配置"/utf8>>, ?ERR_TEACHING_PROVIDER_UNCONFIGURED};
map_reason(code_invalid) -> {<<"微信登录凭证无效或已使用"/utf8>>, ?ERR_WECHAT_CODE_INVALID};
map_reason(login_failed) -> {<<"微信登录失败"/utf8>>, ?ERR_WECHAT_LOGIN_FAILED};
map_reason(identity_none) -> {<<"该微信未绑定教学账号，请联系机构"/utf8>>, ?ERR_TEACHING_IDENTITY_NONE};
%% 上下文/ACL 域（5420 段）
map_reason(context_mismatch) -> {<<"所选身份不属于当前用户"/utf8>>, ?ERR_TEACHING_CONTEXT_INVALID};
map_reason(inactive) -> {<<"所选身份已失效"/utf8>>, ?ERR_TEACHING_CONTEXT_INACTIVE};
map_reason(not_guardian_list) -> {<<"未监护该学员"/utf8>>, ?ERR_TEACHING_LEARNER_NOT_GUARDED};
map_reason(not_guardian) -> {<<"无监护提交权限"/utf8>>, ?ERR_TEACHING_NOT_GUARDIAN};
map_reason(claimed_learner_mismatch) -> {<<"无监护提交权限"/utf8>>, ?ERR_TEACHING_NOT_GUARDIAN};
map_reason(cannot_submit) -> {<<"无监护提交权限"/utf8>>, ?ERR_TEACHING_NOT_GUARDIAN};
map_reason(cannot_view) -> {<<"无权查看该学员回评"/utf8>>, ?ERR_FORBIDDEN};
map_reason(not_staff) -> {<<"非本班任课老师"/utf8>>, ?ERR_TEACHING_NOT_STAFF};
map_reason(role_denied) -> {<<"当前教学角色无操作权限"/utf8>>, ?ERR_TEACHING_STAFF_WRITE_DENIED};
map_reason(cross_org) -> {<<"跨机构访问被拒绝"/utf8>>, ?ERR_TEACHING_CROSS_ORG};
%% 花名册/教学作业域（5430 段，MN-ROSTER/MN-TASK）
map_reason(class_not_visible) -> {<<"班级不存在或不可见"/utf8>>, ?ERR_TEACHING_CLASS_NOT_VISIBLE};
map_reason(learner_not_in_class) -> {<<"学员不在本班或已移出"/utf8>>, ?ERR_TEACHING_LEARNER_NOT_IN_CLASS};
map_reason(guardian_setup_required) -> {<<"监护关系需完善"/utf8>>, ?ERR_TEACHING_GUARDIAN_SETUP_REQUIRED};
%% 作业/提交域（5440 段）
map_reason(not_found) -> {<<"资源不存在"/utf8>>, ?ERR_NOT_FOUND};
map_reason(assignment_not_found) -> {<<"作业不存在"/utf8>>, ?ERR_ASSIGNMENT_NOT_FOUND};
map_reason(assets_invalid) -> {<<"提交附件不合规"/utf8>>, ?ERR_SUBMISSION_ASSETS_INVALID};
map_reason(assignment_closed) -> {<<"作业已截止或关闭"/utf8>>, ?ERR_ASSIGNMENT_CLOSED};
map_reason(already_withdrawn) -> {<<"该提交已撤回"/utf8>>, ?ERR_SUBMISSION_WITHDRAWN};
%% 幂等域（5460 段）
map_reason(idempotency_conflict) -> {<<"请求与幂等键已绑定内容冲突"/utf8>>, ?ERR_IDEMPOTENCY_CONFLICT};
map_reason(idempotency_key_required) -> {<<"缺少幂等键"/utf8>>, ?ERR_IDEMPOTENCY_KEY_REQUIRED};
%% 回评域（5480 段）
map_reason(no_draft) -> {<<"无可发布的回评草稿"/utf8>>, ?ERR_REVIEW_DRAFT_NOT_FOUND};
map_reason(already_reviewed) -> {<<"该提交已有发布回评，不可撤回"/utf8>>, ?ERR_SUBMISSION_REVIEWED};
%% DC-2：存草稿遇已发布回评与撤回语境拆分（5486），5481 保留给撤回场景
map_reason(review_published) -> {<<"回评已发布，不能再保存草稿"/utf8>>, ?ERR_REVIEW_PUBLISHED_DRAFT};
map_reason(withdrawn) -> {<<"提交已撤回，无法发布回评"/utf8>>, ?ERR_REVIEW_SUBMISSION_WITHDRAWN};
map_reason(confirm_mismatch) -> {<<"发布确认学员不一致"/utf8>>, ?ERR_REVIEW_CONFIRM_MISMATCH};
map_reason(reserved_field) -> {<<"请求包含服务端保留字段"/utf8>>, ?ERR_REVIEW_FIELD_NOT_ACCEPTED};
map_reason(empty_content) -> {<<"回评内容为空"/utf8>>, ?ERR_REVIEW_EMPTY_CONTENT};
map_reason(char_reviews_invalid) -> {<<"逐字点评字卡数据不合规"/utf8>>, ?ERR_REVIEW_CHAR_REVIEWS_INVALID};
%% 参数/通用
map_reason(bad_param) -> {<<"参数错误"/utf8>>, ?ERR_PARAM_INVALID};
map_reason(missing_param) -> {<<"缺少必填参数"/utf8>>, ?ERR_MISSING_PARAM};
map_reason(forbidden) -> {<<"无权访问该资源"/utf8>>, ?ERR_FORBIDDEN};
map_reason(db_error) -> {<<"操作失败，请重试"/utf8>>, ?ERR_ERROR};
map_reason(Other) when is_atom(Other) -> {<<"操作失败，请重试"/utf8>>, ?ERR_ERROR}.
