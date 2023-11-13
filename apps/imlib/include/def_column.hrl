
% password 给元素顺序不能够随意修改，
% login_success_transfer 按顺序匹配了
% 密码校验取第4位为密码数据 lists:nth(5, Row)
-define (LOGIN_COLUMN, <<"id,account,mobile,password,email,
        nickname,avatar,gender,region,sign">>).

% 顺序不能够随意修改，id 要放到第一个
-define (DEF_USER_COLUMN, <<"id,account,
        nickname,avatar,sign,gender,region">>).

