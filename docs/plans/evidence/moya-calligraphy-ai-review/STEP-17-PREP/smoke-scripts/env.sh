#!/bin/bash
# 冒烟打点共用环境：BASE/令牌。令牌经 sign_jwt.escript 运行时签发（零硬编码密钥）。
# 用法：source env.sh（须在 imboy 仓根执行，escript 依赖 deps/jwerl）
export SMOKE_BASE=${SMOKE_BASE:-http://127.0.0.1:9811/api/v1}
export SMOKE_CONF=${SMOKE_CONF:-config/sys.local.config}
T() { escript "$(dirname "${BASH_SOURCE[0]}")/sign_jwt.escript" "$1" "$SMOKE_CONF"; }
export TK_GUARDIAN=$(T 770002)   # 家长（can_submit+can_view_review）
export TK_TEACHER=$(T 770001)    # orgA owner + A1 班 teacher
export TK_MANAGER=$(T 770004)    # A1 班 manager（绑定操作）
export TK_ASSISTANT=$(T 770003)  # assistant（写拒绝对照）
export TK_ORGB=$(T 770005)       # orgB owner（跨 Org 拒绝对照）
export TK_TARGET=$(T 770006)     # 绑定目标账号（本人 history 分支）
