#!/usr/bin/env bash
# 生产配置键预检（2026-09-23 附件/客服上线连环 4xx 的根治）：
# 「新代码读新配置键 → 逐机 sys.pro.config 缺键 → fail-closed 线上 4xx/5xx」
# 的模式当日连踩三次（cs_widget_subject_key / eb_enterprise_keyring /
# cors_widget_origins）。本检查在 backend 蓝绿部署前置跑一次：远端配置
# 真源（overlay 输入）必须含全部必需键且值非空，缺失即拒绝部署。
#
# 由 imboy-deploy.sh source 后调用：check_prod_config SSH_OPTS "$SERVER_USER@$SERVER_HOST" "$SERVER_PORT"
# （依赖宿主的 ok/fail 与 SSH_OPTS 数组、SSH ControlMaster 免密通道。）
#
# 维护纪律：后端新增「无合理默认值、缺配即 fail-closed」的配置键时，
# 同步在 REQUIRED_KEYS 追加；只新增键不更新本清单 = 本检查形同虚设。

# 必需键（imboy app env；值非空才放行）：
#   * cs_widget_subject_key   挂件匿名访客 HMAC 材料（缺→bootstrap 422）
#   * eb_enterprise_keyring   企业信封加密密钥环（缺→建 contact 500 missing_key）
#   * cors_widget_origins     挂件面 CORS origin 名单（缺→跨域上传/调用 403）
REQUIRED_KEYS=(cs_widget_subject_key eb_enterprise_keyring cors_widget_origins)

check_prod_config() {
  local host="$1" port="$2"
  local remote_escript="/tmp/imboy-config-key-guard.escript.$$"
  local erl="/usr/local/lib/erlang/bin/escript"
  local conf="/www/wwwroot/imboy-api/config/sys.pro.config"
  local key_list="["
  local first=1 k
  for k in "${REQUIRED_KEYS[@]}"; do
    [ "$first" = "1" ] || key_list+=","
    key_list+="${k}"   # Erlang atom 字面量：Env 键是 atom，binary 比对永不相等
    first=0
  done
  key_list+="]"

  cat > "$remote_escript" <<REMOTE
#!/usr/local/lib/erlang/bin/escript
main([]) ->
    Conf = "$conf",
    {ok, Terms} = file:consult(Conf),
    %% sys.pro.config 允许嵌套 list（[[...]]），consult 后展平找 {imboy, Env}
    Flat = lists:flatten(Terms),
    {imboy, Env} = lists:keyfind(imboy, 1, Flat),
    Required = $key_list,
    Missing = [K || K <- Required, lists:keyfind(K, 1, Env) =:= false],
    Empty = [K || K <- Required, {K, V} <- [lists:keyfind(K, 1, Env)], is_binary(V), V =:= <<>>],
    case {Missing, Empty} of
        {[], []} -> io:format("CONFIG_KEYS_OK~n");
        _ -> io:format("CONFIG_KEYS_FAIL missing=~p empty_binary=~p~n", [Missing, Empty]), halt(1)
    end.
REMOTE

  local out
  # SSH_OPTS 里的 -p（ssh 端口）对 scp 必须是大写 -P：数组展开时替换
  if out="$(scp -P "$port" "${SSH_OPTS[@]/-p/-P}" -q "$remote_escript" "$host:$remote_escript" \
    && ssh -p "$port" "${SSH_OPTS[@]}" "$host" \
      "if [ -x '$erl' ]; then E='$erl'; else E=\$(command -v escript || true); fi; \
       [ -n \"\$E\" ] || { echo GUARD_NO_ESCRIPT; exit 9; }; \
       \"\$E\" '$remote_escript' 2>&1 | tail -1")"; then
    rm -f "$remote_escript"
    ssh -p "$port" "${SSH_OPTS[@]}" "$host" "rm -f '$remote_escript'" 2>/dev/null || true
  else
    rm -f "$remote_escript"
    fail "生产配置键预检执行失败（SSH/远端 escript 异常）——请人工核对 $host:$conf"
  fi
  case "$out" in
    *CONFIG_KEYS_OK*)
      ok "生产配置键预检通过（${#REQUIRED_KEYS[@]} 个必需键齐全非空）"
      ;;
    *CONFIG_KEYS_FAIL*)
      fail "生产配置缺必需键: $out
  修复：编辑 $host:$conf 的 {imboy, [...]} 块补齐缺失键
  （形状/secret 文件变体见 config/sys.config.example 对应注释），
  并同步到当前 release 的 releases/<vsn>/sys.config 后重启节点。"
      ;;
    *)
      fail "生产配置键预检输出异常: $out"
      ;;
  esac
}
