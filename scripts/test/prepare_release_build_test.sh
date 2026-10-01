#!/usr/bin/env bash
set -Eeuo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf -- "$TMP"' EXIT
mkdir -p "$TMP/src" "$TMP/ebin" "$TMP/.erlang.mk" "$TMP/bin"
cat >"$TMP/bin/make" <<'MOCK'
#!/usr/bin/env bash
echo "$*" >> events
if [[ "$1" == clean-app ]]; then
  rm -f ebin/imboy.app ebin/*.beam
fi
MOCK
chmod +x "$TMP/bin/make"
export PATH="$TMP/bin:$PATH"
cd "$TMP"
reset_fixture() {
  rm -f .erlang.mk/imboy.test ebin/*.beam
  printf '{vsn, "1.0.0"}\n' > ebin/imboy.app
  touch src/one.erl ebin/one.beam
  : > events
}
check() {
  bash "$ROOT/scripts/lib/prepare_release_build.sh" 1.0.0
  [[ "$(cat events)" == "$1" ]]
}
reset_fixture
check 'beam-presence-guard'
reset_fixture
touch .erlang.mk/imboy.test
check $'clean-app\nbeam-presence-guard'
reset_fixture
printf '{vsn, "0.9.0"}\n' > ebin/imboy.app
check $'clean-app\nbeam-presence-guard'
reset_fixture
touch ebin/deleted.beam
check $'clean-app\nbeam-presence-guard'
reset_fixture
rm ebin/imboy.app
check $'clean-app\nbeam-presence-guard'
echo 'PASS: 增量缓存、测试模式、版本变化、删除模块、首次构建'

# 实测 rsync 参数：同内容不同 mtime 不传输；旧时间戳的内容变更仍更新。
mkdir incoming remote
printf old > incoming/one.erl
rsync -ac --no-times incoming/ remote/
touch -t 200001010000 incoming/one.erl
[[ -z "$(rsync -aci --no-times incoming/ remote/)" ]]
printf new > incoming/one.erl
touch -t 200001010000 incoming/one.erl
[[ -n "$(rsync -aci --no-times incoming/ remote/)" ]]
cmp incoming/one.erl remote/one.erl
[[ remote/one.erl -nt incoming/one.erl ]]
echo 'PASS: 同内容跳过上传、旧时间戳变更使用远端新时间'
