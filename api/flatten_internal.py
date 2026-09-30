#!/usr/bin/env python3
"""imboy/api/flatten_internal.py — internal 契约确定性展平器。

读取编辑真源 api/openapi-internal.yaml（surface internal），把 paths 段的
每个 $ref 解析到 paths/internal/v1/ 下的端点 leaf，再递归内联 leaf 内引用的
components/responses/internal/ 与 components/schemas/internal/ 文件级 $ref，
生成**完全展开、零 $ref** 的单文件契约 api/openapi-internal.bundle.yaml
（OpenAPI 3.1.0），供 Postman 导入（计划 §15）与单文件交付。

生成是纯函数式的：同样输入必然产出同样字节（确定性 diff 友好）；
info/servers/tags/security/components 段原样保留自 root。

用法（在仓库任意位置）：
    python3 api/flatten_internal.py           # 重新生成并自校验
    python3 api/flatten_internal.py --check   # 只校验磁盘文件与应生成内容一致

--check 不一致或自校验失败时 exit 1（CI/门禁用）。需要 PyYAML。
后续维护口径：改契约改 openapi-internal.yaml 或 paths/internal/v1/**，
然后运行 `python3 api/flatten_internal.py` 并提交结果。
"""
import copy
import sys
from pathlib import Path

import yaml

API_DIR = Path(__file__).resolve().parent
ROOT = API_DIR / 'openapi-internal.yaml'
BUNDLE = API_DIR / 'openapi-internal.bundle.yaml'
METHODS = {'get', 'post', 'put', 'patch', 'delete', 'head', 'options', 'trace'}

# 自校验冻结口径（计划 §15.1 修正后）
EXPECT_PATHS = 28
EXPECT_OPS = 36


def load_yaml(path):
    return yaml.safe_load(path.read_text(encoding='utf-8'))


class Flattener:
    """把文件级 $ref 递归内联为具体内容（base 为当前 ref 所在文件目录）。"""

    def resolve(self, node, base, stack):
        if isinstance(node, list):
            return [self.resolve(item, base, stack) for item in node]
        if not isinstance(node, dict):
            return node
        ref = node.get('$ref')
        if isinstance(ref, str):
            if ref.startswith('#'):
                sys.exit('flatten_internal: unsupported fragment ref: %s' % ref)
            target = (base / ref).resolve()
            if not target.is_file():
                sys.exit('flatten_internal: dangling $ref %s (from %s)' % (ref, base))
            if target in stack:
                sys.exit('flatten_internal: cyclic $ref via %s' % target)
            doc = load_yaml(target)
            return self.resolve(doc, target.parent, stack + [target])
        if '$ref' in node:
            # 3.1 允许 $ref 携带兄弟键；当前真源不使用，但保守支持：
            # 解析目标内容后兄弟键覆盖同名字段。
            resolved = self.resolve({'$ref': node['$ref']}, base, stack)
            merged = dict(resolved)
            for k, v in node.items():
                if k != '$ref':
                    merged[k] = self.resolve(v, base, stack)
            return merged
        return {k: self.resolve(v, base, stack) for k, v in node.items()}


def build():
    root = load_yaml(ROOT)
    fl = Flattener()
    paths = {}
    for path, node in root['paths'].items():
        if not (isinstance(node, dict) and isinstance(node.get('$ref'), str)):
            sys.exit('flatten_internal: path %s is not a $ref bridge' % path)
        leaf = (API_DIR / node['$ref']).resolve()
        if not leaf.is_file():
            sys.exit('flatten_internal: missing leaf %s' % node['$ref'])
        paths[path] = fl.resolve(load_yaml(leaf), leaf.parent, [leaf])
    bundle = {
        'openapi': '3.1.0',
        'info': copy.deepcopy(root['info']),
        'servers': copy.deepcopy(root.get('servers', [])),
        'tags': copy.deepcopy(root.get('tags', [])),
        'security': copy.deepcopy(root.get('security', [])),
        'paths': paths,
    }
    if 'components' in root:
        bundle['components'] = copy.deepcopy(root['components'])
    return bundle


def count_refs(node):
    if isinstance(node, dict):
        return (1 if '$ref' in node else 0) + sum(count_refs(v) for v in node.values())
    if isinstance(node, list):
        return sum(count_refs(v) for v in node)
    return 0


def selfcheck(bundle):
    desc = bundle['info'].get('description', '')
    for token in ('28 条', '36 个操作', '16 枚举'):
        if token not in desc:
            sys.exit('selfcheck: info.description missing frozen count token %r' % token)
    paths = bundle['paths']
    if len(paths) != EXPECT_PATHS:
        sys.exit('selfcheck: paths=%d expect %d' % (len(paths), EXPECT_PATHS))
    ops = [m for p, node in paths.items() for m in node if m.lower() in METHODS]
    if len(ops) != EXPECT_OPS:
        sys.exit('selfcheck: operations=%d expect %d' % (len(ops), EXPECT_OPS))
    nref = count_refs(bundle)
    if nref != 0:
        sys.exit('selfcheck: %d unresolved $ref left' % nref)

    # INT-07 files/presign：三字段 required、无 file_id、expires_at 为 integer
    presign = paths['/api/internal/v1/files/presign']['post']
    schema = presign['responses']['200']['content']['application/json']['schema']
    if 'file_id' in schema.get('properties', {}):
        sys.exit('selfcheck: INT-07 200 schema still declares file_id')
    if schema.get('required') != ['object_key', 'put_url', 'expires_at']:
        sys.exit('selfcheck: INT-07 required=%r' % schema.get('required'))
    if schema['properties']['expires_at'].get('type') != 'integer':
        sys.exit('selfcheck: INT-07 expires_at not integer')

    # INT-12 webhook configure：endpoint_generation required 且 integer
    configure = paths['/api/internal/v1/webhook']['put']
    schema = configure['responses']['200']['content']['application/json']['schema']
    if 'endpoint_generation' not in schema.get('required', []):
        sys.exit('selfcheck: INT-12 endpoint_generation not required')
    if schema['properties']['endpoint_generation'].get('type') != 'integer':
        sys.exit('selfcheck: INT-12 endpoint_generation not integer')

    # INT-13 deliveries/replay：Idempotency-Key required + x-imboy-idempotency=required
    replay = paths['/api/internal/v1/webhook/deliveries/{delivery_id}/replay']['post']
    idem = [p for p in replay.get('parameters', []) if p.get('name') == 'Idempotency-Key']
    if not idem or idem[0].get('required') is not True:
        sys.exit('selfcheck: INT-13 Idempotency-Key not required')
    if replay.get('x-imboy-idempotency') != 'required':
        sys.exit('selfcheck: INT-13 x-imboy-idempotency=%r' % replay.get('x-imboy-idempotency'))


class Dumper(yaml.SafeDumper):
    def ignore_aliases(self, data):
        return True


def _str_representer(dumper, data):
    style = '|' if '\n' in data else None
    return dumper.represent_scalar('tag:yaml.org,2002:str', data, style=style)


Dumper.add_representer(str, _str_representer)

HEADER = """\
# ----------------------------------------------------------------------------
# imboy/api/openapi-internal.bundle.yaml — internal surface 单文件展平契约
# （GENERATED，请勿手改）
#
# 由 api/flatten_internal.py 从编辑真源 openapi-internal.yaml +
# paths/internal/v1/** + components/{responses,schemas}/internal/**
# 确定性展平生成；完全内联、零 $ref，供 Postman 导入（计划 §15）。
# 改契约请改真源/leaf，然后运行 `python3 api/flatten_internal.py`。
# ----------------------------------------------------------------------------
"""


def render(bundle):
    body = yaml.dump(bundle, Dumper=Dumper, sort_keys=False,
                     default_flow_style=False, allow_unicode=True, width=120)
    return HEADER + body


def main():
    check_only = '--check' in sys.argv
    bundle = build()
    selfcheck(bundle)
    want = render(bundle)
    if check_only:
        cur = BUNDLE.read_text(encoding='utf-8')
        if want == cur:
            print('%s: up to date (%d paths / %d ops, 0 $ref)' %
                  (BUNDLE.name, len(bundle['paths']),
                   sum(1 for p in bundle['paths'].values()
                       for m in p if m.lower() in METHODS)))
            return 0
        print('%s: OUT OF SYNC — rerun "python3 api/flatten_internal.py" '
              'and commit the result' % BUNDLE.name)
        return 1
    BUNDLE.write_text(want, encoding='utf-8')
    print('generated %s: %d paths / %d operations, fully inlined (0 $ref)' %
          (BUNDLE.name, len(bundle['paths']),
           sum(1 for p in bundle['paths'].values()
               for m in p if m.lower() in METHODS)))
    return 0


if __name__ == '__main__':
    sys.exit(main())
