#!/usr/bin/env python3
"""imboy/api/gen_aggregate.py — 生成式聚合桥接（三受众布局配套工具）。

三个手工编辑真源：
    api/openapi-api.yaml        surface api（业务 v1）
    api/openapi-adm.yaml        surface adm（管理后台）
    api/openapi-internal.yaml   surface internal（企业集成 v1）

本脚本把三者的 paths 段求并集，生成汇总入口 api/openapi.yaml（GENERATED，
请勿手改）。聚合文件的存在是为了让既有的契约校验工具链
（check_rest_contract_coverage.sh / check_release_consistency.sh /
contract_gate.py）以及单文件导入方继续零改动工作。

用法（在仓库任意位置）：
    python3 api/gen_aggregate.py           # 重新生成 api/openapi.yaml
    python3 api/gen_aggregate.py --check   # 只校验磁盘文件与应生成内容一致

--check 不一致时 exit 1（CI/门禁用）。纯文本解析，无第三方依赖；
三个 root 的格式由本仓约定（path key 两空格缩进，下一行为 $ref）。
"""
import os
import re
import sys

API_DIR = os.path.dirname(os.path.abspath(__file__))
ROOTS = ['openapi-api.yaml', 'openapi-adm.yaml', 'openapi-internal.yaml']
AGG = 'openapi.yaml'

PATH_KEY_RE = re.compile(r'^  (/[^:\s]*):$')
REF_RE = re.compile(r"^\s+\$ref: '(\./paths/[^']+)'$")
TAG_NAME_RE = re.compile(r'^  - name: (\S+)$')


def read(name):
    return open(os.path.join(API_DIR, name)).read().splitlines()


def section_slice(lines, start_re, end_res):
    """Lines from start anchor (inclusive) to the first of end anchors
    (exclusive). end_res may be None (to EOF)."""
    start = None
    for i, l in enumerate(lines):
        if re.match(start_re, l):
            start = i
            break
    if start is None:
        return None
    end = len(lines)
    for i in range(start + 1, len(lines)):
        if end_res and any(re.match(e, lines[i]) for e in end_res):
            end = i
            break
    return lines[start:end]


def parse_paths(root_lines, name):
    """Return ordered list of (key, ref). Supports 'paths: {}' empty form."""
    sec = section_slice(root_lines, r'^paths: \{\}$', None)
    if sec is not None:
        return []
    sec = section_slice(root_lines, r'^paths:$', (r'^[a-zA-Z#]',))
    if sec is None:
        sys.exit(f'{name}: no paths section')
    out = []
    i = 1
    while i < len(sec):
        m = PATH_KEY_RE.match(sec[i])
        if m:
            ref = REF_RE.match(sec[i + 1]) if i + 1 < len(sec) else None
            if not ref:
                sys.exit(f'{name}: path key {m.group(1)} next line is not a $ref bridge')
            out.append((m.group(1), ref.group(1)))
            i += 2
            continue
        i += 1
    return out


def parse_tag_blocks(root_lines):
    sec = section_slice(root_lines, r'^tags:$', (r'^[a-zA-Z#]',))
    if sec is None:
        return {}
    blocks = {}
    cur, buf = None, []
    for l in sec[1:]:
        m = TAG_NAME_RE.match(l)
        if m:
            if cur:
                blocks[cur] = buf
            cur, buf = m.group(1), [l]
        elif cur is not None:
            buf.append(l)
    if cur:
        blocks[cur] = buf
    return blocks


def parse_components_entries(root_lines):
    """Return {subsection: {entry_key: [lines]}} from the components section."""
    sec = section_slice(root_lines, r'^components:$', (r'^security:$', r'^[a-zA-Z]'))
    if sec is None:
        return {}
    result = {}
    sub, key, buf = None, None, []
    for l in sec[1:]:
        m = re.match(r'^  ([A-Za-z]+):$', l)
        if m:
            if sub and key:
                result[sub][key] = buf
            sub = m.group(1)
            result.setdefault(sub, {})
            key, buf = None, []
            continue
        if sub is None:
            continue
        e = re.match(r'^    ([A-Za-z][\w-]*):', l)
        if e and key is None:
            key, buf = e.group(1), [l]
        elif key is not None:
            buf.append(l)
    if sub and key:
        result[sub][key] = buf
    # strip trailing blank lines from each entry buffer
    for sub in result:
        for k in result[sub]:
            while result[sub][k] and result[sub][k][-1].strip() == '':
                result[sub][k].pop()
    return result


def build():
    root_paths = []
    tag_blocks = {}
    comp_entries = {}
    version = None
    header_meta = {}
    for name in ROOTS:
        lines = read(name)
        ps = parse_paths(lines, name)
        root_paths.append((name, ps))
        for t, blk in parse_tag_blocks(lines).items():
            tag_blocks.setdefault(t, blk)
        for sub, ents in parse_components_entries(lines).items():
            merged = comp_entries.setdefault(sub, {})
            for k, blk in ents.items():
                if k in merged and merged[k] != blk:
                    sys.exit(f'components/{sub}/{k} differs between roots')
                merged.setdefault(k, blk)
        if version is None:
            m = re.search(r'^  version: (\S+)$', '\n'.join(lines), re.M)
            assert m, f'{name}: no info.version'
            version = m.group(1)
        if 'servers' not in header_meta:
            header_meta['servers'] = section_slice(lines, r'^servers:$', (r'^tags:$',))
        if 'security' not in header_meta:
            header_meta['security'] = section_slice(lines, r'^security:$', None)

    # duplicate key guard across roots
    seen = {}
    for name, ps in root_paths:
        for k, _ in ps:
            if k in seen:
                sys.exit(f'path key {k} declared in both {seen[k]} and {name}')
            seen[k] = name

    out = []
    out.append('# ----------------------------------------------------------------------------')
    out.append('# imboy/api/openapi.yaml — 三受众聚合桥接（GENERATED，请勿手改）')
    out.append('#')
    out.append('# 本文件由 gen_aggregate.py 从三个编辑真源生成并维护并集：')
    out.append('#   openapi-api.yaml（surface api）/ openapi-adm.yaml（surface adm）/')
    out.append('#   openapi-internal.yaml（surface internal）。')
    out.append('# 手改会被下次生成覆盖；改契约请改对应 root 或 paths/ 下端点文件，')
    out.append('# 然后运行 python3 api/gen_aggregate.py。保留本聚合文件是为了让')
    out.append('# check_rest_contract_coverage.sh / check_release_consistency.sh /')
    out.append('# contract_gate.py 等工具与单文件导入方零改动工作。')
    out.append('# ----------------------------------------------------------------------------')
    out.append('openapi: 3.1.0')
    out.append('info:')
    out.append('  title: IMBoy REST API')
    out.append(f'  version: {version}')
    out.append('  x-stability: stable')
    out.append('  summary: IMBoy HTTP / REST 接口聚合入口（三受众并集）')
    out.append('  description: |')
    out.append('    本文件是**生成式聚合桥接**：三个受众编辑真源（openapi-api.yaml /')
    out.append('    openapi-adm.yaml / openapi-internal.yaml）的 paths 并集，由')
    out.append('    gen_aggregate.py 维护。请勿手改本文件；改契约请改对应 root 或')
    out.append('    paths/ 下端点文件，然后运行 python3 api/gen_aggregate.py。')
    out.append('    完整路径清单以 `imboy/src/imboy_router.erl` 为权威；契约机器真源见')
    out.append('    `.contract/api_contract.json`（contract-export）。')
    out.append('  license:')
    out.append('    name: BUSL-1.1')
    out.append('    url: https://mariadb.com/bsl11/')
    out.append('  contact:')
    out.append('    name: IMBoy Maintainers')
    out.append('    url: https://github.com/imboy-pub/imboy')
    out.extend(header_meta['servers'])
    out.append('tags:')
    for t, blk in tag_blocks.items():
        out.extend(blk)
    out.append('paths:')
    surface_note = {
        'openapi-api.yaml': 'surface api — 业务 v1 端点（编辑真源 openapi-api.yaml）',
        'openapi-adm.yaml': 'surface adm — 管理后台端点（编辑真源 openapi-adm.yaml）',
        'openapi-internal.yaml': 'surface internal — 企业集成 v1 端点（编辑真源 openapi-internal.yaml）',
    }
    for name, ps in root_paths:
        if not ps:
            continue
        out.append(f'  # ---- {surface_note[name]} ----')
        for k, ref in ps:
            out.append(f'  {k}:')
            out.append(f"    $ref: '{ref}'")
    out.append('components:')
    for sub, ents in comp_entries.items():
        out.append(f'  {sub}:')
        for k, blk in ents.items():
            out.extend(blk)
            out.append('')
        while out and out[-1] == '' and out[-2] == '':
            out.pop()
    out.extend(header_meta['security'])
    return '\n'.join(out).rstrip('\n') + '\n'


def main():
    if '--check' in sys.argv:
        want = build()
        cur = open(os.path.join(API_DIR, AGG)).read()
        if want == cur:
            print(f'{AGG}: up to date with {", ".join(ROOTS)}')
            return 0
        print(f'{AGG}: OUT OF SYNC with the three roots — rerun '
              f'"python3 api/gen_aggregate.py" and commit the result')
        return 1
    target = os.path.join(API_DIR, AGG)
    content = build()
    with open(target, 'w') as f:
        f.write(content)
    n_paths = content.count("    $ref: './paths/")
    print(f'generated {AGG}: {n_paths} bridged path keys')
    return 0


if __name__ == '__main__':
    sys.exit(main())
