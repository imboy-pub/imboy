#!/usr/bin/env python3
"""imboy 三仓文档死链全量检查器
扫描指定 docs 目录下所有 .md 文件中的相对链接，验证目标文件是否存在。
"""
import os
import re
import sys

LINK_RE = re.compile(r'\[[^\]]*\]\(([^)]+)\)')

def check_dir(root):
    dead = []
    checked = 0
    for dirpath, _, files in os.walk(root):
        for fn in files:
            if not fn.endswith('.md'):
                continue
            fpath = os.path.join(dirpath, fn)
            try:
                with open(fpath, encoding='utf-8') as f:
                    content = f.read()
            except Exception:
                continue
            for m in LINK_RE.finditer(content):
                link = m.group(1).strip()
                # 跳过外链、锚点、mailto、asset://
                if link.startswith(('http://', 'https://', '#', 'mailto:', 'asset://', 'ftp://')):
                    continue
                # 去掉锚点部分
                target = link.split('#')[0]
                if not target:
                    continue
                # 相对当前 md 文件目录解析
                resolved = os.path.normpath(os.path.join(dirpath, target))
                checked += 1
                if not os.path.exists(resolved):
                    dead.append((os.path.relpath(fpath, root), link))
    return checked, dead

repos = [
    '/Users/leeyi/project/imboy.pub/imboy/docs',
    '/Users/leeyi/project/imboy.pub/imboyapp/docs',
    '/Users/leeyi/project/imboy.pub/imboyadmin/docs',
]

total_dead = 0
for root in repos:
    checked, dead = check_dir(root)
    name = root.split('/imboy.pub/')[-1]
    print(f'\n=== {name}: 检查 {checked} 个链接，死链 {len(dead)} 个')
    for src, link in dead[:20]:
        print(f'  [{src}] -> {link}')
    total_dead += len(dead)

print(f'\n总计死链: {total_dead}')
sys.exit(1 if total_dead else 0)
