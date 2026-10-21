#!/usr/bin/env python3
"""Only the registered talk_record routes delegate augments the main router."""
import argparse
import re
from pathlib import Path


def uncomment(source):
    # Preserve quoted path/atom text; a percent inside a string is not a comment.
    token = r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'|%[^\n]*'
    return re.sub(token, lambda m: '' if m.group(0).startswith('%') else m.group(0), source)


def get_routes_expressions(source):
    heads = list(re.finditer(r'^get_routes\(\)\s*->', source, re.M))
    if not heads:
        return []
    if len(heads) != 1:
        raise ValueError('Unsupported get_routes clauses')
    start = heads[0].end()
    depth = 0
    quote = None
    escaped = False
    expressions = []
    cursor = start
    for i in range(start, len(source)):
        ch = source[i]
        if quote:
            if escaped:
                escaped = False
            elif ch == '\\':
                escaped = True
            elif ch == quote:
                quote = None
            continue
        if ch in ('"', "'"):
            quote = ch
        elif ch in '([{':
            depth += 1
        elif ch in ')]}':
            depth -= 1
            if depth < 0:
                raise ValueError('Unbalanced get_routes body')
        elif depth == 0 and ch in ',.':
            expressions.append(source[cursor:i].strip())
            cursor = i + 1
            if ch == '.':
                return expressions
    raise ValueError('Unterminated get_routes body')


def registered_talk_record(source):
    expressions = get_routes_expressions(source)
    if not expressions:
        return False
    body = ','.join(expressions)
    executable = re.sub(r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'', '""', body)
    if not re.search(r'\btalk_record_routes:routes\s*\(', executable):
        return False
    # Only the current unconditional assignment/consumption chain is supported.
    # A fun/case/if expression or any changed return structure must fail closed.
    if re.search(r'\b(fun|case|if|receive|try|begin)\b', executable):
        raise ValueError('Conditional/deferred route registration is unsupported')
    names = ['Host', 'MainRoutes', 'ApiV1Routes', 'AdmRoutes',
             'CompiledApiRoutes', 'CompiledAdmRoutes', 'CompiledPluginRoutes', 'CoreRoutes']
    assignments = []
    for expression in expressions[:-1]:
        match = re.fullmatch(r'([A-Z][A-Za-z0-9_]*)\s*=\s*(.*)', expression, re.S)
        if not match:
            raise ValueError('Unsupported get_routes expression')
        assignments.append((match.group(1), re.sub(r'\s+', '', match.group(2))))
    if [name for name, _ in assignments] != names:
        raise ValueError('Unsupported get_routes assignment chain')
    values = dict(assignments)
    expected = 'customer_service_wire(enterprise_wire(imboy_feature:compiled_routes(api,ApiV1Routes++talk_record_routes:routes())))'
    if values['CompiledApiRoutes'] != expected:
        raise ValueError('Delegate is not the unconditional compiled API assignment')
    if values['CoreRoutes'] != 'MainRoutes++CompiledApiRoutes++CompiledAdmRoutes++enterprise_internal_routes()':
        raise ValueError('CoreRoutes does not consume the compiled API routes')
    if re.sub(r'\s+', '', expressions[-1]) != '[{Host,CoreRoutes++CompiledPluginRoutes}]':
        raise ValueError('Final return does not consume CoreRoutes')
    return True


def talk_record_delegate(router_source, repo):
    router = uncomment(router_source)
    if not registered_talk_record(router):
        return ''
    path = Path(repo) / 'src/api/talk_record_routes.erl'
    source = uncomment(path.read_text(encoding='utf-8'))
    if not re.search(r'-module\(talk_record_routes\)\.', source):
        raise ValueError('Unexpected talk_record route module')
    body = re.search(r'^routes\(\)\s*->\s*(\[.*?\])\s*\.', source, re.M | re.S)
    if body is None:
        raise ValueError('Expected literal talk_record routes/0 list')
    return body.group(1)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--repo', required=True, type=Path)
    args = parser.parse_args()
    router = (args.repo / 'src/imboy_router.erl').read_text(encoding='utf-8')
    print(uncomment(router))
    print(talk_record_delegate(router, args.repo))


if __name__ == '__main__':
    main()
