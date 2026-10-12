import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import vm from 'node:vm';
import { test } from 'node:test';

// Supply HTML rendered by org_invite_landing_handler:landing_html/1 or fetched
// from the deployed public URL. This executes the shipped script, not a copy.
const html = readFileSync(process.env.INVITE_HTML, 'utf8');
const script = html.match(/<script>([\s\S]*?)<\/script>/)?.[1];
assert.ok(script, 'invitation page must contain its copy script');
const code = html.match(/<div class="code">([^<]+)<\/div>/)?.[1];
assert.match(code, /^[A-HJ-NP-Z2-9]{8}$/);

for (const mode of ['clipboard', 'fallback', 'rejected', 'false', 'throws']) {
  test(`copy invitation: ${mode}`, async () => {
    const button = { textContent: '复制邀请码' };
    const input = { value: '', select() {} };
    let copied;
    let removed = false;
    const sandbox = {
      navigator: mode === 'clipboard' || mode === 'rejected' ? {
        clipboard: { async writeText(value) {
          if (mode === 'rejected') throw new Error('denied');
          copied = value;
        } },
      } : {},
      document: {
        getElementById: () => button,
        createElement: () => input,
        body: { appendChild() {}, removeChild() { removed = true; } },
        execCommand() {
          if (mode === 'throws') throw new Error('denied');
          if (mode === 'false') return false;
          copied = input.value;
          return true;
        },
      },
      setTimeout() {},
    };
    vm.createContext(sandbox);
    vm.runInContext(script, sandbox);
    vm.runInContext('copyCode()', sandbox);
    await new Promise(setImmediate);
    const success = !['false', 'throws'].includes(mode);
    assert.equal(button.textContent, success
      ? '已复制，打开 imboy 粘贴即可' : '复制失败，请长按上方邀请码');
    if (success) assert.equal(copied, code);
    if (mode !== 'clipboard') assert.ok(removed);
  });
}
