import { expect, test } from 'bun:test';
import { createMarkdownRenderer } from 'vitepress';
import config from '../.vitepress/config';

const md = await createMarkdownRenderer(process.cwd(), config.markdown);
const github = 'https://github.com/imboy-pub/imboy/blob/main/';

function render(source: string, relativePath = 'reference/env-vars.md') {
  const env = { relativePath, cleanUrls: true, links: [] as string[] };
  return { html: md.render(source, env), links: env.links };
}

test.each([
  ['reference/env-vars.md', '../../deploy/README.md#customer-input', 'deploy/README.md#customer-input'],
  ['guides/operations/single-node-ops-chain-index-2026-08.md', '../../../deploy/README.md#customer-input', 'deploy/README.md#customer-input'],
  ['guides/operations/deployment/day1-quickstart.md', '../../../../deploy/README.md#customer-input', 'deploy/README.md#customer-input'],
  ['guides/operations/deployment/livekit-single-service-runbook.md', '../../../../deploy/README.md#rtc-turn-regression', 'deploy/README.md#rtc-turn-regression'],
  ['guides/index.md', '../../deploy/README.md', 'deploy/README.md'],
  ['overview.md', '../deploy/README.md', 'deploy/README.md'],
  ['reference/env-vars.md', '../../LICENSE', 'LICENSE'],
  ['reference/env-vars.md', './../../VERSION', 'VERSION'],
  ['reference/env-vars.md', '../../deploy/README.md?plain=1#customer-input', 'deploy/README.md?plain=1#customer-input'],
])('repository links resolve from %s: %s', (page, href, target) => {
  const result = render(`[repository file](${href})`, page);
  expect(result.html).toContain(`href="${github}${target}"`);
  expect(result.html).toContain('target="_blank"');
  expect(result.html).toContain('rel="noreferrer"');
  expect(result.links).toEqual([]);
});

test('reference-style links preserve their title and deep-link target', () => {
  const result = render('[deployment][guide]\n\n[guide]: ../../deploy/README.md#customer-input "Deployment inputs"');
  expect(result.html).toContain(`href="${github}deploy/README.md#customer-input"`);
  expect(result.html).toContain('title="Deployment inputs"');
  expect(result.links).toEqual([]);
});

test.each([
  ['../glossary.md', './../glossary'],
  ['./missing-docs-link.md', './missing-docs-link'],
  ['../../deploy/missing-docs-link.md', './../../deploy/missing-docs-link'],
  ['../../../other-repository/README.md', './../../../other-repository/README'],
  ['../../deploy', './../../deploy'],
])('local or unresolved links retain VitePress checking: %s', (href, expected) => {
  const result = render(`[local file](${href})`);
  expect(result.html).toContain(`href="${expected}"`);
  expect(result.html).not.toContain(github);
  expect(result.links).toEqual([expected]);
});

test.each([
  ['https://example.com/deploy/README.md#customer-input', 'https://example.com/deploy/README.md#customer-input'],
  ['//example.com/deploy/README.md', '//example.com/deploy/README.md'],
  ['mailto:docs@example.com', 'mailto:docs@example.com'],
  ['#customer-input', '#customer-input'],
  ['/reference/env-vars.md', '/reference/env-vars'],
])('non-relative links retain their existing rendering: %s', (href, expected) => {
  const result = render(`[existing link](${href})`);
  expect(result.html).toContain(`href="${expected}"`);
  expect(result.html).not.toContain(github);
});

test('inline and fenced code keep link examples literal', () => {
  const example = '[deployment](../../deploy/README.md#customer-input)';
  const result = render('`' + example + '`\n\n```text\n' + example + '\n```');
  expect(result.html).toContain(`<code v-pre>${example}</code>`);
  expect(result.html).toContain(example);
  expect(result.html).not.toContain(github);
  expect(result.links).toEqual([]);
});

test('renders without a page context keep the existing Markdown behavior', () => {
  expect(md.render('[deployment](../../deploy/README.md#customer-input)'))
    .not.toContain(github);
});
