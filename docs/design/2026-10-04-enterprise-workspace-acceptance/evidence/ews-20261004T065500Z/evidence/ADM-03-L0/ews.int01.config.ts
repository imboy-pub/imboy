import { defineConfig } from '@playwright/test'

// EWS run (ews-20261004T065500Z) int01 stack config:
// bypasses main config's testIgnore of enterprise-ent-int01, pins baseURL to
// the run-scoped admin dev server (8906 → VITE_PROXY_TARGET 20800).
export default defineConfig({
  testDir: './tests/e2e/enterprise-ent-int01',
  timeout: 120_000,
  fullyParallel: false,
  workers: 1,
  retries: 0,
  reporter: [['line']],
  use: {
    baseURL: process.env.IMBOY_ADMIN_E2E_BASE_URL || 'http://127.0.0.1:8906',
    headless: true,
  },
})
