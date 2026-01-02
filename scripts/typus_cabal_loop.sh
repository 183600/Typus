name: iflow-cabal-autoloop

on:
  workflow_dispatch:
  push:
    branches:
      - master

concurrency:
  group: iflow-autoflow-${{ github.ref }}
  cancel-in-progress: true

permissions:
  contents: write

jobs:
  # 检查是否应该运行
  check-trigger:
    runs-on: ubuntu-latest
    outputs:
      should_run: ${{ steps.check.outputs.should_run }}
    steps:
      - name: Check commit message
        id: check
        run: |
          # workflow_dispatch 总是运行
          if [[ "${{ github.event_name }}" == "workflow_dispatch" ]]; then
            echo "should_run=true" >> $GITHUB_OUTPUT
            exit 0
          fi
          
          # 获取最新的 commit message
          COMMIT_MSG="${{ github.event.head_commit.message }}"
          
          # 只有包含 [LOOP-CONTINUE] 标记的 push 才触发
          if [[ "$COMMIT_MSG" == *"[LOOP-CONTINUE]"* ]]; then
            echo "should_run=true" >> $GITHUB_OUTPUT
          else
            echo "should_run=false" >> $GITHUB_OUTPUT
            echo "ℹ️ 跳过：commit message 不包含 [LOOP-CONTINUE] 标记"
          fi

  run-5h:
    needs: check-trigger
    if: needs.check-trigger.outputs.should_run == 'true'
    runs-on: ubuntu-latest
    timeout-minutes: 360
    env:
      RUN_HOURS: 5
      WORK_BRANCH: master
      RELEASE_MARKER_FILE: .git/typus_release_tag

      IFLOW_API_KEY: ${{ secrets.IFLOW_API_KEY }}
      IFLOW_BASE_URL: https://apis.iflow.cn/v1
      IFLOW_MODEL_NAME: glm-4.6

      GH_TOKEN: ${{ github.token }}

      # 你需要在 GitHub Secrets 里配置它
      GITEE_REMOTE_URL: ${{ secrets.GITEE_REMOTE_URL }}

      # 可选：如需走镜像（更适合国内网络），把它改成你的镜像地址
      # 例如：https://mirrors.tuna.tsinghua.edu.cn/hackage/
      CABAL_MIRROR_URL: ""

      # 可选：强制 cabal 使用 curl 传输（CI 上通常更稳）
      CABAL_TRANSPORT: "curl"

    steps:
      - name: Checkout with PAT
        uses: actions/checkout@v4
        with:
          token: ${{ secrets.IFLOW_PAT }}
          fetch-depth: 0

      - name: Ensure branch and git identity
        shell: bash
        run: |
          set -euo pipefail
          BRANCH="${WORK_BRANCH:-master}"
          git fetch origin "${BRANCH}" --prune
          git checkout -B "${BRANCH}" "origin/${BRANCH}"
          git config user.name "iflow-bot"
          git config user.email "iflow-bot@users.noreply.github.com"

      - name: Setup Node (for iFlow CLI)
        uses: actions/setup-node@v4
        with:
          node-version: '22'

      - name: Install iFlow CLI
        shell: bash
        run: |
          set -euo pipefail
          npm i -g @iflow-ai/iflow-cli@latest
          iflow --version

      - name: Verify IFLOW_API_KEY is present
        shell: bash
        run: |
          set -euo pipefail
          test -n "${IFLOW_API_KEY:-}" || { echo "Missing IFLOW_API_KEY secret"; exit 1; }

      - name: Verify GITEE_REMOTE_URL is present
        shell: bash
        run: |
          set -euo pipefail
          test -n "${GITEE_REMOTE_URL:-}" || { echo "Missing GITEE_REMOTE_URL secret"; exit 1; }

      - name: Setup Haskell (GHC + Cabal)
        uses: haskell-actions/setup@v2
        with:
          ghc-version: 'latest'
          cabal-version: 'latest'
          # 关键：不要让 setup action 内部自动 cabal update
          cabal-update: false

      - name: Show tool versions
        shell: bash
        run: |
          set -euo pipefail
          ghc --version
          cabal --version

      - name: Cache cabal (store + index) and dist-newstyle
        uses: actions/cache@v4
        with:
          path: |
            ~/.cabal/store
            ~/.cabal/packages
            dist-newstyle
          key: cabal-${{ runner.os }}-${{ hashFiles('**/*.cabal','cabal.project*') }}
          restore-keys: |
            cabal-${{ runner.os }}-

      - name: Configure cabal (http-transport + optional mirror)
        shell: bash
        run: |
          set -euo pipefail

          mkdir -p ~/.cabal
          if [ ! -f ~/.cabal/config ]; then
            cabal user-config init
          fi

          # 使用 curl 作为 http 传输（cabal 识别的是 http-transport）
          if [ "${CABAL_TRANSPORT:-}" = "curl" ]; then
            if ! grep -qE '^[[:space:]]*http-transport:[[:space:]]*curl[[:space:]]*$' ~/.cabal/config; then
              printf '\nhttp-transport: curl\n' >> ~/.cabal/config
            fi
          fi

          # 可选：切换到镜像（仅当 CABAL_MIRROR_URL 非空）
          if [ -n "${CABAL_MIRROR_URL:-}" ]; then
            # 兼容缩进：匹配任何以 url: 开头且包含 hackage.haskell.org 的行
            sed -i -E "s#^([[:space:]]*url:[[:space:]]*).*(hackage\\.haskell\\.org).*#\\1${CABAL_MIRROR_URL}#g" ~/.cabal/config || true
          fi

      - name: Cabal update (retry, ignore project)
        shell: bash
        run: |
          set -euo pipefail
          for i in 1 2 3 4 5; do
            echo "cabal update attempt $i"
            if cabal update --ignore-project; then
              exit 0
            fi
            sleep $((i * 5))
          done
          echo "cabal update failed after retries" >&2
          exit 1

      - name: Make loop script executable
        shell: bash
        run: |
          set -euo pipefail
          chmod +x scripts/typus_cabal_loop.sh

      - name: Run for ${{ env.RUN_HOURS }} hours
        shell: bash
        run: |
          set -euo pipefail
          timeout --signal=TERM $(( RUN_HOURS * 3600 )) bash scripts/typus_cabal_loop.sh || true

      - name: Commit & push once (GitHub + Gitee)
        if: always()
        shell: bash
        run: |
          set -euo pipefail
          BRANCH="${WORK_BRANCH:-master}"

          # 确保 gitee 远端存在（不打印 URL，避免日志泄漏）
          if git remote get-url gitee >/dev/null 2>&1; then
            git remote set-url gitee "${GITEE_REMOTE_URL}"
          else
            git remote add gitee "${GITEE_REMOTE_URL}"
          fi

          # 最后统一提交一次（如果有 staged 变更）
          # 使用 [LOOP-CONTINUE] 标记，确保这次 push 能触发下一轮
          git add -A
          if ! git diff --cached --quiet; then
            git commit -m "ci: ${RUN_HOURS}h batch update (run $GITHUB_RUN_ID) [LOOP-CONTINUE]" || true
          else
            # 即使没有变更，也创建一个空提交来触发下一轮
            git commit --allow-empty -m "ci: ${RUN_HOURS}h loop continue (run $GITHUB_RUN_ID) [LOOP-CONTINUE]" || true
          fi

          # marker 可能在 env 指定路径，也可能在真实 gitdir 下（兼容 worktree 场景）
          MARKER_FILE="${RELEASE_MARKER_FILE}"
          if [[ ! -f "${MARKER_FILE}" ]]; then
            GIT_DIR_REAL="$(git rev-parse --git-dir 2>/dev/null || echo ".git")"
            MARKER_FILE="${GIT_DIR_REAL%/}/typus_release_tag"
          fi
          if [[ -f "${MARKER_FILE}" ]]; then
            echo "Release tag prepared: $(cat "${MARKER_FILE}")"
          fi

          # 推送到 GitHub（origin）和 Gitee（gitee）
          git push origin "HEAD:${BRANCH}" --follow-tags
          git push gitee "HEAD:${BRANCH}" --follow-tags --force
