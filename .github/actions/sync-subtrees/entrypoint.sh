#!/bin/bash
set -euo pipefail

echo "testMerge:   ${TEST_MERGE}"
echo "dryRun:      ${DRY_RUN}"
echo "DEFAULT_REF: ${DEFAULT_REF}"
echo "TEST_REF:    ${TEST_REF}"

expected="b7ff89ebb635bba5eac9652f5eae8a5123346c1da6ef42852d4494f58b0bf0cb"
actual=$(echo "$AUTH_TOKEN" | sha256sum | awk '{print $1}' | tr -d '\r')
if [ "$actual" != "$expected" ]; then
  echo "ERROR: Invalid AUTH_TOKEN" >&2
  exit 1
fi

uname -a
git --version
gh --version
printenv | sort

git clone https://${AUTH_TOKEN}@${CI_SERVER_HOST}/${GROUP}/${REPO_NAME}.git
cd "${REPO_NAME}"

git config --global user.name "github-actions[bot]"
git config --global user.email "41898282+github-actions[bot]@users.noreply.github.com"

if [ "$TEST_MERGE" = "true" ]; then
  REF=${TEST_REF}
else
  REF=${DEFAULT_REF}
fi

if [ -z "${REF}" ]; then
  echo "ERROR: REF not set" >&2
  exit 1
fi

echo "Using REF: ${REF}"

git fetch origin
git checkout "${REF}" || git checkout -b "${REF}"

JSON=".github/subtrees.json"
export GIT_EDITOR=true

mapfile -t entries < <(jq -r 'to_entries[] | "\(.value.prefix) \(.value.url)"' "${JSON}" | envsubst)

for entry in "${entries[@]}"; do
  read -r prefix url <<< "$entry"
  echo "🧩 Pulling: $url into $prefix"
  git subtree pull --prefix "$prefix" --squash "$url" "$DEFAULT_REF"
done

if [ "$DRY_RUN" = "true" ]; then
  echo "✅ Pull complete. Dry run enabled: skipping pushes"
  exit 0
fi

echo "✅ Pull complete. Pushing subtrees back to remotes..."

for entry in "${entries[@]}"; do
  read -r prefix url <<< "$entry"
  echo "📤 Pushing $prefix to $url (branch: $REF)"
  git subtree push --prefix "$prefix" "$url" "$REF" > /dev/null 2>&1 || echo "⚠️ Push failed for $prefix" >&2
done

echo "Pushing to origin..."
git push origin "${REF}"

echo "✅ Sync complete."
