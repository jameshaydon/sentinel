# Generate all the things
[group('gen')]
gen: hpack

# Generate Haskell compiler files
[group('gen')]
hpack:
  cd backend && hpack && gen-hie > hie.yaml

# Format all the code
[group('lint')]
fmt:
  treefmt

# Autoreload GHCi
[group('dev')]
watch-haskell: hpack
  cd backend && ghcid --command "cabal repl" --warnings

[group('build')]
build: hpack
  cd backend && cabal build -O0

# Run weeder to detect dead code
[group('lint')]
weeder: build
  cd backend && weeder --hie-directory ".hie"

[group('test')]
test: build
  cd backend && cabal test -O0

# Start WebSocket backend for the frontend
[group('dev')]
backend-ws example='passport' user='usr_test' port='9090': build
  cd backend && cabal run repl -- --example {{example}} --user {{user}} --mode websocket --port {{port}}

[group('dev')]
frontend-dev:
  cd sentinel-web && npm run dev

[group('build')]
frontend-build:
  cd sentinel-web && npm run build

# Type-check the frontend
[group('lint')]
frontend-check:
  cd sentinel-web && npx svelte-check --tsconfig ./tsconfig.json

[group('gen')]
frontend-install:
  cd sentinel-web && npm install
