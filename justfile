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
