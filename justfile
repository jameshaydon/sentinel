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
