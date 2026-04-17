build:
    pnpm spago build

build_square: build
    pnpm esbuild square-standalone.js --bundle --outfile=dist/square-standalone/pantograph.js

serve_square: build_square
    pnpm http-server dist/square-standalone
