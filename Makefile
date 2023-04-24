clean:
	rm -rf node_modules
	rm -rf output
	rm -rf .spago
	rm *.lock
	rm .cache

install:
	npm install
	npm run postinstall

build: install
	npm run build

test: build
	npm run test
	# !TODO test frontend

run:
	# !TODO run frontend
