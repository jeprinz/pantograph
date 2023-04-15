install:
	# !TODO npm install

build: install
	spago build

test: build
	spago test
	# !TODO test frontend

run:
	# !TODO run frontend
