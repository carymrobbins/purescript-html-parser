.PHONY: default build quick test deps clean

default: clean build test

build: deps quick

quick:
	npm run pulp -- build

watch:
	npm run pulp -- --watch build

test:
	npm run pulp -- test

test-watch:
	npm run pulp -- --watch test

deps:
	npm install
	npm run bower -- i

clean:
	rm -rf output
	rm -rf bower_components
