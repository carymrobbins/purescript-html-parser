.PHONY: build test deps clean validate

build: 	validate
	pulp build

test: 	validate
	pulp test

deps: 	validate
	bower i

clean:
	rm -rf output
	rm -rf bower_components

validate:
	tools/validate
