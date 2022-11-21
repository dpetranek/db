.PHONY: all deps jar install deploy nodejs browser webworker cljtest \
        cljs-browser-test cljs-node-test cljstest test eastwood ci clean \
        js-packages publish-nodejs publish-browser publish-webworker publish-js

DOCS_MARKDOWN := $(shell find docs -name '*.md')
DOCS_TARGETS := $(DOCS_MARKDOWN:docs/%.md=docs/%.html)

SOURCES := $(shell find src)
RESOURCES := $(shell find resources)
BROWSER_SOURCES := src-cljs/flureedb.cljs
WEBWORKER_SOURCES := src-cljs/flureeworker.cljs
NODEJS_SOURCES := $(shell find src-nodejs)
ALL_SOURCES := $(SOURCES) $(BROWSER_SOURCES) $(WEBWORKER_SOURCES) $(NODEJS_SOURCES)

all: jar browser nodejs webworker js-packages docs

target/fluree-db.jar: out node_modules src/deps.cljs $(ALL_SOURCES) $(RESOURCES)
	clojure -T:build jar

jar: target/fluree-db.jar

package-lock.json node_modules: package.json
	npm install && touch package-lock.json node_modules

out/flureenjs.js: package.json package-lock.json node_modules build-nodejs.edn deps.edn src/deps.cljs $(SOURCES) $(NODEJS_SOURCES) $(RESOURCES)
	clojure -M:nodejs && cp out/nodejs/flureenjs.js out/flureenjs.js

nodejs: out/flureenjs.js

out/flureedb.js: package.json package-lock.json node_modules build-browser.edn deps.edn src/deps.cljs $(SOURCES) $(BROWSER_SOURCES) $(RESOURCES)
	clojure -M:browser && cp out/browser/main.js out/flureedb.js

browser: out/flureedb.js

out/flureeworker.js: package.json package-lock.json node_modules build-webworker.edn deps.edn src/deps.cljs $(SOURCES) $(WEBWORKER_SOURCES) $(RESOURCES)
	clojure -M:webworker && cp out/webworker/main.js out/flureeworker.js

webworker: out/flureeworker.js

deps:
	clojure -A:cljtest:cljstest:eastwood:docs -P

src/deps.cljs: package.json
	clojure -M:js-deps

install: target/fluree-db.jar
	clojure -T:build install

deploy: target/fluree-db.jar
	clojure -T:build deploy

js-packages/nodejs/flureenjs.js: out/flureenjs.js
	cp $< $@
	bb run sync-package-json $(@D)/package.json --node

js-packages/browser/flureedb.js: out/flureedb.js
	cp $< $@
	bb run sync-package-json $(@D)/package.json

js-packages/webworker/flureeworker.js: out/flureeworker.js
	cp $< $@
	bb run sync-package-json $(@D)/package.json

js-packages: js-packages/nodejs/flureenjs.js js-packages/browser/flureedb.js js-packages/webworker/flureeworker.js

NPM_TAG ?= latest

publish-nodejs: js-packages/nodejs/flureenjs.js
	cd $(<D) && npm publish --tag $(NPM_TAG)

publish-browser: js-packages/browser/flureedb.js
	cd $(<D) && npm publish --tag $(NPM_TAG)

publish-webworker: js-packages/webworker/flureeworker.js
	cd $(<D) && npm publish --tag $(NPM_TAG)

publish-js: publish-nodejs publish-browser publish-webworker

docs/fluree.db.api.html docs/index.html: src/fluree/db/api.clj
	clojure -T:build docs :output-path "\"$(@D)\""

docs/%.html: docs/%.md
	clojure -T:build docs :output-path "\"$(@D)\""

docs: docs/fluree.db.api.html docs/index.html $(DOCS_TARGETS)

cljs-browser-test: node_modules package-lock.json
	rm -rf out/* # prevent circular dependency cljs.core -> cljs.core error
	clojure -M:cljs-browser-test

cljs-node-test: node_modules package-lock.json
	rm -rf out/* # prevent circular dependency cljs.core -> cljs.core error
	clojure -M:cljs-node-test

nodejs-test: out/flureenjs.js
	cd test/nodejs && npm install && npm test

cljstest: cljs-browser-test cljs-node-test

cljtest:
	clojure -M:cljtest

test: cljtest cljstest nodejs-test

eastwood:
	clojure -M:test:eastwood

ci: test eastwood

clean:
	clojure -T:build clean
	rm -rf out/*
	rm -rf docs/*.html
	rm -rf node_modules
