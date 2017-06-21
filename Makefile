BASEDIR = $(shell pwd)
REBAR = rebar3
APPNAME = xaptum_client
RELPATH = _build/default/rel/$(APPNAME)
BUILDDIR = _build
APPVSN = 1.0
SHELL = /bin/bash
PRIVDIR = $(BASEDIR)/priv
IDF = $(PRIVDIR)/build_id
ID_CMD = echo "`date +'%y-%m-%d %H:%M:%S'`" > $(IDF)
DEBUG=1

compile:
	$(REBAR) compile

recompile:
	find . -name ebin | xargs rm -rf
	$(REBAR) compile

ct-run:
	ct_run -dir $(BASEDIR)/ct -logdir /var/log/xaptum/$(APPNAME)/ct/logs \
	-pa $(BASEDIR)/_build/test/rel/$(APPNAME)/lib/*/ebin -erl_args \
	-config $(BASEDIR)/_build/test/rel/$(APPNAME)/releases/$(APPVSN)/sys.config

test-release: release
	$(REBAR) as test release

test: recompile test-release ct-run

privdir:
	mkdir -p $(PRIVDIR)

id: privdir
	$(ID_CMD)

release: id test
	$(REBAR) release

console: release
	cd $(RELPATH) && ./bin/$(APPNAME) console

start: release
	cd $(RELPATH) && ./bin/$(APPNAME) start

device-release: id
	$(REBAR) as device release

device-console: device-release
	$(BASEDIR)/_build/device/rel/$(APPNAME)/bin/$(APPNAME) console

device: device-release
	$(BASEDIR)/_build/device/rel/$(APPNAME)/bin/$(APPNAME) start
	    
subscriber-release: id
	$(REBAR) as subscriber release
	
subscriber-console: subscriber-release
	$(BASEDIR)/_build/subscriber/rel/$(APPNAME)/bin/$(APPNAME) console

subscriber: subscriber-release
	$(BASEDIR)/_build/subscriber/rel/$(APPNAME)/bin/$(APPNAME) start

gateway-release: id
	$(REBAR) as gateway release

gateway-console: gateway-release
	$(BASEDIR)/_build/gateway/rel/$(APPNAME)/bin/$(APPNAME) console

gateway: gateway-release
	$(BASEDIR)/_build/gateway/rel/$(APPNAME)/bin/$(APPNAME) start


dialyzer: test
	$(REBAR) dialyzer

clean:
	$(REBAR) clean
	rm -rf _build

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach
