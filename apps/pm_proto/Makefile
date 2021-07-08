THRIFT ?= thrift
GEN := "erlang:scoped_typenames,app_prefix=dmsl"

src/dmsl_%_thrift.erl src/dmsl_%_thrift.hrl: proto/%.thrift
	$(THRIFT) --gen $(GEN) -I $(REBAR_DEPS_DIR) --out src/ $^

include/dmsl_%_thrift.hrl: src/dmsl_%_thrift.hrl
	mv $^ $@

clean:
	rm -vf src/dmsl_party_state_thrift.?rl include/dmsl_party_state_thrift.hrl
