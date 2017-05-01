.PHONY: clean all run

all: clean compile run

clean:
	rm data_* data.* log* ring_state_* ttb_last_config compile_commands.json undefined -Rf

compile:
	rebar3 compile

run:
	rebar3 shell --name test1@127.0.0.1 --config priv/example_limitless.config --apps example_minidb_backend
