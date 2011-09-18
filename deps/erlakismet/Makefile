all:
	@test -d ebin || mkdir ebin
	@cp src/erlakismet.app ebin/
	@erl -make

clean:
	@rm -rf ebin/* erl_crash.dump
