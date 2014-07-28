all : erlang-part c-part

erlang-part : src/porttest.erl
	./rebar get-deps compile escriptize

c-part : src/cport.c
	gcc -o ./cport src/cport.c

clean :
	@rm -rf ebin/*.beam ./cport
