test : c-part
	ct_run -logdir logs -suite async_ports_SUITE

c-part : async_ports_SUITE_data/cport.c
	gcc -o async_ports_SUITE_data/cport async_ports_SUITE_data/cport.c
