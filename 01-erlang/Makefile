run: clean main.beam
	erl -noshell -s main main input.txt -s init stop
main.beam:
	erlc main.erl
clean:
	rm -f main.beam erl_crash.dump
