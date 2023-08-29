runAll:
	find . -mindepth 2 -name Makefile -execdir make \;
clean:
	find . -mindepth 2 -name Makefile -execdir sh -c 'grep -qE "^clean:" Makefile && make clean' \;
