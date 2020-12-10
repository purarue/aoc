run: base.sql query.sql
	cat base.sql query.sql >main.sql
	sudo mysql -u root <./main.sql
base.sql: main.sh input.txt
	./main.sh ./input.txt
