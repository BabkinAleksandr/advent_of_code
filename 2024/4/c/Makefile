INPUT = input.txt

run: run1 run2

run1: p1
	./p1 < $(INPUT) && rm ./p1

run2: p2
	./p2 < $(INPUT) && rm ./p2

%: %.c
	gcc $< -o $@
