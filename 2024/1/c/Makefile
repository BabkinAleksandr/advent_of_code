INPUT = input.txt

run: run1 run2

run1: p1
	./p1 < $(INPUT) && rm ./p1

run2: p2
	./p2 < $(INPUT) && rm ./p2

p1:
	gcc p1.c -o p1

p2:
	gcc p2.c hashmap.c -o p2
