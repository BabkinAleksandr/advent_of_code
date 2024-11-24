INPUT = input.txt

run: run1 run2

run1: p1
	erl -noshell -s p1 main -s init stop < $(INPUT) && rm p1.beam

run2: p2
	erl -noshell -s p2 main -s init stop < $(INPUT) && rm p2.beam

%: %.erl
	erl -compile $<
