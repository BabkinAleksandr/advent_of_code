INPUT = input.txt

run: run1 run2

run1: 
	go run p1/main.go < $(INPUT)

run2: p2
	go run p2/main.go < $(INPUT)

