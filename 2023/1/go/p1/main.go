package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"unicode"
)

const CORRECT_ANSWER = 54697

func main() {
	reader := bufio.NewReader(os.Stdin)
	sum := 0
	for {
		line, err := reader.ReadString('\n')
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Println("Something went wrong", err)
			os.Exit(1)
		}
		sum += parseLine(line)
	}

	if sum != CORRECT_ANSWER {
		panic(fmt.Sprintf("Answer is incorrect: %d", sum))
	}

	fmt.Println("\n/2023/d1/P1 result:", sum)
}

func parseLine(line string) int {
	first := -1
	second := -1

	i := 0
	j := len(line) - 1

	for i < len(line) && j >= 0 {
		if first == -1 && unicode.IsDigit(rune(line[i])) {
			first = int(line[i]) - int('0')
		}
		if second == -1 && unicode.IsDigit(rune(line[j])) {
			second = int(line[j]) - int('0')
		}

		i++
		j--
	}

	return first*10 + second
}
