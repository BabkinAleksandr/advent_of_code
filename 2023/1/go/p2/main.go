package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
	"unicode"
)

const CORRECT_ANSWER = 54885

func main() {
	reader := bufio.NewReader(os.Stdin)
	sum := 0
	for {
		line, err := reader.ReadString('\n')
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Println("Something hapenned", err)
			os.Exit(1)
		}
		sum += parseLine(makeSubstitutes(line))
	}

	if sum != CORRECT_ANSWER {
		panic(fmt.Sprintf("Answer is incorrect: %d", sum))
	}

	fmt.Println("\n/2023/d1/P2 result:", sum)
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

type Substitute struct {
	source string
	target string
}

var SUBSTITUTES = []Substitute{
	{source: "one", target: "o1e"},
	{source: "two", target: "t2o"},
	{source: "three", target: "th3ee"},
	{source: "four", target: "f4ur"},
	{source: "five", target: "f5ve"},
	{source: "six", target: "s6x"},
	{source: "seven", target: "se7en"},
	{source: "eight", target: "ei8ht"},
	{source: "nine", target: "n9ne"},
}

func makeSubstitutes(line string) string {
	var buf string = line
	for _, sub := range SUBSTITUTES {
		buf = strings.ReplaceAll(buf, sub.source, sub.target)
	}
	return buf
}
