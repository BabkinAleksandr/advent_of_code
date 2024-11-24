package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

const CORRECT_ANSWER = 2105

var CUBES_AVAILABLE = Cubes{red: 12, green: 13, blue: 14}

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

	fmt.Println("\n/2023/d2/P1 result:", sum)
}

type Cubes struct {
	red   int
	green int
	blue  int
}
type Game struct {
	id       int
	maxCubes Cubes
}

func (g *Game) isPossible(cubesAvailable *Cubes) bool {
	return g.maxCubes.red <= cubesAvailable.red &&
		g.maxCubes.green <= cubesAvailable.green &&
		g.maxCubes.blue <= cubesAvailable.blue
}

func parseLine(line string) int {
	game := Game{
		id: -1,
		maxCubes: Cubes{
			red:   0,
			green: 0,
			blue:  0,
		},
	}
	splitted := strings.Split(line, ":")
	id, err := strconv.Atoi(splitted[0][5:]) // strip "Game "
	assertErr(err, "Id parsing error")
	game.id = id

	for _, turnStr := range strings.Split(splitted[1], ";") {
		for _, cubePairStr := range strings.Split(strings.TrimSpace(turnStr), ",") {
			cubePair := strings.Split(strings.TrimSpace(cubePairStr), " ")
			count, err := strconv.Atoi(cubePair[0])
			assertErr(err, "Parse cubes count")
			switch cubePair[1] {
			case "red":
				if count > game.maxCubes.red {
					game.maxCubes.red = count
				}
			case "green":
				if count > game.maxCubes.green {
					game.maxCubes.green = count
				}
			case "blue":
				if count > game.maxCubes.blue {
					game.maxCubes.blue = count
				}
			}
		}
	}

	if game.isPossible(&CUBES_AVAILABLE) {
		return game.id
	}
	return 0
}

func assertErr(err error, message string) {
	if err != nil {
		panic(message)
	}
}
