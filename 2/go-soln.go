package main

import (
    "fmt"
    "io"
    "bufio"
    "os"
    "log"
    "strings"
    "strconv"
)

func openFile(name string) *os.File {
    f, err := os.Open(name)

    if err != nil {
        log.Fatalf("Cannot open input file.")
    }

    return f
}

func parseDimension(s string) (l int, w int, h int) {
    p := strings.Split(strings.TrimSpace(s), "x")

    l, _ = strconv.Atoi(p[0])
    w, _ = strconv.Atoi(p[1])
    h, _ = strconv.Atoi(p[2])

    return
}

func calculateTotal(l, w, h int) int {
    s1, s2, s3 := (l * w), (w * h), (h * l) 

    s := findSlack(s1, s2, s3)

    return (2 * s1) + (2 * s2) + (2 * s3) + (s)
}

func findSlack(nums ...int) int {
    if len(nums) == 1 {
        return nums[0]
    }

    s := nums[0]

    for _, n := range nums[1:] {
        if n < s {
            s = n
        }
    }

    return s
}

func main() {
    file := openFile("./input.txt")
    defer file.Close()

    reader := bufio.NewReader(file)

    total := 0

    for {
        line, err := reader.ReadString('\n')

        if line != "" {
        l, w, h := parseDimension(line)

        total += calculateTotal(l, w, h)

        if err == io.EOF {
            break
        }
    }

    fmt.Printf("Elves should order %d square feet of wrapping paper.\n", total)
}
