package main

import (
	"fmt"
	"time"
)

func isLeapYear(year int) bool {
	if year%100 == 0 {
		return year%400 == 0
	}

	return year%4 == 0
}

func getMonthLastDay(year int, month time.Month) int {
	if month == time.February {
		if isLeapYear(year) {
			return 29
		}
		return 28
	}

	if month%2 == 0 {
		if month >= time.August {
			return 31
		}
		return 30
	} else if month < time.August {
		return 31
	}
	return 30
}

func main() {
	fmt.Println("Hello, playground")
	year := time.Now().Year()
	year = 2200
	fmt.Println("current year:", year)
	for month := time.January; month <= time.December; month++ {
		fmt.Printf("month %s, day: %d\n", month, getMonthLastDay(year, month))
	}
}
