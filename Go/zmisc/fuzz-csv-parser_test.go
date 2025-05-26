package main

import (
	"encoding/csv"
	"strings"
	"testing"
)

func FuzzCSVParser(f *testing.F) {
	testCases := []string{
		"name,age,city\nAlice,25,New York",
		"name;age;city\nBob;30;Los Angeles",
		"name,age,city\nCharlie,invalid_age,Chicago",
		"name,age,city\nDavid,999999999999999999,Unknown",
	}
	for _, tc := range testCases {
		f.Add(tc) // Add initial test cases
	}

	f.Fuzz(func(t *testing.T, input string) {
		reader := csv.NewReader(strings.NewReader(input))
		_, err := reader.ReadAll() // Attempt to parse CSV

		// Log parsing errors without crashing the program
		if err != nil {
			t.Logf("Failed to parse CSV: %v", err)
		}
	})
}
