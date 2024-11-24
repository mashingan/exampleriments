package main

import (
	"fmt"
	"unicode"
)

// Basically like this, we got *lexer to hold our current state and passing
// it around to another rules.
// we can see it as
// ourrule := rule_1 -> rule_2 -> ... -> rule_n
// and its executor is defined in apply function

type lexer struct {
	pos    int
	buffer string
}

type rule func(*lexer) (*lexer, error)

type rules struct {
	*lexer
	combinators []rule
}

// newRules is combining all defined rules in function arguments
func newRules(therule ...rule) rules {
	result := rules{}
	result.lexer = new(lexer)
	result.combinators = make([]rule, len(therule))
	for i, r := range therule {
		result.combinators[i] = r
	}
	return result
}

func errNum(i int) error {
	return fmt.Errorf("error in rule: #%d", i)
}

// apply the rules to the string that we want to parse
func apply(buf string, therules rules) error {
	therules.lexer.pos = 0
	therules.lexer.buffer = buf
	var err error
	for _, r := range therules.combinators {
		therules.lexer, err = r(therules.lexer)
		if err != nil {
			return err
		}
	}
	return err
}

func main() {
	numRule := func(rulepos int) rule {
		return func(lx *lexer) (*lexer, error) {
			err1 := errNum(rulepos)
			if lx == nil {
				return lx, err1
			}
			for lx.pos < len(lx.buffer) {
				c := lx.buffer[lx.pos]
				if !unicode.IsNumber(rune(c)) {
					break
				}
				lx.pos += 1
			}
			return lx, nil
		}
	}

	skipWhitespaces := func(lx *lexer) (*lexer, error) {
		errws := fmt.Errorf("Whitespace error")
		if lx == nil {
			return lx, errws
		}
		for lx.pos < len(lx.buffer) {
			c := lx.buffer[lx.pos]
			if !unicode.IsSpace(rune(c)) {
				break
			}
			lx.pos += 1
		}
		return lx, nil
	}

	addRules := newRules(
		numRule(1),
		skipWhitespaces,
		func(lx *lexer) (*lexer, error) {
			err2 := errNum(2)
			if lx == nil {
				return lx, err2
			}
			if lx.buffer[lx.pos] != '+' {
				return nil, fmt.Errorf(
					"%s: expect '+' at but got '%c' at %d",
					err2.Error(),
					lx.buffer[lx.pos],
					lx.pos)
			}
			lx.pos += 1
			return lx, nil
		},
		skipWhitespaces,
		numRule(2),
	)

	// let's see it in action
	parse1 := "3+5"
	parse2 := "3=5"
	err := apply(parse1, addRules)
	if err != nil {
		fmt.Printf("%s got error: %s\n", parse1, err.Error())
	} else {
		fmt.Printf("%s no problem with rules\n", parse1)
	}
	err = apply(parse2, addRules)
	if err != nil {
		fmt.Printf("%s got error: %s\n", parse2, err.Error())
	} else {
		fmt.Printf("%s no problem with rules\n", parse2)
	}
	parse3 := "10+22"
	err = apply(parse3, addRules)
	if err != nil {
		fmt.Printf("%s got error: %s\n", parse3, err.Error())
	} else {
		fmt.Printf("%s no problem with rules\n", parse3)
	}

	parse4 := "10 + 22"
	err = apply(parse4, addRules)
	if err != nil {
		fmt.Printf("%s got error: %s\n", parse4, err.Error())
	} else {
		fmt.Printf("%s no problem with rules\n", parse4)
	}
}
