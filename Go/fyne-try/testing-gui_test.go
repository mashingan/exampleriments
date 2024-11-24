package main

// to run test:
// go test testing-gui*

import (
	"testing"

	"fyne.io/fyne/v2/test"
)

func TestGreeting(t *testing.T) {
	out, in := makeUI()
	if out.Text != "Hello world" {
		t.Error("incorrect initial greeting")
	}
	test.Type(in, "Andy")
	if out.Text != "Hello Andy" {
		t.Error("incorrect user greeting")
	}
}
