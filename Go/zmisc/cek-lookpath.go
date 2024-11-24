package main

import (
    "log"
    "os/exec"
)

func main() {
    path, err := exec.LookPath("sqlite3")
    if err != nil {
	log.Fatal(err)
    }
    log.Println(path)
}
