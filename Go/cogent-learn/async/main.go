package main

import (
	"time"

	"cogentcore.org/core/core"
)

func main() {
	b := core.NewBody()
	txt := core.NewText(b)
	txt.Updater(func() {
		txt.SetText(time.Now().Format("15:04:05"))
	})
	go func() {
		ticker := time.NewTicker(time.Second)
		for range ticker.C {
			txt.AsyncLock()
			txt.Update()
			txt.AsyncUnlock()
		}
	}()
	b.RunMainWindow()
}
