package main

import (
	"fmt"
	"log"
	"os"
	"time"

	"gioui.org/app"
	"gioui.org/io/event"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/unit"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/explorer"
	"gioui.org/x/notify"
)

const (
	updateWindowDuration = 10 * time.Millisecond
	progressStep         = 0.01
)

func runGui() {
	go func() {
		window := new(app.Window)
		window.Option(
			app.Title("Clean epub (remove zh)"),
			app.Size(unit.Dp(400), unit.Dp(300)),
		)
		if err := run(window); err != nil {
			log.Fatal(err)
		}
		os.Exit(0)

	}()
	app.Main()
}

type pageState struct {
	choosingFile, cleaning bool
	cleaningDone           bool
	fileInfoLabelRatio     float32
	cleaningProgress       float32
	filename               string
	cleaningInfo           string
	choseFile, cleanBtn    widget.Clickable
	notify.Notifier
}

func run(w *app.Window) error {
	expl := explorer.NewExplorer(w)
	theme := material.NewTheme()
	var (
		err error
		ps  pageState
		ops op.Ops
	)
	ps.Notifier, err = notify.NewNotifier()
	if err != nil {
		log.Println("no notifier:", err)
		ps.Notifier = nil
	}
	events := make(chan event.Event)
	acks := make(chan struct{})
	go func() {
		for {
			ev := w.Event()
			events <- ev
			<-acks
			if _, ok := ev.(app.DestroyEvent); ok {
				return
			}
		}
	}()
	for {
		select {
		case e := <-events:
			switch e := e.(type) {
			case app.DestroyEvent:
				acks <- struct{}{}
				return e.Err
			case app.FrameEvent:
				ops.Reset()
				gtx := app.NewContext(&ops, e)
				if ps.choseFile.Clicked(gtx) && !ps.choosingFile {
					go choosingFile(expl, &ps)
				}
				if ps.cleanBtn.Clicked(gtx) && !ps.cleaning && ps.filename != "" {
					go cleaningOnprogress(&ps)
				}
				cleanPage(gtx, theme, &ps)
				e.Frame(gtx.Ops)
			}
			acks <- struct{}{}
		case <-time.Tick(updateWindowDuration):
			w.Invalidate()
		}
	}
}

func cleanPage(gtx layout.Context, th *material.Theme, ps *pageState) layout.Dimensions {
	info := layout.Rigid(material.H6(th, ps.cleaningInfo).Layout)
	if ps.cleaning {
		info = layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{}.Layout(gtx,
				layout.Rigid(material.H6(th, "Cleaning").Layout),
				layout.Rigid(material.ProgressBar(th, ps.cleaningProgress).Layout),
			)
		})
	}
	return layout.Inset{
		Left: unit.Dp(30), Right: unit.Dp(30), Top: unit.Dp(15), Bottom: unit.Dp(15),
	}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
			layout.Rigid(material.Button(th, &ps.choseFile, "Choose epub").Layout),
			layout.Rigid(layout.Spacer{Height: unit.Dp(10)}.Layout),
			layout.Rigid(material.H6(th, ps.filename).Layout),
			layout.Rigid(layout.Spacer{Height: unit.Dp(10)}.Layout),
			layout.Rigid(material.Button(th, &ps.cleanBtn, "Clean").Layout),
			layout.Rigid(layout.Spacer{Height: unit.Dp(10)}.Layout),
			info,
		)
	})
}

func choosingFile(expl *explorer.Explorer, st *pageState) {
	st.choosingFile = true
	st.cleaningDone = false
	st.cleaningInfo = ""
	st.filename = ""
	defer func(st *pageState) {
		st.choosingFile = false
	}(st)
	ff, err := expl.ChooseFile("epub")
	if err != nil {
		log.Println(err)
		return
	}
	defer ff.Close()
	tf, ok := ff.(*os.File)
	if !ok {
		log.Println("not file")
		return
	}
	st.filename = tf.Name()
	st.fileInfoLabelRatio = 0.2
}

func cleaningOnprogress(st *pageState) {
	st.cleaning = true
	defer func(st *pageState) {
		st.cleaning = false
	}(st)
	done := make(chan error, 1)
	go func() {
		err := clean(st.filename)
		if err != nil {
			done <- err
			st.cleaningInfo = fmt.Sprintf("Error: %v", err)
			go st.CreateNotification("Error", st.cleaningInfo)
			return
		}
		done <- nil
	}()
	for {
		select {
		case err := <-done:
			if err == nil {
				st.cleaningInfo = "success"
				go st.CreateNotification("Success", fmt.Sprintf("Cleaning file %s succeed", st.filename))
			}
			st.cleaningDone = true
			st.cleaningProgress = 1
			return
		case <-time.Tick(updateWindowDuration):
			if st.cleaningProgress >= 1 {
				st.cleaningProgress = 0
			} else {
				st.cleaningProgress += progressStep
			}
		}
	}
}
