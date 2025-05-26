package main

import (
	"bytes"
	_ "embed"
	"flag"
	"io"
	"log"
	"sync"
	"time"

	"github.com/gopxl/beep"
	"github.com/gopxl/beep/mp3"
	"github.com/gopxl/beep/speaker"
)

//go:embed cycle.mp3
var cyclebyte []byte

//go:embed start.mp3
var startbyte []byte

//go:embed end.mp3
var endbyte []byte

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile | log.Lmicroseconds)
	var (
		cycle int
		dur   string
	)
	flag.IntVar(&cycle, "cycle", 8, "-cycle for each cycle")
	flag.StringVar(&dur, "dur", "32s", "-dur duration to run, default 32s")
	flag.Parse()
	totaldur, err := time.ParseDuration(dur)
	if err != nil {
		log.Fatal(err)
	}
	var (
		fformat, startf, endf beep.Format
		wg                    sync.WaitGroup
		bcycle                *beep.Buffer
		bstart, bend          beep.StreamCloser
	)
	wg.Add(3)
	go func(w *sync.WaitGroup) {
		defer w.Done()
		bcycle, fformat = openFile(cyclebyte)
	}(&wg)
	go func(w *sync.WaitGroup) {
		defer w.Done()
		var err error
		bstart, startf, err = mp3.Decode(io.NopCloser(bytes.NewBuffer(startbyte)))
		if err != nil {
			log.Fatal(err)
		}
	}(&wg)
	go func(w *sync.WaitGroup) {
		defer w.Done()
		var err error
		bend, endf, err = mp3.Decode(io.NopCloser(bytes.NewBuffer(endbyte)))
		if err != nil {
			log.Fatal(err)
		}
	}(&wg)
	wg.Wait()
	defer bstart.Close()
	defer bend.Close()
	speaker.Init(fformat.SampleRate, fformat.SampleRate.N(time.Second/10))
	speaker.Init(startf.SampleRate, startf.SampleRate.N(time.Second/10))
	speaker.Init(endf.SampleRate, endf.SampleRate.N(time.Second/10))
	startok := make(chan struct{}, 1)
	time.Sleep(time.Second * 10)
	speaker.Play(beep.Seq(bstart, beep.Callback(func() {
		startok <- struct{}{}
	})))
	<-startok
	log.Println("start")
	counts := 0
	start := time.Now()
	done := make(chan struct{}, 1)
	for tt := range time.Tick(time.Second * time.Duration(cycle)) {
		sec := tt.Sub(start).Seconds()
		log.Println(sec, "seconds")
		if sec > totaldur.Seconds() && (counts+1)%4 == 0 {
			break
		}
		counts++
		speaker.Play(beep.Seq(bcycle.Streamer(0, fformat.SampleRate.N(time.Second)/2), beep.Callback(func() {
			done <- struct{}{}
		})))
		<-done
	}
	log.Println("end")
	speaker.Play(beep.Seq(bend, beep.Callback(func() {
		startok <- struct{}{}
	})))
	<-startok
	log.Println("music")
}

func openFile(stream []byte) (*beep.Buffer, beep.Format) {
	streamer, format, err := mp3.Decode(io.NopCloser(bytes.NewBuffer(stream)))
	if err != nil {
		log.Fatal(err)
	}
	b := beep.NewBuffer(format)
	b.Append(streamer)
	streamer.Close()
	return b, format

}
