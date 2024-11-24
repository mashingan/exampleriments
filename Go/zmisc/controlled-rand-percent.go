package main

import (
	"fmt"
	"math/rand"
)

func main() {
	//rand.Seed(time.Now().Unix())
	var countAyam, countSapi, countKambing int
	for i := 0; i < 1000; i++ {
		prob := rand.Float32()
		if prob <= 0.5 {
			countAyam++
		} else if prob > 0.5 && prob <= 0.8 {
			countSapi++
		} else {
			countKambing++
		}
	}
	fmt.Printf("lauk ayam percent %f%%, sapi percent %f%%, kambing percent %f%%",
		float32(countAyam)/10.0,
		float32(countSapi)/10.0,
		float32(countKambing)/10.0)
}
