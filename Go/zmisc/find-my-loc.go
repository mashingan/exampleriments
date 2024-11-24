package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	fmt.Println("Hello, playground")
	resp, err := http.Get("https://ifconfig.me/ip")
	if err != nil {
		panic(err)
	}
	ipstr, err := ioutil.ReadAll(resp.Body)
	defer resp.Body.Close()
	fmt.Println(ipstr)
	resp2, err := http.Get(fmt.Sprintf("https://ipinfo.io/%s", ipstr))
	if err != nil {
		panic(err)
	}
	ipinfo, err := ioutil.ReadAll(resp2.Body)
	ipjson := map[string]interface{}{}
	err = json.Unmarshal(ipinfo, &ipjson)
	defer resp2.Body.Close()
	fmt.Println(ipjson)

}
