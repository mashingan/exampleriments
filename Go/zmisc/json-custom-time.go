package main

import (
	"encoding/json"
	"fmt"
	"log"
	"strings"
	"time"
)

type CustomTime struct {
	t time.Time
}

func (ct CustomTime) MarshalJSON() ([]byte, error) {
	ret := ct.t.Format("2006-01-02 15:04:05")
	return []byte("\"" + ret + "\""), nil
}

func (ct *CustomTime) UnmarshalJSON(b []byte) error {
	str := strings.Trim(string(b), `"`)
	loc, err := time.LoadLocation("Asia/Jakarta")
	if err != nil {
		return fmt.Errorf("CustomTime.UnmarshalJSON: %s", err.Error())
	}
	t, err := time.ParseInLocation("2006-01-02 15:04:05", str, loc)
	if err != nil {
		return fmt.Errorf("CustomTime.UnmarshalJSON: %s", err.Error())
	}
	ct.t = t
	return err
}

func (ct CustomTime) Equal(other CustomTime) bool {
	cty, ctm, ctd := ct.t.Date()
	oty, otm, otd := other.t.Date()
	cth, ctmn, cts := ct.t.Clock()
	oth, otmn, ots := other.t.Clock()
	return cty == oty && ctm == otm && ctd == otd &&
		cth == oth && ctmn == otmn && cts == ots
}

type SentTime struct {
	ActualTime time.Time  `json:"actual_time"`
	ReprTime   CustomTime `json:"repr_time"`
}

func main() {
	log.Println("Hello, playground")
	now := time.Now()
	st := SentTime{
		ActualTime: now,
		ReprTime:   CustomTime{now},
	}
	log.Println(st)
	jsout, err := json.Marshal(&st)
	if err != nil {
		log.Fatal(err)
	}
	log.Println(string(jsout))
	st2 := SentTime{}
	err = json.Unmarshal(jsout, &st2)
	if err != nil {
		log.Fatal(err)
	}
	log.Println(st2)
	if !(st.ActualTime.Equal(st2.ActualTime) &&
		st.ReprTime.Equal(st2.ReprTime)) {
		log.Fatal("The object is not equal for json (un)marshalling")
	} else {
		log.Println("The object is equal")
	}

}
