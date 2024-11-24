package main

import (
	"database/sql"
	"database/sql/driver"
	"fmt"
	"time"

	"github.com/mashingan/smapping"
)

type customNullInt struct {
	Int   int
	Valid bool
}

func (cint *customNullInt) Scan(src interface{}) error {
	if src == nil {
		cint.Valid = false
		return nil
	}
	valint, ok := src.(int)
	if !ok {
		cint.Valid = false
		return fmt.Errorf("sql scan: Failed converting %v to int", src)
	}
	cint.Int = valint
	cint.Valid = true
	return nil
}

func (c customNullInt) Value() (driver.Value, error) {
	if !c.Valid {
		return nil, fmt.Errorf("Nil value of customNullInt")
	}
	return c.Int, nil
}

type dummyValues struct {
	Int     int
	Int8    int8
	Int16   int16
	Int32   int32
	Int64   int64
	Uint    uint
	Uint8   uint8
	Uint16  uint16
	Uint32  uint32
	Uint64  uint64
	Float32 float32
	Float64 float64
	Bool    bool
	String  string
	Bytes   []byte
	sql.NullBool
	sql.NullFloat64
	sql.NullInt32
	sql.NullInt64
	sql.NullString
	sql.NullTime
	IntNull customNullInt
}

type dummyRow struct {
	Values dummyValues
}

func (dr *dummyRow) Scan(dest ...interface{}) error {
	fmt.Println("dest:", dest)
	for i, x := range dest {
		switch x.(type) {
		case *int:
			dest[i] = &dr.Values.Int
		case *int8:
			dest[i] = &dr.Values.Int8
		case *int16:
			dest[i] = &dr.Values.Int16
		case *int32:
			dest[i] = &dr.Values.Int32
		case *int64:
			dest[i] = &dr.Values.Int64
		case *uint:
			dest[i] = &dr.Values.Uint
		case *uint8:
			dest[i] = &dr.Values.Uint8
		case *uint16:
			dest[i] = &dr.Values.Uint16
		case *uint32:
			dest[i] = &dr.Values.Uint32
		case *uint64:
			dest[i] = &dr.Values.Uint64
		case *float32:
			dest[i] = &dr.Values.Float32
		case *float64:
			dest[i] = &dr.Values.Float64
		case *string:
			dest[i] = &dr.Values.String
		case *[]byte:
			dest[i] = &dr.Values.Bytes
		case *bool:
			dest[i] = &dr.Values.Bool
		case sql.Scanner:
			switch x.(type) {
			case *sql.NullBool:
				//dest[i] = &dr.Values.NullBool
				(dest[i]).(sql.Scanner).Scan(dr.Values.NullBool.Bool)
			case *sql.NullFloat64:
				//dest[i] = &dr.Values.NullFloat64
				(dest[i]).(sql.Scanner).Scan(nil)
			case *sql.NullInt32:
				dest[i] = &dr.Values.NullInt32
			case *sql.NullInt64:
				dest[i] = &dr.Values.NullInt64
			case *sql.NullString:
				dest[i] = &dr.Values.NullString
			case *sql.NullTime:
				dest[i] = &dr.Values.NullTime
			default:
				fmt.Println("got sql scanner")
				(dest[i]).(sql.Scanner).Scan(5)
			}
		}
	}
	return nil
}

func createDummyRow(destTime time.Time) *dummyRow {
	return &dummyRow{
		Values: dummyValues{
			Int:         -5,
			Int8:        -4,
			Int16:       -3,
			Int32:       -2,
			Int64:       -1,
			Uint:        1,
			Uint8:       2,
			Uint16:      3,
			Uint32:      4,
			Uint64:      5,
			Float32:     42.1,
			Float64:     42.2,
			Bool:        true,
			String:      "hello 異世界",
			Bytes:       []byte("hello 異世界"),
			NullBool:    sql.NullBool{Bool: true, Valid: true},
			NullFloat64: sql.NullFloat64{Float64: 42.2, Valid: true},
			NullInt32:   sql.NullInt32{Int32: 421, Valid: true},
			NullInt64:   sql.NullInt64{Int64: 422, Valid: true},
			NullString:  sql.NullString{String: "hello 異世界", Valid: true},
			NullTime:    sql.NullTime{Time: destTime, Valid: true},
		},
	}
}

func main() {
	currtime := time.Now()
	dr := createDummyRow(currtime)
	result := dummyValues{}
	fmt.Println("dr:", *dr)
	if err := smapping.SQLScan(dr, &result, "",
		"Int", "Int8", "Int32", "Int64",
		"Uint", "Uint8", "Uint32", "Uint64",
		"Float32", "Float64",
		"Bool",
		"String", "Bytes",
		"NullBool", "NullFloat64", "NullInt32", "NullInt64",
		"NullString", "NullTime"); err != nil {
		fmt.Println("Error happened!", err)
		return
	}
	fmt.Println("NullString is Valid?", result.NullString.Valid)
	fmt.Println("result:", result)
	res2 := dummyValues{}
	if err := smapping.SQLScan(dr, &res2, ""); err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println("res2:", res2)
}
