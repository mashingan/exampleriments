package main

import (
	"fmt"
	"math/rand"
	"time"
)

type CustomError struct {
	errtype string
	errmsg  string
}

func (e *CustomError) Error() string {
	return e.errmsg
}

func (e *CustomError) IsDbError() bool {
	return e.errtype == "dberror"
}

func (e *CustomError) IsInternalError() bool {
	return e.errtype == "internalerror"
}

func (e *CustomError) IsNotFound() bool {
	return e.errtype == "notfound"
}

func problematicProcess(commonerr error) error {
	if commonerr == nil {
		errtype := ""
		errmsg := ""
		switch rand.Intn(3) {
		case 0:
			errtype = "dberror"
			errmsg = "Some db error happened"

		case 1:
			errtype = "internalerror"
			errmsg = "Internal server error"

		case 2:
			errtype = "notfound"
			errmsg = "Requested query not found"

		default:
			panic("this should not be happened")
		}
		return &CustomError{
			errtype: errtype,
			errmsg:  errmsg,
		}
	}
	return commonerr
}

func errorHandler(therr error) {
	if therr == nil {
		return
	}

	cust, ok := therr.(*CustomError)
	if !ok {
		fmt.Println(therr)
		return
	}
	if cust.IsDbError() {
		fmt.Println("DB error:", cust.Error())
	} else if cust.IsNotFound() {
		fmt.Println("Request error:", cust.Error())
	} else if cust.IsInternalError() {
		fmt.Println("Error:", cust.Error())
	}
	// continue with other kind of error
	fmt.Println("error msg:", cust.Error())

}

func main() {
	rand.Seed(time.Now().UnixNano())
	err := problematicProcess(fmt.Errorf("tnis an everyday error"))
	errorHandler(err)
	err = problematicProcess(nil)
	errorHandler(err)
}
