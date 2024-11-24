package main

import (
	"context"
	"fmt"
	"time"
)

type ctxImpl struct {
	err          error
	nextDeadline time.Time
	mailbox      map[interface{}]interface{}
	done         bool
	isCanceled   bool
}

func (c *ctxImpl) Deadline() (time.Time, bool) {
	return c.nextDeadline, c.nextDeadline.Unix() > 0
}

func (c *ctxImpl) Err() error {
	return c.err
}

func (c *ctxImpl) Value(key interface{}) interface{} {
	return c.mailbox[key]
}

func (c *ctxImpl) Done() <-chan struct{} {
	res := make(chan struct{})
	go func(r chan struct{}) {
		time.Sleep(time.Until(c.nextDeadline))
		r <- struct{}{}
	}(res)
	return res

}
func withDeadline(dur time.Duration) (context.Context, func()) {
	result := &ctxImpl{
		nextDeadline: time.Now().Add(dur),
	}

	go func(r *ctxImpl) {
		time.Sleep(dur)
		r.done = true
	}(result)

	cancelfunc := func() {
		result.isCanceled = true
	}

	return result, cancelfunc
}

func main() {
	fmt.Println("Hello, playground")
	ctx, cancel := withDeadline(time.Second * 1)
	defer cancel()
	start := time.Now()
	select {
	case <-ctx.Done():
		fmt.Println("deadline reached")
	case <-time.After(500 * time.Millisecond):
		fmt.Println("no deadline")
	}
	fmt.Println("time elapsed:", time.Now().Sub(start))

}
