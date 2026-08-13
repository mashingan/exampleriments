package main

import (
	"fmt"
	"log"
	"math/rand"
	"time"

	"wareware"

	"github.com/mashingan/bitfield"
	"github.com/oklog/ulid/v2"
)

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	w := wareware.NewWarehouseCapacities("test-gudang", map[string]uint64{
		"pcs": 5_000,
	})
	const workermax = 5
	workers := make([]wareware.User, 0, workermax)
	for i := range workermax {
		workers = append(workers, wareware.User{
			Roles: bitfield.New(wareware.RoleAddInventory).Value(),
			Id:    ulid.Make().String(),
			Name:  fmt.Sprintf("worker-%d", i+1),
		})
	}
	item := wareware.Item{
		Id:   ulid.Make().String(),
		Name: "onderdil",
		Unit: "pcs",
		Infos: map[string]wareware.Fields{
			"part-code": {
				FieldType: wareware.FieldAlpha,
				Key:       "part-code",
				Value:     "ab333L",
			},
			"production-year": {
				FieldType: wareware.FieldNumeric,
				Key:       "production-year",
				Value:     "2020",
			},
			"gen": {
				FieldType: wareware.FieldAlpha,
				Key:       "gen",
				Value:     "1Q2",
			},
		},
	}
	target, _ := w.Cap("pcs")
	sentInv := uint64(0)
	daysMax := 60
	for sentInv < target {
		if target-sentInv == 0 {
			break
		}
		toSent := max(rand.Intn(int(target-sentInv+1)), 0)
		log.Println("toSent:", toSent)
		inv := wareware.Inventory{item, uint64(toSent)}
		sentInv += uint64(toSent)
		tosub := max(rand.Intn(daysMax), daysMax/2)
		daysMax -= tosub
		thedate := time.Now().Add(24 * time.Hour * -1 * time.Duration(tosub))
		act := wareware.MakeActivity(
			wareware.ItemIn, []wareware.Inventory{inv}, ulid.Make().String(),
			w.Id, thedate, workers[rand.Intn(workermax)])
		w.AddInventory(item.Id, inv)
		log.Println("activity:\n", act)
	}
	fmt.Printf("%#v\n", *w)
	fmt.Println(w)

}
