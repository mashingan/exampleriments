package main

import (
	"fmt"
	"log"
	"maps"
	"math/rand"
	"strings"
	"time"

	"github.com/oklog/ulid/v2"
)

type Warehouse struct {
	Id, Name    string
	inventories map[string]Inventory
	capacities  map[string]uint64
}

func NewWarehouse(name string) *Warehouse {
	return &Warehouse{
		Id:          ulid.Make().String(),
		Name:        name,
		inventories: map[string]Inventory{},
		capacities:  map[string]uint64{},
	}
}

func NewWarehouseInventory(name string, inv map[string]Inventory) *Warehouse {
	w := &Warehouse{
		Id:          ulid.Make().String(),
		Name:        name,
		inventories: inv,
		capacities:  map[string]uint64{},
	}
	return w
}

func NewWarehouseCapacities(name string, caps map[string]uint64) *Warehouse {
	w := &Warehouse{
		Id:          ulid.Make().String(),
		Name:        name,
		inventories: map[string]Inventory{},
		capacities:  caps,
	}
	return w
}

func (w *Warehouse) AddInventory(itemName string, inv Inventory) {
	ext, ok := w.inventories[itemName]
	if ok {
		ext.Qty += inv.Qty
		w.inventories[itemName] = ext
		return
	}
	w.inventories[itemName] = inv
}

func (w *Warehouse) TakeInventory(itemName string, qty uint64) (uint64, error) {
	ext, ok := w.inventories[itemName]
	if ok {
		ext.Qty = min(ext.Qty-qty, 0)
		w.inventories[itemName] = ext
		return ext.Qty, nil
	}
	return 0, fmt.Errorf("item not available: [%s]", itemName)
}

func (w *Warehouse) AddInventories(inv map[string]Inventory) {
	maps.Copy(w.inventories, inv)
}

func (w *Warehouse) TakeInventories(list map[string]uint64) map[string]error {
	errs := map[string]error{}
	for itemName, qty := range list {
		_, err := w.TakeInventory(itemName, qty)
		if err != nil {
			errs[itemName] = err
		}
	}
	return errs
}

func (w *Warehouse) Cap(unit string) (uint64, error) {
	cp, ok := w.capacities[unit]
	if !ok {
		return 0, fmt.Errorf("not supported capacities: [%s]", unit)
	}
	return cp, nil
}

func (w *Warehouse) AddCapacities(caps map[string]uint64) {
	maps.Copy(w.capacities, caps)
}

type Inventory struct {
	Item
	Qty uint64
}

func (i Inventory) String() string {
	return fmt.Sprintf("%s\nQty: %d", i.Item, i.Qty)
}

type Item struct {
	Id, Name, Unit string
	Infos          map[string]Fields
}

func (i Item) String() string {
	info := fmt.Sprintf(`Id: %s
Name: %s,
Unit: %s,
==========
Additional Tags:
`, i.Id, i.Name, i.Unit)
	bld := &strings.Builder{}
	bld.WriteString(info)
	for k, v := range i.Infos {
		fmt.Fprintf(bld, "%s: type: %s, default-value: %s, filename(optional): %s\n",
			k, v.FieldType, v.Value, v.filename)

	}
	info += "=========="
	return bld.String()
}

type FieldType uint8

const (
	FieldAlpha FieldType = iota
	FieldDate
	FieldNumeric
	FieldDocument
	FieldImage
)

func (f FieldType) String() string {
	fieldsInfo := [...]string{
		"Alphanumeric", "Datetime", "Numeric", "Document", "Image",
	}
	return fieldsInfo[f]
}

type Fields struct {
	FieldType
	Key string

	// id file for documents and images
	// parseable int for number
	// parseable date for date
	Value string

	filename string // if only FieldDocument and FieldImage
}

type ItemInOut uint8

const (
	ItemIn ItemInOut = iota
	ItemOut
)

type Activity struct {
	flowitem      ItemInOut
	inventories   []Inventory
	id, warehouse string // warehouse using id
	datetime      time.Time
}

func (a Activity) String() string {
	bld := &strings.Builder{}
	itemsflow := "items in"
	if a.flowitem == ItemOut {
		itemsflow = "items out"
	}

	bld.WriteString("======start-activity========\n")
	fmt.Fprintf(bld, "At time %s, there's %s with list:\n",
		a.datetime.Format("Monday 15:04:05+07 02-Jan-2006"),
		itemsflow)
	for _, inv := range a.inventories {
		fmt.Fprint(bld, inv)
	}
	bld.WriteString("\n========end-activity========")
	return bld.String()
}

func main() {
	log.SetFlags(log.LstdFlags | log.Lshortfile)
	w := NewWarehouseCapacities("test-gudang", map[string]uint64{
		"pcs": 5_000,
	})
	item := Item{
		Id:   ulid.Make().String(),
		Name: "onderdil",
		Unit: "pcs",
		Infos: map[string]Fields{
			"part-code":       {FieldAlpha, "part-code", "ab333L", ""},
			"production-year": {FieldNumeric, "production-year", "2020", ""},
			"gen":             {FieldAlpha, "gen", "1Q2", ""},
		},
	}
	target, _ := w.Cap("pcs")
	sentInv := uint64(0)
	daysMax := 60
	for sentInv < target {
		toSent := max(rand.Intn(int(target-sentInv+1)), 0)
		log.Println("toSent:", toSent)
		if toSent == 0 {
			break
		}
		inv := Inventory{item, uint64(toSent)}
		sentInv += uint64(toSent)
		tosub := max(rand.Intn(daysMax), daysMax/2)
		daysMax -= tosub
		thedate := time.Now().Add(24 * time.Hour * -1 * time.Duration(tosub))
		act := Activity{ItemIn, []Inventory{inv}, ulid.Make().String(), w.Id, thedate}
		w.AddInventory(item.Id, inv)
		log.Println("activity:\n", act)
	}
	fmt.Printf("%#v\n", *w)
	fmt.Println(w)

}
