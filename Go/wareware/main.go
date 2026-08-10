package main

import (
	"fmt"
	"maps"
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
	return &Warehouse{
		Id:          ulid.Make().String(),
		Name:        name,
		inventories: inv,
		capacities:  map[string]uint64{},
	}
}

func NewWarehouseCapacities(name string, caps map[string]uint64) *Warehouse {
	return &Warehouse{
		Id:          ulid.Make().String(),
		Name:        name,
		inventories: map[string]Inventory{},
		capacities:  caps,
	}
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

func (w *Warehouse) AddCapacities(caps map[string]uint64) {
	maps.Copy(w.capacities, caps)
}

type Inventory struct {
	Item
	Qty uint64
}

type Item struct {
	Id, Name, Unit string
	Infos          map[string]Fields
}

type FieldType uint8

const (
	FieldAlpha FieldType = iota
	FieldDate
	FieldNumeric
	FieldDocument
	FieldImage
)

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

func main() {
}
