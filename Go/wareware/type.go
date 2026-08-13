package wareware

import (
	"fmt"
	"maps"
	"strings"
	"time"

	"github.com/mashingan/bitfield"
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
	User
}

func MakeActivity(f ItemInOut, inv []Inventory, id, warehouse string,
	datetime time.Time, user User) Activity {
	return Activity{f, inv, id, warehouse, datetime, user}
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
	fmt.Fprintf(bld, "\n\twith user:\n%s", a.User)
	bld.WriteString("\n========end-activity========")
	return bld.String()
}

type UserRole uint16

const (
	RoleAdminItem UserRole = iota
	RoleAdminWarehouse
	RoleAddInventory
	RoleTakeInventory
	RolePermitWarehouse
	RoleAddValidator
	RoleTakeValidator
	RoleAddAccepter
	RoleTakeAccepter
)

func (UserRole) Enums() []UserRole {
	return []UserRole{
		RoleAdminItem,
		RoleAdminWarehouse,
		RoleAddInventory,
		RoleTakeInventory,
		RolePermitWarehouse,
		RoleAddValidator,
		RoleTakeValidator,
		RoleAddAccepter,
		RoleTakeAccepter,
	}
}

func (u UserRole) String() string {
	rolestr := map[UserRole]string{
		RoleAdminItem:       "admin-item",
		RoleAdminWarehouse:  "admin-warehouse",
		RoleAddInventory:    "add-to-warehouse",
		RoleTakeInventory:   "take-from-warehouse",
		RolePermitWarehouse: "warehouse-supervisor",
	}
	return rolestr[u]
}

type User struct {
	Roles    uint32 // from bitfield.New[UserRole]().Value()
	Id, Name string
}

func (u User) String() string {
	// center := func(t string) string {
	// 	return strings.Repeat(" ", 10-(len(t)+1)/2) + t + strings.Repeat(" ", 10-(len(t)+1)/2)
	// }
	return fmt.Sprintf(`
%-10s: %26s
%-10s: %26s
%-10s: %v`,
		"ID", u.Id,
		"Name", u.Name,
		"Role", bitfield.From[UserRole](u.Roles).Sets(),
	)
}

type DocType uint8

const (
	GoodsReceipt DocType = iota
	DeliveryNote
	Adjustment
)

type DocProgress uint8

const (
	ReceiptDraft DocProgress = iota
	ReceiptValidate
	ReceiptAccepted
	ReceiptCancelled
)

type Receipt struct {
	Type     DocType
	Progress DocProgress
}
