package wareware

import (
	"fmt"
	"strings"
	"time"

	"github.com/oklog/ulid/v2"
)

type Activity struct {
	flowitem      ItemInOut
	inventories   []Inventory
	id, warehouse string // warehouse using id
	datetime      time.Time
	User
}

func NewActivity(f ItemInOut, inv []Inventory, warehouse string, user User) *Activity {
	return &Activity{
		flowitem:    f,
		inventories: inv,
		id:          ulid.Make().String(),
		warehouse:   warehouse,
		datetime:    time.Now(),
		User:        user,
	}
}

func (a *Activity) SetDatetime(dt time.Time) {
	a.datetime = dt
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
