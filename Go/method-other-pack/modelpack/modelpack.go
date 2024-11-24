package modelpack

type MPack struct {
	field1 int
	field2 string
	field3 float64
	field4 bool
}

func New(v int) *MPack {
	return &MPack{field1: v}
}
