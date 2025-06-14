package comp

type SymbolScope string

const (
	GlobalScope   SymbolScope = "GLOBAL"
	LocalScope    SymbolScope = "LOCAL"
	BuiltinScope  SymbolScope = "BUILTIN"
	FreeScope     SymbolScope = "FREE"
	FunctionScope SymbolScope = "FUNCTION"
)

type Symbol struct {
	Name  string
	Scope SymbolScope
	Index int
}

type SymbolTable struct {
	store       map[string]Symbol
	numdef      int
	scoped      *SymbolTable
	FreeSymbols []Symbol
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{store: map[string]Symbol{}}
}

func (s *SymbolTable) Define(sym string) Symbol {
	syms := Symbol{sym, GlobalScope, s.numdef}
	if s.scoped != nil {
		syms.Scope = LocalScope
	}
	s.numdef++
	s.store[sym] = syms
	return syms
}

func (s *SymbolTable) DefineBuiltin(index int, sym string) Symbol {
	ss := Symbol{sym, BuiltinScope, index}
	s.store[sym] = ss
	return ss
}

func (s *SymbolTable) ResolveBuiltin(sym string) (Symbol, bool) {
	ss, ok := s.store[sym]
	if !ok {
		return ss, ok
	}
	if ss.Scope != BuiltinScope {
		return ss, false
	}
	return ss, true
}

func (s *SymbolTable) Resolve(sym string) (Symbol, bool) {
	ss, ok := s.store[sym]
	if !ok && s.scoped != nil {
		ss, ok = s.scoped.Resolve(sym)
		if !ok {
			return ss, ok
		}
		if ss.Scope == GlobalScope || ss.Scope == BuiltinScope {
			return ss, ok
		}
		s.FreeSymbols = append(s.FreeSymbols, ss)
		s.store[sym] = ss
		ss.Scope = FreeScope
		ss.Index = len(s.FreeSymbols) - 1
	}
	return ss, ok
}

func NewFrameSymbolTable(outer *SymbolTable) *SymbolTable {
	st := NewSymbolTable()
	st.scoped = outer
	return st
}

func (s *SymbolTable) DefineFunctionName(sym string) Symbol {
	ss := Symbol{sym, FunctionScope, 0}
	s.store[sym] = ss
	return ss
}
