package main

import (
	"context"
	"fmt"
	"io"
	"log/slog"
	"maps"
	"os"
	"path"
	"slices"
	"strings"
	"time"
)

type customL struct {
	w      io.Writer
	level  slog.Level
	attrs  []slog.Attr
	opts   *slog.HandlerOptions
	groups map[string][]slog.Attr
}

func NewL(w io.Writer, opts *slog.HandlerOptions) *customL {
	c := &customL{
		w:      w,
		attrs:  make([]slog.Attr, 0),
		opts:   opts,
		groups: make(map[string][]slog.Attr),
	}
	if opts != nil && opts.Level != nil {
		c.level = opts.Level.Level()
	}
	return c
}

func (c *customL) Enabled(_ context.Context, lvl slog.Level) bool {
	return c.level >= lvl
}

func (c *customL) Handle(_ context.Context, rec slog.Record) error {
	if rec.Level < c.level {
		return nil
	}
	b := new(strings.Builder)
	b.WriteString("|")
	fmt.Fprintf(b, "%s=%s", slog.TimeKey, rec.Time.Format(time.RFC3339))
	b.WriteString("|")
	fmt.Fprintf(b, "%s=%s", slog.LevelKey, rec.Level.String())
	b.WriteString("|")
	fmt.Fprintf(b, "%s=%s", slog.MessageKey, rec.Message)
	b.WriteString("|")
	if c.opts != nil && c.opts.AddSource {
		source := rec.Source()
		if source != nil {
			if c.opts != nil && c.opts.ReplaceAttr != nil {
				attr := slog.Attr{Key: slog.SourceKey, Value: slog.AnyValue(source)}
				attr = c.opts.ReplaceAttr(slices.Collect(maps.Keys(c.groups)), attr)
				newsource, ok := attr.Value.Any().(*slog.Source)
				if ok {
					source = newsource
				}
			}
			fmt.Fprintf(b, "%s=%s:%d", slog.SourceKey, source.File, source.Line)
			b.WriteString("|")
		}
	}
	rec.Attrs(func(a slog.Attr) bool {
		if c.opts != nil && c.opts.ReplaceAttr != nil {
			a = c.opts.ReplaceAttr(slices.Collect(maps.Keys(c.groups)), a)
		}
		fmt.Fprintf(b, "%s|", a.String())
		return true
	})
	fmt.Fprintln(c.w, b.String())
	return nil
}

func (c *customL) WithAttrs(attrs []slog.Attr) slog.Handler {
	c.attrs = append(c.attrs, attrs...)
	return c

}

func (c *customL) WithGroup(name string) slog.Handler {
	return c
}

func main() {
	slog.SetDefault(slog.New(NewL(os.Stdout, &slog.HandlerOptions{
		AddSource: true,
		ReplaceAttr: func(groups []string, a slog.Attr) slog.Attr {
			if a.Key == slog.SourceKey {
				s := a.Value.Any().(*slog.Source)
				s.File = path.Base(s.File)
			}
			return a
		},
	})))
	slog.Info("hello", "key1", "val1", "key2", 2)
}
