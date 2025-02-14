package discloser

import (
	icon "gio.test/experiments/components/icons"
	"gio.test/experiments/components/pages"
	"gioui.org/layout"
	"gioui.org/unit"
	"gioui.org/widget"
	"gioui.org/widget/material"
	"gioui.org/x/component"
)

type TreeNode struct {
	Text     string
	Children []TreeNode
	component.DiscloserState
}

type Page struct {
	TreeNode
	widget.List
	*pages.Router
	CustomDiscloserState component.DiscloserState
}

func New(r *pages.Router) *Page {
	return &Page{
		Router: r,
		TreeNode: TreeNode{
			Text: "Expand Me",
			Children: []TreeNode{
				{
					Text: "Disclosers",
					Children: []TreeNode{
						{Text: "... nested arbitrary depths"},
						{
							Text: "There are also types available to customize feel and look of the discloser",
							Children: []TreeNode{
								{
									Text: "• DiscloserStyle lets you provide your own control instead of default triangle used here",
								},
								{
									Text: "• DiscloserArrowStyle lets you alter the presentation of triangle used here, like changing its color, size, left/right anchoring and margin",
								},
							},
						},
					},
				},
			},
		},
	}
}

var _ pages.Pager = &Page{}

func (p *Page) Actions() []component.AppBarAction {
	return []component.AppBarAction{}
}

func (p *Page) Overflow() []component.OverflowAction {
	return []component.OverflowAction{}
}

func (p *Page) NavItem() component.NavItem {
	return component.NavItem{
		Name: "Discloser Features",
		Icon: icon.VisibilityIcon,
	}
}

func (p *Page) LayoutTreeNode(gtx layout.Context, th *material.Theme, tn *TreeNode) layout.Dimensions {
	if len(tn.Children) == 0 {
		return layout.UniformInset(unit.Dp(2)).Layout(gtx,
			material.Body1(th, tn.Text).Layout)
	}
	children := make([]layout.FlexChild, 0, len(tn.Children))
	for i := range tn.Children {
		child := &tn.Children[i]
		children = append(children, layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return p.LayoutTreeNode(gtx, th, child)
		}))
	}
	return component.SimpleDiscloser(th, &tn.DiscloserState).Layout(gtx,
		material.Body1(th, tn.Text).Layout,
		func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Vertical}.Layout(gtx, children...)
		})
}

func (p *Page) LayoutCustomDiscloser(gtx layout.Context, th *material.Theme) layout.Dimensions {
	return component.Discloser(th, &p.CustomDiscloserState).Layout(gtx,
		func(gtx layout.Context) layout.Dimensions {
			var l material.LabelStyle
			l = material.Body1(th, "+")
			if p.CustomDiscloserState.Visible() {
				l.Text = "-"
			}
			l.Font.Typeface = "Ubuntu Mono"
			return layout.UniformInset(unit.Dp(2)).Layout(gtx, l.Layout)
		},
		material.Body1(th, "Custom control").Layout,
		material.Body2(th, "This control only took 9 lines of code").Layout,
	)
}

func (p *Page) Layout(gtx layout.Context, th *material.Theme) layout.Dimensions {
	p.List.Axis = layout.Vertical
	return material.List(th, &p.List).Layout(gtx, 2, func(gtx layout.Context, index int) layout.Dimensions {
		return layout.UniformInset(unit.Dp(4)).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			if index == 0 {
				return p.LayoutTreeNode(gtx, th, &p.TreeNode)
			}
			return p.LayoutCustomDiscloser(gtx, th)
		})
	})
}
