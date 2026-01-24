#let inline-svg(it) = context {
	if target() == "html" {
		html.elem("div", attrs: (class:"inline-typst"), html.frame(it))
	} else {
		it
	}
}

#let block-svg(it) = context {
	if target() == "html" {
		html.elem("div", attrs: (class:"block-typst"), html.frame(it))
	} else {
		align(center, it)
	}
}

#let parbreak() = context{
	if target() == "html" {
		html.elem("div", [~])
	} else {
		[~]
	}
}

#let styles(doc) = {
	set text(fill: rgb(0x33,0x33,0x33), size: 1.7em);
	show math.equation.where(block: false): inline-svg
	show math.equation.where(block: true): it => context {
		if target() == "html" {
			block-svg(it)
		} else {
			it
		}
	}
	context{
		if target() == "paged" {
			set page(height: auto, paper: "a2")
			doc
		} else {
			doc
		}
	}
}