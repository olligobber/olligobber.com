#let inline-svg(it) = html.elem("div", attrs: (class:"inline-typst"), html.frame(it))

#let block-svg(it) = html.elem("div", attrs: (class:"block-typst"), html.frame(it))

#let parbreak() = html.elem("div", [~])

#let styles(doc) = [
	#set text(fill: rgb(0x33,0x33,0x33), size: 1.7em);
	#doc
]