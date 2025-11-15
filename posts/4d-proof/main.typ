// #import "template.typ" : styles, block-svg, inline-svg, parbreak
#import "@preview/cetz:0.4.1": canvas, draw

#let block-svg(x) = align(center, x)

// #show: styles

#let step-shape(tl: (0,5), br: (5,0), dimensions: $n$, left-gutter: 0.5, bottom-gutter: 0.5) = {
	import draw: line, content

	let (left, top) = tl
	let (right, bottom) = br
	let dx = (right - left)/5
	let dy = (bottom - top)/5

	line(tl, (left, bottom), br)

	for i in (0,1,3,4) {
		line(
			(left + i * dx, top + i * dy),
			(left + (i + 1) * dx, top + i * dy),
			(left + (i + 1) * dx, top + (i + 1) * dy)
		)
	}
	line(
		(left + 2 * dx, top + 2 * dy),
		(left + 3 * dx, top + 3 * dy),
		stroke: (dash: "dotted")
	)

	line(
		(left - dx * left-gutter, top),
		(left - dx * left-gutter, bottom),
		mark: (end: "stealth", start: "stealth", fill: black)
	)

	content(
		(left - dx * left-gutter, (top + bottom) / 2),
		box(fill: white, inset: 0.3em, dimensions)
	)

	line(
		(left, bottom + dy * bottom-gutter),
		(right, bottom + dy * bottom-gutter),
		mark: (end: "stealth", start: "stealth", fill: black)
	)

	content(
		((left + right) / 2, bottom + dy * bottom-gutter),
		box(fill: white, inset: 0.3em, dimensions)
	)
}

I stumbled across a fun numerical result.

$ (sum_(i=1)^n i)^2 = sum_(i=1)^n i^3 $

I found this because of a user on #link("https://mathstodon.xyz")[mathstodon.xyz], #link("https://mathstodon.xyz/@SvenGeier")[\@SvenGeier], who has the display name "Σ(i³)~=~(Σi)²". It's a nice result, which can be proved fairly easily by induction and a little bit of algebra. What interested me though was finding a deeper understanding of why this result works.

As an example of what I mean, let's turn to the slightly simpler result,

$ sum_(i=1)^n i = (n(n+1))/2, $

and see if we can understand it on a deeper level. This result is about what happens when you add up $i$ as $i$ varies from $1$ to $n$. We can graph this sum by having $n$ rows, one for each value of $i$, and putting $i$ boxes in each row.

#block-svg(table(
	rows: 2em,
	columns: (auto,) + (2em,) * 4,
	stroke: (x,y) => {
		if x == 0 { return none }
		if x - 1 <= y { return black }
	},
	align: center+horizon,
	..(
		($i=1$,),
		([],) * 4,
		($i=2$,),
		([],) * 4,
		($dots.v$, [], [], $dots.down$, []),
		($i=n$, [], [], $dots$, []),
	).join()
))

The total summation is then the total number of boxes, or if we say each box is $1 times 1$, the total area of this shape:

#block-svg(canvas(step-shape()))