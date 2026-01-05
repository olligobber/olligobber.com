#import "template.typ" : styles, block-svg, inline-svg, parbreak
#import "@preview/cetz:0.4.1": canvas, draw

// #set page(height: auto)

// #let block-svg(x) = align(center, x)

#show: styles

#let Area = "Area"

#let Volume = "Vol"

#let pad-normal = pad.with(rest: 0.15em)

#let pad-small = pad.with(bottom: 0.3em, rest: 0.15em)

#let pad-op = pad.with(left: 1em, right: 1em)

#let arrow = draw.line.with(mark: (end: "stealth", fill: black))

#let mark-dim(start, end, dim) = {
	draw.content(
		(start, 50%, end),
		dim,
		name: "dim"
	)

	arrow(
		"dim",
		start
	)

	arrow(
		"dim",
		end
	)
}

#let steps(
	tl: (0,3),
	br: (3,0),
) = {
	import draw: line

	let (left, top) = tl
	let (right, bottom) = br
	let dx = (right - left)/5
	let dy = (bottom - top)/5

	for i in (0,1,3,4) {
		line(
			(left + i * dx, top + i * dy),
			(left + (i + 1) * dx, top + i * dy),
			(left + (i + 1) * dx, top + (i + 1) * dy),
		)
	}

	line(
		(left + 2 * dx, top + 2 * dy),
		(left + 3 * dx, top + 3 * dy),
		stroke: (dash: "dotted")
	)
}

#let step-shape(
	tl: (0,3),
	br: (3,0),
	dimensions: pad-small($n$),
	left-gutter: 0.35,
	bottom-gutter: 0.35,
) = {
	import draw: line

	let (left, top) = tl
	let (right, bottom) = br
	let dx = (right - left) / calc.abs(right - left)
	let dy = (bottom - top) / calc.abs(bottom - top)

	line(tl, (left, bottom), br)

	steps(tl: tl, br: br)

	mark-dim(
		(left - dx * left-gutter, top),
		(left - dx * left-gutter, bottom),
		dimensions
	)

	mark-dim(
		(left, bottom + dy * bottom-gutter),
		(right, bottom + dy * bottom-gutter),
		dimensions
	)
}

#let n-1-step-shape = step-shape.with(left-gutter: 0.85, dimensions: pad-small($n-1$))

#let interval(
	tl: (0, 0),
	br: (0, 3),
	dimensions: pad-small($n$),
	gutter: 0.3,
) = {
	import draw: line

	let (left, top) = tl
	let (right, bottom) = br
	let width = right - left
	let height = bottom - top
	let len = calc.sqrt(height * height + width * width)
	let dx = (right - left) / len
	let dy = (bottom - top) / len

	line(tl, br)

	mark-dim(
		(left + dy * gutter, top - dx * gutter),
		(right + dy * gutter, bottom - dx * gutter),
		dimensions
	)
}

#let rectangle(
	tl: (0,3),
	br: (3,0),
	height: pad-small($n$),
	width: pad-small($n$),
	left-gutter: 0.35,
	bottom-gutter: 0.35,
) = {
	import draw: line

	let (left, top) = tl
	let (right, bottom) = br
	let dx = (right - left) / calc.abs(right - left)
	let dy = (bottom - top) / calc.abs(bottom - top)

	line(tl, (left, bottom), br, (right, top), tl)

	mark-dim(
		(left - dx * left-gutter, top),
		(left - dx * left-gutter, bottom),
		height
	)

	mark-dim(
		(left, bottom + dy * bottom-gutter),
		(right, bottom + dy * bottom-gutter),
		width
	)
}

I stumbled across a fun numerical result.

#block-svg($ (sum_(i=1)^n i)^2 = sum_(i=1)^n i^3 $)

I found this because of a user on #link("https://mathstodon.xyz")[mathstodon.xyz], #link("https://mathstodon.xyz/@SvenGeier")[\@SvenGeier], who has the display name "Σ(i³)~=~(Σi)²". It's a nice result, which can be proved fairly easily by induction and a little bit of algebra. What interested me though was finding a deeper understanding of why this result works.

As an example of what I mean, let's turn to the slightly simpler result,

#block-svg($ sum_(i=1)^n i = (n(n+1))/2, $)

and see if we can understand it on a deeper level. This result is about what happens when you add up #inline-svg($i$) as #inline-svg($i$) varies from #inline-svg($1$) to #inline-svg($n$). We can graph this sum by having #inline-svg($n$) rows, one for each value of #inline-svg($i$), and putting #inline-svg($i$) boxes in each row.

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

The total sum is then the total number of boxes, or if we say each box is #inline-svg($1 times 1$) and has an area of #inline-svg($1$), then it is the total area of this shape:

#block-svg(canvas(step-shape()))

Writing this as an equation, we have

#block-svg($ sum_(i=1)^n i = Area(#canvas(step-shape())) $)

Doubling both sides and using some basic geometry, we get

#block-svg($
	2 sum_(i=1)^n i
	&= 2 Area(#canvas(step-shape())) \
	&= Area(#canvas(step-shape())) + Area(#canvas(step-shape())) \
	&= Area(#canvas(step-shape() + step-shape(tl: (4,3), br: (7,0)))) \
	&= Area(#canvas(step-shape() + step-shape(tl: (5,0), br: (2,3)))) \
	&= Area(#canvas(step-shape() + step-shape(tl: (3.6, 0), br: (0.6, 3)))) \
	&= Area(#pad-normal(canvas(rectangle(br:(3.6, 0), width: pad-small($n+1$))))) \
	&= n (n+1),
$)

and so dividing both sides by #inline-svg($2$), we finally get

#block-svg($ sum_(i=1)^n i = n(n+1)/2. $)

This works nicely because we have a two dimensional way of interpreting this sum, and two dimensions are easier to visualise. However, the result I want to understand is four dimensional. One one side the have #inline-svg($(sum_(i=1)^n i)^2$), which is the square of our two dimensional sum, and on the other side we have #inline-svg($sum_(i=1)^n i^3$), which is a stack of three dimensional cubes, which we stack using a fourth dimension. So how on earth can we graph this?

The trick is that some four dimensional shapes are just the product of two two dimensional shapes. To illustrate this, notice that some two dimensional shapes are the product of two one dimensional shapes. For example these two one dimensional shapes have lengths #inline-svg($n$) and #inline-svg($n+1$),

#block-svg($
	#canvas(interval())
	#h(3em)
	#canvas(interval(dimensions: pad-small($n+1$), gutter: 0.85)),
$)

and their product has area #inline-svg($n(n+1)$),

#block-svg($
	#canvas(interval())
	#pad-op($times$)
	#canvas(interval(dimensions: pad-small($n+1$), gutter: 0.85))
	#pad-op($=$)
	#canvas(rectangle(width: pad-small($n+1$), br: (3.6, 0)))
	#pad-op($.$)
$)

We can also use this to make three dimensional shapes:

#block-svg($
	#canvas({
		step-shape()
	})
	#pad-op($times$)
	#canvas(interval())
	#pad-op($=$)
	#canvas({
		import draw: line
		step-shape()

		steps(tl: (1.5, 3.75), br: (4.5, 0.75))
		for i in range(5) {
			let d = 3 * i / 5
			line((d, 3 - d), (d + 1.5, 3 - d + 0.75))
		}
		for i in (0,1,3,4) {
			let d = 3 * i / 5
			line((d + 3/5, 3 - d), (d + 3/5 + 1.5, 3 - d + 0.75))
		}
		interval(tl: (3, 0), br: (4.5, 0.75))
	})
	#pad-op($.$)
$)

As well as multiplying shapes, we can add them as we saw above. This allows us to create four dimensional shapes using just two dimensional shapes, which we can easily visualise.

Thus the thing we want to prove,

#block-svg($ (sum_(i=1)^n i)^2 = sum_(i=1)^n i^3, $)

becomes the geometric result,

#block-svg($
	Volume(
		#pad-normal(canvas(step-shape()))
		times
		#pad-normal(canvas(step-shape()))
	)
	=
	Volume(
		sum_(i=1)^n (
			#pad-normal(canvas(rectangle(tl: (0,1.5), height: pad-small($1$), width: pad-small($i$))))
			times
			#pad-normal(canvas(rectangle( height: pad-small($i$), width: pad-small($i$))))
		)
	)
	.
$)

This can't be visualised as a four dimensional shape, because our minds don't have the intuition for that. However, we can still do rotations and cutting and gluing of four dimensional shapes by doing the operations on the two dimensional shapes, though we may need a little bit of algebra, and some induction to handle the sum of cubes on the right hand side.

#parbreak()

So let's begin.

#block-svg($ (sum_(i=1)^n i)^2
	&= Volume(
		#pad-normal(canvas(step-shape()))
		times
		#pad-normal(canvas(step-shape()))
	) \
	&= Volume(
		#pad-normal(canvas({
			import draw: line
			step-shape()
			line((0,0.4), (1.6, 0.4))
		}))
		times
		#pad-normal(canvas(step-shape()))
	) \
	&= Volume(
		(
			#pad-normal(canvas(n-1-step-shape()))
			+
			#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		)
		times
		#pad-normal(canvas(step-shape()))
	)
$)

Here we have just cut off one of the rows of the bottom of the stairs. Next we will use distributivity, #inline-svg($(a + b) times c = a times c + b times c$).

#block-svg($ (sum_(i=1)^n i)^2
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(step-shape())) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		#pad-normal(canvas(step-shape()))
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		(
			#pad-normal(canvas(n-1-step-shape()))
			+
			#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		#pad-normal(canvas(step-shape()))
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5)))) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		#pad-normal(canvas(step-shape()))
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		#pad-normal(canvas(step-shape()))
	$))
$)

That last step might need some explanation. It looks like a simple instance of multiplication commuting, #inline-svg($a times b = b times a$), but geometrically, it is actually a rotation. If the left part of the product is in the #inline-svg($w - x$) plane and the right part is in the #inline-svg($y - z$) plane, then we have swapped the #inline-svg($w$) and #inline-svg($y$) axes, which is a reflection through the #inline-svg($w + y = 0$) hyperplane, and swapped the #inline-svg($x$) and #inline-svg($z$) axes, which is a reflection through the #inline-svg($x + z = 0$) hyperplane. The composition of two reflections is a rotation. Moving on.

#block-svg($
	(sum_(i=1)^n i)^2
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		(
			#pad-normal(canvas(n-1-step-shape()))
			+
			#pad-normal(canvas(step-shape()))
		)
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		(
			#pad-normal(canvas(n-1-step-shape()))
			+
			#pad-normal(canvas(step-shape(tl: (3.6,0), br: (0,3.6))))
		)
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		#pad-normal(canvas({
			import draw: line
			step-shape(tl: (3.75,0), br: (0,3.75))
			interval(tl: (0,0), br: (3,0), dimensions: pad-small($n - 1$), gutter: 0.35)
			interval(tl: (0,3), br: (0,0), dimensions: pad-small($n - 1$), gutter: 0.8)
		}))
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		#pad-normal(canvas(rectangle()))
	$))
$)

Now we repeatedly apply this same construction to the remaining product of stair-step shapes, gradually stripping off a layer of each stair and gluing them together into cubes, until we have a stack of cubes as required. Alternatively, we can view this as an induction, where the above is the main part of the inductive step. Either way, we have either the inductive hypothesis, or the result of repeated cutting and gluing, that

#block-svg($
	Volume(
		#pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape()))
	)
	=
	Volume(
		sum_(i=1)^(n-1)
		(
			#pad-normal(canvas(rectangle(height: pad-small($1$), width: pad-small($i$), tl: (0,1.5))))
			times
			#pad-normal(canvas(rectangle(height: pad-small($i$), width: pad-small($i$))))
		)
	)
$)

which we can sub into the proof so far.

#block-svg($
	(sum_(i=1)^n i)^2
	&= Volume(#block($
		& sum_(i=1)^(n-1)
			(
				#pad-normal(canvas(rectangle(height: pad-small($1$), width: pad-small($i$), tl: (0,1.5))))
				times
				#pad-normal(canvas(rectangle(height: pad-small($i$), width: pad-small($i$))))
			) \
		+&
		#pad-normal(canvas(rectangle(height: pad-small($1$), tl: (0,1.5))))
		times
		#pad-normal(canvas(rectangle()))
	$)) \
	&= Volume(
		sum_(i=1)^n (
			#pad-normal(canvas(rectangle(height: pad-small($1$), width: pad-small($i$), tl: (0,1.5))))
			times
			#pad-normal(canvas(rectangle(height: pad-small($i$), width: pad-small($i$))))
		)
	) \
	&= sum_(i=1)^n i^3
$)

This is, as far as I know, the first visual proof of a four dimensional result, and a new perspective on a well know result.