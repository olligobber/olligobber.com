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

#let mark-dim(start, end, dim, arrows: true) = {
	draw.content(
		(start, 50%, end),
		dim,
		name: "dim"
	)

	if arrows {
		arrow(
			"dim",
			start
		)
		arrow(
			"dim",
			end
		)
	}
}

#let steps(
	tl: (0,3),
	br: (3,0),
	types: ("full", "full", "dotted", "full", "full"),
) = {
	import draw: line

	let (left, top) = tl
	let (right, bottom) = br
	let n = types.len()
	let dx = (right - left)/n
	let dy = (bottom - top)/n

	for (i, t) in types.enumerate() {
		if t == "full" {
			line(
				(left + i * dx, top + i * dy),
				(left + (i + 1) * dx, top + i * dy),
				(left + (i + 1) * dx, top + (i + 1) * dy),
			)
		} else if t == "dotted" {
			line(
				(left + i * dx, top + i * dy),
				(left + (i + 1) * dx, top + (i + 1) * dy),
				stroke: (dash: "dotted")
			)
		} else {
			panic("Invalid step type")
		}
	}
}

#let step-shape(
	tl: (0,3),
	br: (3,0),
	dimensions: pad-small($n$),
	left-gutter: 0.35,
	bottom-gutter: 0.35,
	types: ("full", "full", "dotted", "full", "full"),
) = {
	import draw: line

	let (left, top) = tl
	let (right, bottom) = br
	let dx = (right - left) / calc.abs(right - left)
	let dy = (bottom - top) / calc.abs(bottom - top)

	line(tl, (left, bottom), br)

	steps(tl: tl, br: br, types: types)

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

#let n-1-step-shape = step-shape.with(
	left-gutter: 0.85,
	dimensions: pad-small($n-1$),
	types: ("full", "full", "dotted", "full"),
	tl: (0, 2.4),
	br: (2.4, 0),
	)

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

#let rectangle-1(
	tl: (0,0.6),
	br: (3,0),
	height: pad-small($1$),
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
		height,
		arrows: false,
	)

	mark-dim(
		(left, bottom + dy * bottom-gutter),
		(right, bottom + dy * bottom-gutter),
		width,
	)
}

I stumbled across a fun numerical result.

#block-svg($ (sum_(i=1)^n i)^2 = sum_(i=1)^n i^3 $)

I found this because of a user on #link("https://mathstodon.xyz")[mathstodon.xyz], #link("https://mathstodon.xyz/@SvenGeier")[\@SvenGeier], who has the display name "Σ(i³)~=~(Σi)²". It's an unexpected and beautiful result, which can be proved fairly easily by induction and a little bit of algebra. What interested me though was finding a deeper understanding of why this result works.

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

The total sum is then the total number of boxes, or if we say each box is #inline-svg($1 times 1$) and has an area of #inline-svg($1,$) then it is the total area of this shape:

#block-svg(canvas(step-shape()))

Writing this as an equation, we have

#block-svg($ sum_(i=1)^n i = Area(#canvas(step-shape())). $)

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

and so dividing both sides by #inline-svg($2,$) we finally get

#block-svg($ sum_(i=1)^n i = n(n+1)/2. $)

This works nicely because we have a two-dimensional way of interpreting this sum, and two dimensions are easier to draw. However, the result I want to understand is four-dimensional. On one side we have #inline-svg($(sum_(i=1)^n i)^2$), which is the square of our two-dimensional sum, and on the other side we have #inline-svg($sum_(i=1)^n i^3$), which is a stack of three-dimensional cubes, which we stack using a fourth dimension. So how on earth can we graph this?

The trick is that some four-dimensional shapes are just a product of two two-dimensional shapes. By product, I am referring to the cartesian product, which allows us to combine #inline-svg($n"-dimensional"$) and #inline-svg($m"-dimensional"$) coordinates into #inline-svg($(n+m)"-dimensional"$) coordinates. For example, one-dimensional coordinates are just numbers along a number line, but we can combine two to get a point in a plane.

#block-svg($
	#canvas({
		import draw: line, circle, content

		line((0, -3), (0, 3), mark: (end: "stealth", start: "stealth", fill: black))
		line((0.1, 0), (-0.1, 0))
		content((0, 3.5), $x$)
		circle((0, 2), radius: 0.1, fill: black)
		content((0.5, 2), $2$)
	})
	#pad-op($times$)
	#canvas({
		import draw: line, circle, content

		line((0, -3), (0, 3), mark: (end: "stealth", start: "stealth", fill: black))
		line((0.1, 0), (-0.1, 0))
		content((0, 3.5), $y$)
		circle((0, 1), radius: 0.1, fill: black)
		content((0.5, 1), $1$)
	})
	#pad-op($=$)
	#canvas({
		import draw: line, circle, content

		line((0, -3), (0, 3), mark: (end: "stealth", start: "stealth", fill: black))
		content((0, 3.5), $y$)
		line((-3, 0), (3, 0), mark: (end: "stealth", start: "stealth", fill: black))
		content((3.5, 0), $x$)
		circle((2, 1), radius: 0.1, fill: black)
		content((2.5, 1.5), $(2, 1)$)
	})
$)

Similarly, we can product one-dimensional points and two-dimensional points to get three-dimensional points.

#block-svg($
	#canvas({
		import draw: line, circle, content

		line((0, -3), (0, 3), mark: (end: "stealth", start: "stealth", fill: black))
		line((0.1, 0), (-0.1, 0))
		content((0, 3.5), $x$)
		circle((0, -1), radius: 0.1, fill: black)
		content((0.7, -1), $-1$)
	})
	#pad-op($times$)
	#canvas({
		import draw: line, circle, content

		line((0, -3), (0, 3), mark: (end: "stealth", start: "stealth", fill: black))
		content((0, 3.5), $z$)
		line((-3, 0), (3, 0), mark: (end: "stealth", start: "stealth", fill: black))
		content((3.5, 0), $y$)
		circle((1, -2), radius: 0.1, fill: black)
		content((1.5, -1.5), $(1, -2)$)
	})
	#pad-op($=$)
	#canvas({
		import draw: line, circle, content, ortho

		ortho(x: 30deg, y: 60deg, {
			line((-3, 0, 0), (3, 0, 0), mark: (end: "stealth", start: "stealth", fill: black))
			content((3.5, 0, 0), $x$)
			line((0, -3, 0), (0, 3, 0), mark: (end: "stealth", start: "stealth", fill: black))
			content((0, 3.5, 0), $y$)
			line((0, 0, -3), (0, 0, 3), mark: (end: "stealth", start: "stealth", fill: black))
			content((0, 0, 3.5), $z$)
			content((-1, 1, -2), canvas({circle((), radius: 0.1, fill: black)}))
			content((-0.5, 1.5, -3), $(-1, 1, -2)$)
		})
	})
$)

We can continue this, writing four-dimensional points as a product of two two-dimensional points, but drawing it yields something that is hard to draw.

#block-svg($
	#canvas({
		import draw: line, circle, content

		line((0, -3), (0, 3), mark: (end: "stealth", start: "stealth", fill: black))
		content((0, 3.5), $x$)
		line((-3, 0), (3, 0), mark: (end: "stealth", start: "stealth", fill: black))
		content((3.5, 0), $w$)
		circle((2, -1), radius: 0.1, fill: black)
		content((2, -0.5), $(2, -1)$)
	})
	#pad-op($times$)
	#canvas({
		import draw: line, circle, content

		line((0, -3), (0, 3), mark: (end: "stealth", start: "stealth", fill: black))
		content((0, 3.5), $z$)
		line((-3, 0), (3, 0), mark: (end: "stealth", start: "stealth", fill: black))
		content((3.5, 0), $y$)
		circle((1, -2), radius: 0.1, fill: black)
		content((1.5, -1.5), $(1, -2)$)
	})
	#pad-op($=$)
	#canvas({
		import draw: line, circle, content

		line((0, -3), (0, 3), mark: (end: "stealth", start: "stealth", fill: black))
		content((0, 3.5), $w$)
		line((-3, 0), (3, 0), mark: (end: "stealth", start: "stealth", fill: black))
		content((3.5, 0), $x$)
		line((-2, -1), (2, 1), mark: (end: "stealth", start: "stealth", fill: black))
		content((2.5, 1.5), $y$)
		line((-1, -2), (1, 2), mark: (end: "stealth", start: "stealth", fill: black))
		content((1.5, 2.5), $z$)
		circle((1.5, -2.25), radius: 0.1, fill: black)
		content((2.5, -1.75), $(2, -1, 1, -2)$)
	})
$)

It makes a lot of sense to draw four-dimensional points as a product of two-dimensional points, as drawing is a two-dimensional medium, and it aids a lot in visualising what is going on as we manipulate the shapes, as drawing four-dimensional manipulations would otherwise be difficult. Now we can generalise this way of drawing points to drawing shapes. A shape is just a collection of points, and so when we do the cartesian product of two shapes, we just take every cartesian product of pairs of points from those collections. This way our original shapes are the shadow of their product when light is shined at them from the right angle. This might be hard to imagine, so let's start with lower dimensions again. For a first example, notice that some two-dimensional shapes are the product of two one-dimensional shapes. For example these two one-dimensional shapes have lengths #inline-svg($n$) and #inline-svg($n+1,$)

#block-svg($
	#canvas(interval())
	#h(3em)
	#canvas(interval(dimensions: pad-small($n+1$), gutter: 0.85)),
$)

and their product has area #inline-svg($n(n+1),$)

#block-svg($
	#canvas(interval())
	#pad-op($times$)
	#canvas(interval(dimensions: pad-small($n+1$), gutter: 0.85))
	#pad-op($=$)
	#canvas(rectangle(width: pad-small($n+1$), br: (3.6, 0)))
	#pad-op($.$)
$)

We can also use this to make three-dimensional shapes:

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

As well as multiplying shapes, we can add or glue them as we saw in the first proof. This allows us to create certain four-dimensional shapes using just two-dimensional shapes, which we can more easily manipulate in proofs. The important facts about cartesian products is that volumes multiply, so doing the product of a shape with volume #inline-svg($A$) and a shape with volume #inline-svg($B$) will result in a shape with volume #inline-svg($A times B,$) and that rotating, cutting, and gluing the lower-dimensional shapes will have corresponding actions in the product shape, and preserve volume.

Moving on to what we wanted to prove,

#block-svg($ (sum_(i=1)^n i)^2 = sum_(i=1)^n i^3, $)

this becomes the geometric result,

#block-svg($
	Volume(
		#pad-normal(canvas(step-shape()))
		times
		#pad-normal(canvas(step-shape()))
	)
	=
	Volume(
		sum_(i=1)^n (
			#pad-normal(canvas(rectangle-1(width: pad-small($i$))))
			times
			#pad-normal(canvas(rectangle(height: pad-small($i$), width: pad-small($i$))))
		)
	)
	.
$)

This can't be drawn as a four-dimensional shape, because drawings are two-dimensional. However, we can still do rotations and cutting and gluing of four-dimensional shapes by doing the operations on the two-dimensional shapes, though we may need a little bit of algebra, and some induction to handle the sum of cube prisms on the right hand side. Note that the cube prisms are #inline-svg($i times i times i times 1,$) a cube in three dimensions and extended in the fourth by a single unit so that all the shapes are four-dimensional, and we have a consistent meaning of volume.

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
			line((0,0.6), (2.4, 0.6))
		}))
		times
		#pad-normal(canvas(step-shape()))
	) \
	&= Volume(
		(
			#pad-normal(canvas(n-1-step-shape()))
			+
			#pad-normal(canvas(rectangle-1()))
		)
		times
		#pad-normal(canvas(step-shape()))
	)
$)

Here we have just cut off one of the rows of the bottom of the stairs. Next we will use distributivity, #inline-svg($(a + b) times c = a times c + b times c.$)

#block-svg($ (sum_(i=1)^n i)^2
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(step-shape())) \
		+&
		#pad-normal(canvas(rectangle-1()))
		times
		#pad-normal(canvas(step-shape()))
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		(
			#pad-normal(canvas(n-1-step-shape()))
			+
			#pad-normal(canvas(rectangle-1()))
		) \
		+&
		#pad-normal(canvas(rectangle-1()))
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
		#pad-normal(canvas(rectangle-1())) \
		+&
		#pad-normal(canvas(rectangle-1()))
		times
		#pad-normal(canvas(step-shape()))
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle-1()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle-1()))
		times
		#pad-normal(canvas(step-shape()))
	$))
$)

That last step might need some explanation. It looks like a simple instance of multiplication commuting, #inline-svg($a times b = b times a,$) but geometrically, it is actually a rotation. If the left part of the product is in the #inline-svg($w - x$) plane and the right part is in the #inline-svg($y - z$) plane, then we have swapped the #inline-svg($w$) and #inline-svg($y$) axes, which is a reflection through the #inline-svg($w + y = 0$) hyperplane, and swapped the #inline-svg($x$) and #inline-svg($z$) axes, which is a reflection through the #inline-svg($x + z = 0$) hyperplane. The composition of two reflections is a rotation.

#parbreak()

Moving on.

#block-svg($
	(sum_(i=1)^n i)^2
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle-1()))
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
		#pad-normal(canvas(rectangle-1()))
		times
		(
			#pad-normal(canvas(n-1-step-shape()))
			+
			#pad-normal(canvas(step-shape(tl: (3,0), br: (0,3))))
		)
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle-1()))
		times
		#pad-normal(canvas({
			import draw: line
			step-shape(tl: (3,0), br: (0,3))
			n-1-step-shape()
		}))
	$)) \
	&= Volume(#block($
		& #pad-normal(canvas(n-1-step-shape()))
		times
		#pad-normal(canvas(n-1-step-shape())) \
		+&
		#pad-normal(canvas(rectangle-1()))
		times
		#pad-normal(canvas(rectangle()))
	$))
$)

Now we repeatedly apply this same construction to the remaining product of stair-step shapes, gradually stripping off a layer of each stair and gluing them together into cubes, until we have a stack of cubes as required. Alternatively, we can view this as an induction, where the above is the main part of the inductive step. Either way, from either the inductive hypothesis, or the result of repeated cutting and gluing, we have

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
			#pad-normal(canvas(rectangle-1(width: pad-small($i$))))
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
				#pad-normal(canvas(rectangle-1(width: pad-small($i$))))
				times
				#pad-normal(canvas(rectangle(height: pad-small($i$), width: pad-small($i$))))
			) \
		+&
		#pad-normal(canvas(rectangle-1()))
		times
		#pad-normal(canvas(rectangle()))
	$)) \
	&= Volume(
		sum_(i=1)^n (
			#pad-normal(canvas(rectangle-1(width: pad-small($i$))))
			times
			#pad-normal(canvas(rectangle(height: pad-small($i$), width: pad-small($i$))))
		)
	) \
	&= sum_(i=1)^n i^3
$)

This is, as far as I know, the first visual proof of a four-dimensional result, and a new perspective on a well known result.