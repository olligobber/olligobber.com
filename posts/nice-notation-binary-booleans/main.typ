#import "template.typ" : styles, block-svg, parbreak, no-lig
#import "@preview/cetz:0.4.2" : canvas, draw

#show: styles

#let True = text(rgb(51, 92, 51), $"True"$)
#let False = text(rgb(192, 51, 51), $"False"$)

#let my_xor = $<#h(-0.2em)>$

#let cartesian-product(..args) = {
	let pos = args.pos()
	if pos.len() == 0 {
		return ((),)
	} else {
		return pos.at(0).map(x =>
			cartesian-product(..pos.slice(1)).map(xs =>
				(x, ..xs)
			)
		).join()
	}
}

#let truth-table(vars, math-f, logic-f) = {
	let variables = if type(vars) == int {
		if vars > 4 { panic("Cannot write truth table for more than 4 inputs") }
		($P$, $Q$, $R$, $S$).slice(0, vars)
	} else {
		vars
	}
	let arity = variables.len()
	let inputs = cartesian-product(..variables.map(_ => (true, false)))
	table(
		columns: arity + 1,
		align: center,
		..variables, math-f(..variables),
		..inputs.map(i => (..i, logic-f(..i))).join().map(x => if x { $True$ } else { $False$ })
	)
}

While studying logic, I have encountered a variety of *boolean operators*: functions that take in a number of booleans, each either $True$ or $False,$ and output a single boolean. For example, the "and" operator, which is often notated with the symbol $and,$ takes in two booleans and outputs $True$ only when both inputs are $True.$ We can illustrate how this operator acts by writing a *truth table*, which has a row for each combination of inputs, named $P$ and $Q,$ and shows the output for those inputs.

#block-svg(truth-table(2, (p, q) => $#p and #q$, (p, q) => p and q))

The number of inputs an operator takes is called its *arity*, and it doesn't have to be two. For example, the "not" operator has an arity of one, so it is called *unary*:

#block-svg(truth-table(1, (p) => $not #p$, (p) => not p))

Also, the "falsum" operator has an arity of zero, so it is called *nullary*:

#block-svg(truth-table(0, () => $bot$, () => false))

There are even operators with higher arity, such as the conditional operator with an arity of three, which is called *ternary*:

#block-svg(truth-table(3, (p, q, r) => $#p #math.op($:$) #q #math.op($?$) #r$, (p, q, r) => if p { q } else { r } ))

The focus of this blog post will be on operators with arity two, or *binary* operators. As well as the "and" operator we saw above, most systems of logic include the "or", "implies", and "equivalent" operators:

#block-svg(
	box(truth-table(2, (p, q) => $#p or #q$, (p, q) => p or q))
	+
	h(1em)
	+
	box(truth-table(2, (p, q) => $#p -> #q$, (p, q) => p <= q))
	+
	h(1em)
	+
	box(truth-table(2, (p, q) => $#p <-> #q$, (p, q) => p == q))
)

These are all very standard, though some authors prefer to use double arrows such as $=>$ and $<=>$ instead of the single arrows I am using. There are also some binary operators with less standardised notation. First is the "exclusive or" operator, which I have seen notated as $+,$ $xor,$ and $underline(or).$ For now, let's use $xor$:

#block-svg(truth-table(2, (p, q) => $#p xor #q$, (p, q) => p != q))

Second, is the "nand" operator, which I have seen notated as $|,$ $arrow.t,$ and $overline(and).$ For now, let's use $overline(and)$:

#block-svg(truth-table(2, (p, q) => $#p overline(and) #q$, (p, q) => not (p and q)))

Lastly, is the "nor" operator, which I have seen notated as $arrow.b$ and $overline(or).$ For now, let's use $overline(or)$:

#block-svg(truth-table(2, (p, q) => $#p overline(or) #q$, (p, q) => not (p or q)))

And that's all for operators I've seen when studying logic. But it feels like there must be some missing: we've found seven binary operators, and seven is such a strange number for something involving booleans. Since a binary operator has four possible input combinations, and for each of those it can output one of two booleans, there should be $2^4=16$ binary operators. Two of these are essentially nullary operators, because they output the same thing regardless of their inputs, and four of these are essentially unary operators, because they ignore one input and just output either the other input or its negation. That leaves a total of ten binary operators, so where are the three we haven't found?

#parbreak()

Zachtronics released a game as a part of Last Call BBS called ChipWizard Professional. In this game, you are tasked with constructing fake electronic circuits, with one of the key tools being transistors made out of two types of silicon, N-Type and P-Type. I wanted to analyse what these transistors did, as boolean operators. One type of transistor, called NPN, was easy to model: applying charge to the P terminal connects the two N terminals, and if we take one of those N terminals as one input and the P terminal as the other input, then the transistor acts exactly like the "and" operator. The other one, called PNP, was more difficult: applying charge to the N terminal disconnects the two P terminals, so if we take one of those P terminals as one input and the N terminal as the other input, then we get the following operator:

#block-svg(truth-table(($P$, $N$), (p, q) => $#p "PNP" #q$, (p, q) => p and not q))

You may notice, this is not equal to any of the binary operators we've looked at so far. The one it is most similar to is the "implies" operator: this is the negation of it. For that reason, I called it "nimplies", and gave it the symbol $arrow.not.$

#parbreak()

With this, there are only two missing symbols, and those can be found through symmetry. With most of the interesting binary operators, swapping the order of the inputs doesn't change the output: $P and Q = Q and P.$ For two of our operators, $->$ and $arrow.not,$ we can flip the order, giving us the final operators, $<-$ and $arrow.l.not$:

#block-svg(
	box(truth-table(2, (p, q) => $#p <- #q$, (p, q) => p >= q))
	+
	h(1em)
	+
	box(truth-table(2, (p, q) => $#p arrow.l.not #q$, (p, q) => p < q))
)

So now we have a complete list of all ten binary operators.

#parbreak()

I didn't find this very satisfying, so the ten binary operators sat in my mind as I worked on other projects relating to them. The two nullary operators, $top$ and $bot$ have such a nice symmetry. $xor$ is really the same as $arrow.l.r.not$ or $!=.$ $->$ is really the same as $<=,$ as almost everyone agrees that $False < True$ is the standard order for booleans. And isn't it nice that we have that symmetry for $and$ and $or,$ which is kind of echoed in $overline(and)$ and $overline(or).$ Also, $arrow.not$ is so clunky to write, and doesn't always typeset nicely, and how do you even type it with just keyboard characters?

#parbreak()

What followed was a satisfying realisation. Suppose we build all ten binary operators using only wedges and arrows. See, $arrow.t$ is sometimes used to write "nand", and $arrow.b$ is sometimes used to write "nor". Also, if $->$ is the same as $<=,$ then $arrow.not$ is the same as $lt.eq.not,$ which is just $>,$ another wedge just like $and$ and $or.$ This hints at a possible rule: negating the output of a boolean operator turns arrows into wedges pointing the same direction, and vice versa. $and$ and $arrow.t$ are negations of each other, $or$ and $arrow.b$ are negations of each other, and $->$ and $>$ are negations of each other. Extending this rule, we get the obvious pair of $<-$ and $<,$ but also the non-obvious pair of $<->$ and $<>,$ which I often write as $#my_xor,$ omitting the space. Note that in some programming languages, #no-lig(`<>`) is a way of writing $!=,$ beautifully tying this all together.

#parbreak()

Then one night, I came up with this diagram:

#block-svg(canvas({
	import draw : content, line
	let draw-node(it) = pad(0.2em, it)
	content((6, 10), draw-node($top$), name: "top")
	content((0, 8), draw-node($P or Q$), name: "or")
	content((4, 8), draw-node($P -> Q$), name: "implies")
	content((8, 8), draw-node($P <- Q$), name: "implied")
	content((12, 8), draw-node($P arrow.t Q$), name: "nand")
	content((0, 5), draw-node($P$), name: "p")
	content((3, 5), draw-node($Q$), name: "q")
	content((6, 6), draw-node($P <-> Q$), name: "equiv")
	content((6, 4), draw-node($P #my_xor Q$), name: "xor")
	content((9, 5), draw-node($not Q$), name: "notq")
	content((12, 5), draw-node($not P$), name: "notp")
	content((0, 2), draw-node($P and Q$), name: "and")
	content((4, 2), draw-node($P < Q$), name: "lesser")
	content((8, 2), draw-node($P > Q$), name: "greater")
	content((12, 2), draw-node($P arrow.b Q$), name: "nor")
	content((6, 0), draw-node($bot$), name: "bottom")

	line("top", "or")
	line("top", "implies")
	line("top", "implied")
	line("top", "nand")
	line("or", "p")
	line("or", "q")
	line("or", "xor")
	line("implies", "q")
	line("implies", "notp")
	line("implies", "equiv")
	line("implied", "p")
	line("implied", "notq")
	line("implied", "equiv")
	line("nand", "notp")
	line("nand", "notq")
	line("nand", "xor")
	line("p", "and")
	line("q", "and")
	line("equiv", "and")
	line("notp", "lesser")
	line("q", "lesser")
	line("xor", "lesser")
	line("p", "greater")
	line("notq", "greater")
	line("xor", "greater")
	line("notp", "nor")
	line("notq", "nor")
	line("equiv", "nor")
	line("and", "bottom")
	line("lesser", "bottom")
	line("greater", "bottom")
	line("nor", "bottom")
}))

This diagram is a Hasse diagram, as a lower operator connected by a line to a higher operator means the lower operator implies the higher operator. For example, if $P and Q$ is true, then all three operators it connects to that are above it are also true: $P,$ $Q,$ and $P <-> Q.$

#parbreak()

It is also symmetric in a few different ways. The first is that if you rotate the entire diagram 180 degrees, each symbol moves to where its negation was. As well as negating the output, we can negate the inputs, such as turning $P or Q$ into $(not P) or (not Q),$ which is equal to $P arrow.t Q,$ as flipping this diagram horizontally negates the inputs. Lastly, we could negate both the inputs and output, giving us what is called the dual operator, which in this diagram is found by flipping the diagram vertically.

#parbreak()

So I finally have a satisfying grip on the binary operators, now I can do some actual logic with them. But that will have to wait for another time.