#import "template.typ" : styles, block-svg, inline-svg, parbreak
#import "@preview/cetz:0.4.1": canvas, draw
#import "@preview/metalogo:1.2.0": LaTeX

#show: styles

I just made a new website! Here I'll be posting about my maths and programming projects. But first, I want to talk about how I made this blog, which supports
- Code blocks: ```
	f :: Int -> String
	f x = repeat x "a"
```
- Inline maths: $P > Q = not (P -> Q)$
- Block maths: $ sum_(i=1)^n (i^3) = (sum_(i=1)^n i)^2 $
- Diagrams: #block-svg(canvas({
	import draw: *
	let row1 = ((1,1), (1,2), (1,3))
	let row2 = ((2,1), (2,3))
	let row3 = ((3,1), (3,3))
	let row4 = ((4,1), (4,2), (4,3))
	for c in row1 {
		for d in row2 {
			line(c,d)
		}
	}
	line((2,1),(3,1))
	line((2,3),(3,3))
	for c in row3 {
		for d in row4 {
			line(c,d)
		}
	}
	for c in row1 + row3 {
		circle(c, radius: .1, fill: blue, stroke: none)
	}
	for c in row2 + row4 {
		circle(c, radius: .1, fill:red, stroke: none)
	}
}))
and all of this in a typesetting language that is nice to use.

#parbreak()

First, let's go over some history. My first few websites were cobbled together by hand in HTML and CSS, and a pain to maintain myself, as I had to modify the HTML every time I wanted to make an update. As a result, they were all eventually abandoned. At one point, a friend made a website for me, which provided a web interface for writing posts. While this was very nice of them, I still had to upload any maths formulas or diagrams as static files, and maintain their code, which I did not understand. As a result, this website too was left under-used until eventually the code was too old to run correctly.

#parbreak()

Then, after complaining about #inline-svg(LaTeX), I was introduced to Typst, and quickly found that writing in it was actually enjoyable to write things in. I would use it for random ideas or trivial exercises, and so eventually I made the connection, what if I used it for my blog? It has HTML export, so how hard could it be?

#parbreak()

Ignoring all the warnings about HTML export being an experimental feature, and using experience I'd gained from making a website for my gaming group, I cobbled this site together. The posts are written in Typst, which is compiled to some very minimal HTML. Haskell code then extracts the body of that HTML, and inserts it into a hand-coded HTML template. Haskell also sets up the home page with the most recent posts, a page with a list of all the posts, and pages for the tag system, all using a text file that lists the key details of every post. There's also some static HTML and CSS in the mix. Then finally, all of this is pushed to Github, where it is hosted using Github pages. You can view the code on #link("https://github.com/olligobber/olligobber.com", "Github").

#parbreak()

Is this a cobbled together mess that could fall together at any moment? Yes, but because I made it using languages I enjoy coding in, it will most likely be maintained far longer than any of my other attempts. I'm looking forward to sharing my projects with the world via this new blog!