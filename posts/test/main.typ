#set text(fill: rgb(51,51,51), font: "Fira Sans");
#show math.equation.where(block: false): it => html.elem("div", attrs: (class:"inline-typst"), html.frame(it))
#show math.equation.where(block: true): it => html.elem("div", attrs: (class:"block-typst"), html.frame(it))

This is a test post. $(1+2+...+9)^2 = 1^3+2^3+...+9^3 = 2025.$

$ sum_(i=1)^n i^3 = (sum_(i=1)^n i)^2 $

The next block of text is... blocky...