# olligobber.com

This repo contains the code to generate the website, as well as the generated site itself.

To view the website, go to [olligobber.com](https://olligobber.com/).

## Directories

`generator` contains code to generate parts of the site using haskell.

`static` contains static content to be served as is.

`templates` contains template html pages, with markers for where content is to be placed in, as well as the typst template.

`posts` contains all the code for the posts and their metadata.

`typst_build` contains partly built posts.

`docs` contains the generated website.

## Building

To build this site requires cabal 3.12, typst 0.14, and make. Simply running `make` will build all pages that are not up to date. `make clean` will remove all generated data so it can be rebuilt from scratch. Individual files can be built by simply calling `make <file>`, for example `make docs/how-i-made-this-blog/index.html`.

## Hosting

To host the site, simply serve the contents of the `docs` directory as static content. For exmaple, run `cd docs; python -m http.server`.