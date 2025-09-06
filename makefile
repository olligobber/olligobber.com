posts := $(shell grep "^path:" < posts/list | sed "s/^path: //")
tags := $(shell grep "^tags:" < posts/list | sed "s/^tags: //" | tr ' ' '\n' | sort | uniq -u)

all: static dynamic

.SECONDARY:

.PHONY: clean
clean:
	rm -rf docs
	rm -rf typst_build
	cd generator && cabal clean

generator/exe/allposts \
generator/exe/feed \
generator/exe/homepage \
generator/exe/post \
generator/exe/taggedposts \
generator/exe/tags \
	&: \
	generator/app/AllPosts.hs \
	generator/app/Feed.hs \
	generator/app/HomePage.hs \
	generator/app/Post.hs \
	generator/app/TaggedPosts.hs \
	generator/app/Tags.hs \
	generator/sitegenerator.cabal
	-mkdir -p generator/exe
	cd generator && cabal install --install-method=copy --installdir=exe --overwrite-policy=always

.PHONY: static
static: \
	docs/404.html \
	docs/about/index.html \
	docs/CNAME \
	docs/Logo.png \
	docs/style.css \
	docs/ttf/all

docs/CNAME: static/CNAME
	-mkdir -p docs
	cp static/CNAME docs/CNAME

docs/Logo.png: static/Logo.png
	-mkdir -p docs
	cp static/Logo.png docs/Logo.png

docs/style.css: static/style.css
	-mkdir -p docs
	cp static/style.css docs/style.css

docs/about/index.html: static/about_me.html
	-mkdir -p docs/about
	cp static/about_me.html docs/about/index.html

docs/404.html: static/404.html
	-mkdir -p docs
	cp static/404.html docs/404.html

.PHONY: docs/ttf/all
docs/ttf/all: \
	docs/ttf/FiraCode-Bold.ttf \
	docs/ttf/FiraCode-Light.ttf \
	docs/ttf/FiraCode-Medium.ttf \
	docs/ttf/FiraCode-Regular.ttf \
	docs/ttf/FiraCode-SemiBold.ttf

docs/ttf/FiraCode-%.ttf: static/ttf/FiraCode-%.ttf
	-mkdir -p docs/ttf
	cp $< $@

.PHONY: dynamic
dynamic: \
	docs/index.html \
	docs/atom.xml \
	docs/tags/index.html \
	docs/posts/index.html \
	$(addsuffix /index.html,$(addprefix docs/,$(posts))) \
	$(addsuffix /index.html,$(addprefix docs/tag/,$(tags)))

docs/index.html: \
	posts/list \
	templates/home_page.html \
	templates/post_div.html \
	templates/tag.html \
	generator/exe/homepage
	-mkdir -p docs
	generator/exe/homepage

docs/atom.xml: posts/list generator/exe/feed
	-mkdir -p docs
	generator/exe/feed

docs/tags/index.html: \
	posts/list \
	templates/all_tags.html \
	templates/tag_big.html \
	generator/exe/tags
	-mkdir -p docs/tags
	generator/exe/tags

docs/posts/index.html: \
	posts/list \
	templates/all_posts.html \
	templates/post_div.html \
	templates/tag.html \
	generator/exe/allposts
	-mkdir -p docs/posts
	generator/exe/allposts

docs/%/index.html: \
	posts/list \
	typst_build/%.html \
	templates/post.html \
	templates/tag.html \
	generator/exe/post
	-mkdir -p docs/$*
	generator/exe/post $*

docs/tag/%/index.html: \
	posts/list \
	templates/tagged_posts.html \
	templates/post_div.html \
	templates/tag.html \
	generator/exe/taggedposts
	-mkdir -p docs/tag/$*
	generator/exe/taggedposts $*

typst_build/%.html: posts/%/main.typ templates/template.typ
	-mkdir -p typst_build
	typst c --features html --format html $< $@