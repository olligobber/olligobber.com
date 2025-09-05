posts := $(shell grep "^path:" < posts/list | sed "s/^path: //")
tags := $(shell grep "^tags:" < posts/list | sed "s/^tags: //" | tr ' ' '\n' | sort | uniq -u)

all: static dynamic

.PHONY: clean
clean:
	rm -rf docs
	rm -rf typst_build
	cd generator && cabal clean

generator/exe/sitegenerator: generator/app/Main.hs generator/sitegenerator.cabal
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
	$(addsuffix /index.html,$(addprefix docs/,$(docs))) \
	$(addsuffix /index.html,$(addprefix docs/tag/,$(tags)))

docs/index.html \
docs/atom.xml \
docs/tags/index.html \
docs/posts/index.html \
$(addsuffix /index.html,$(addprefix docs/,$(docs))) \
$(addsuffix /index.html,$(addprefix docs/tag/,$(tags))) &: \
	posts/list \
	$(addsuffix .html,$(addprefix typst_build/,$(posts))) \
	templates/all_posts.html \
	templates/all_tags.html \
	templates/home_page.html \
	templates/post_div.html \
	templates/post.html \
	templates/tag_big.html \
	templates/tag.html \
	templates/tagged_posts.html \
	generator/exe/sitegenerator

	generator/exe/sitegenerator

typst_build/%.html: posts/%/main.typ templates/template.typ
	-mkdir -p typst_build
	typst c --features html --format html $< $@