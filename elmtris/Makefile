CC = /usr/local/bin/elm
build = build/

runtime = elm/elm-runtime.js

src = src
flags = --make --set-runtime=$(runtime) --build-dir=$(build) --src-dir=$(src)

all: compile

compile: build/Tetris.html build/elm/

build/elm/: elm/
	cp -av elm build

build/Tetris.html: src/ snd/
	$(CC) $(flags) Tetris.elm
	cp -av snd/ build/snd

clean: build
	rm -rf build
	rm -rf cache

