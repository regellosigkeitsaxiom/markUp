project=$(shell basename `pwd`)

all: run
solve:
	@stack solver --update-config

install:
	@cowsay "You hear voice calling you \"Iceberg!\""
	@stack --nix install
	@echo -e '\033[1;32m\e[1mInstallation succesful\e[0m'

deps:
	@vim $(project).cabal

build:
	@clear
	@cowsay "You hear voice calling you \"Move along!\""
	@stack --nix build
	@echo -e '\e[1mBuild succesful\e[0m'

help:
	@echo 'solve   → if changed dependencies'
	@echo 'install → install program (it will be in $$PATH)'
	@echo 'deps    → edit .cabal file'
	@echo 'build   → compile'

run: install
	@stack exec $(project)

install_bsd: build
	@stack install --local-bin-path .
	@mv shorturl /usr/local/sbin/shorturl
	@mv addurl /usr/local/sbin/addurl
	@cp misc/shorturl /usr/local/etc/rc.d/shorturl
	@cp misc/shorturl.conf /usr/local/etc/shorturl.conf
