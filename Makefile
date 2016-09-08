project=$(shell basename `pwd`)
binary=`stack --nix path --local-install-root`

all: build
solve:
	@stack --nix solver --update-config

install: build
	@stack install
	@echo -e '\033[1;32m\e[1mInstallation succesful\e[0m'

deps:
	@vim $(project).cabal

build:
	@clear
	@cowsay "Compile!"
	@stack --nix build
	@echo -e '\e[1mBuild succesful\e[0m'

help:
	@echo 'solve   → if changed dependencies'
	@echo 'install → install program (it will be in $$PATH)'
	@echo 'deps    → edit .cabal file'
	@echo 'build   → compile'

run: build
	@stack exec $(project)

pushw: build install
	@echo 'Sending binary to remote server'
	@scp -P 7999 -i ~/.ssh/gray_rsa ~/.local/bin/powerstand valentin@psm.polyus-nt.ru:~/processing

push2: build install
	@echo 'Sending binary to remote server'
	@sshpass -p psmpass1! scp ~/.local/bin/powerstand psm@192.168.1.98:~/processing
	@sshpass -p psmpass1! scp preset7.json psm@192.168.1.98:

pushv: build install
	@echo 'Sending binary to virtual machine'
	@sshpass -p barter22 scp -P 2002 ~/.local/bin/powerstand naydenov@psm.golodnyj.ru:processing

force:
	stack build --ghc-options -fforce-recomp

run-d: install-d
	~/.local/bin/powerstand +RTS -xc

ship: build
	@git archive master -o /repo/$(project).tar.gz
