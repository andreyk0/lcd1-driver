build:
	stack build lcd1-driver

test:
	stack test lcd1-driver

clean:
	stack clean lcd1-driver

ghci:
	stack ghci

exec:
	stack exec -- lcd1-driver

tags:
	hasktags-generate .


.PHONY: tags exec clean build build-armhf test
