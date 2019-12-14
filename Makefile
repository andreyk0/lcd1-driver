build:
	stack build lcd1-driver

clean:
	stack clean lcd1-driver

exec:
	stack exec -- lcd1-driver


.PHONY: exec clean build
