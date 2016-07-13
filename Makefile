build:
	stack build lcd1-driver

test:
	stack test lcd1-driver

build-armv7l:
	docker-haskell-platform-armv7l jessie stack build

clean:
	stack clean lcd1-driver

ghci:
	stack ghci

exec:
	stack exec -- lcd1-driver

tags:
	hasktags-generate .

install: install-etc install-bin

install-etc:
	scp etc/systemd/system/lcd1-driver.service amqp:/tmp
	ssh amqp sudo install -o root -g root -m 644 /tmp/lcd1-driver.service /etc/systemd/system/lcd1-driver.service
	ssh amqp sudo systemctl daemon-reload

install-bin:
	scp .stack-work/install/arm-linux/nightly-2016-07-10/8.0.1/bin/lcd1-driver amqp:/tmp/
	ssh amqp sudo systemctl stop lcd1-driver.service
	ssh amqp sudo install -o root -g root -m 755 /tmp/lcd1-driver /www/lcd1-driver
	ssh amqp sudo systemctl start lcd1-driver.service


.PHONY: tags exec clean build build-armhf test install install-etc install-bin
