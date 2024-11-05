SHELL=/bin/bash
.SHELLFLAG=-x
all: clean config build install
config:
	if [ ! -d build ]; then mkdir build; fi
	cd build && cmake $(OPENPHIGS_OPTS) $(GL_OPTS) ../src
build:	config
	cd build && cmake --build .
install: build
	cd build && cmake --install . --prefix ../distrib
clean:
	rm -rf build
	rm -rf distrib
