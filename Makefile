SHELL=/bin/bash
.SHELLFLAG=-x
ifeq ("x${NO_GLEW}", "x")
	export GL_OPTS=-DUSE_GLEW=1
else
	export GL_OPTS=-DUSE_GLEW=0
endif
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
