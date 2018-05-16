include version.inc
build:
	mkdir -p exec include/g2_$(VERSION_STRING)_4 include/g2_$(VERSION_STRING)_d && \
	( set -xue ; cd src ; make -f makefile_4 ) && \
	( set -xue ; cd src ; make -f makefile_d ) && \
	( set -xue ; cd unit_test ; make all )

clean:
	rm -rf exec include/g2_$(VERSION_STRING)_4 include/g2_$(VERSION_STRING)_d *.a && \
	( set -xue ; cd unit_test ; make bare )

test: build
	./exec/g2_unit_test_4 && ./exec/g2_unit_test_d

# Common synonyms:
bare: clean
distclean: clean
all: build test
