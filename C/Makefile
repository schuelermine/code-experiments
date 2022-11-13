ALL := sorting-algorithms
CC := clang
STD := c11
O := 2
W := -Wall -Wpedantic
XCFLAGS :=
CFLAGS := -std=$(STD) -O$(O) $(W) $(XCFLAGS)
XCPPFLAGS :=
CPPFLAGS := $(XCPPFLAGS)

.PHONY: sorting-algorithms
sorting-algorithms:
	$(MAKE) -C ./sorting-algorithms

%.o: %.c
	$(CC) $(CPPFLAGS) -c $^ $(CFLAGS) -o $@

chop-args: chop-args.o printstrv.o
	$(CC) $(CPPFLAGS) $^ $(CFLAGS) -o $@

printenv: printenv.o envc.o
	$(CC) $(CPPFLAGS) $^ $(CFLAGS) -o $@

.PHONY: clean
clean: clean-sorting-algorithms

.PHONY: clean-sorting-algorithms
clean-sorting-algorithms:
	$(MAKE) -C ./sorting-algorithms clean
