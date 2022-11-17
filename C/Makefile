AALL := chop-args printenv
ALL := sorting-algorithms $(AALL)
CC := clang
STD := c11
O := 2
W := -Wall -Wpedantic
XCFLAGS :=
CFLAGS := -std=$(STD) -O$(O) $(W) $(XCFLAGS)
XCPPFLAGS :=
CPPFLAGS := $(XCPPFLAGS)

all: $(ALL)

%.o: %.c
	$(CC) $(CPPFLAGS) -c $^ $(CFLAGS) -o $@

.PHONY: sorting-algorithms
sorting-algorithms:
	$(MAKE) -C ./sorting-algorithms

chop-args: chop-args.o printstrv.o
	$(CC) $(CPPFLAGS) $^ $(CFLAGS) -o $@

printenv: printenv.o envc.o
	$(CC) $(CPPFLAGS) $^ $(CFLAGS) -o $@

.PHONY: clean
clean: clean-sorting-algorithms
	rm -f $(AALL)
	rm -f *.o

.PHONY: clean-sorting-algorithms
clean-sorting-algorithms:
	$(MAKE) -C ./sorting-algorithms clean
