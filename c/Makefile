AALL := chop-args printenv orbit
ALL := sorting_algorithms $(AALL)
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

.PHONY: sorting_algorithms
sorting_algorithms:
	$(MAKE) -C ./sorting_algorithms

chop-args: chop-args.o printstrv.o
	$(CC) $(CPPFLAGS) $^ $(CFLAGS) -o $@

printenv: printenv.o envc.o
	$(CC) $(CPPFLAGS) $^ $(CFLAGS) -o $@

orbit: orbit.o
	$(CC) $(CPPFLAGS) -lmpfr -lm $^ $(CFLAGS) -o $@

.PHONY: clean
clean: clean_sorting_algorithms
	rm -f $(AALL)
	rm -f *.o

.PHONY: clean_sorting_algorithms
clean_sorting_algorithms:
	$(MAKE) -C ./sorting_algorithms clean
