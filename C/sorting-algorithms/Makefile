ALL := demonstrate_sorts
CC := clang
STD := c11
O := 2
W := -Wall -Wpedantic
XCFLAGS :=
CFLAGS := -std=$(STD) -O$(O) $(W) $(XCFLAGS)
XCPPFLAGS :=
CPPFLAGS := $(XCPPFLAGS)

.PHONY: all
all: $(ALL)

demonstrate_sorts: demonstrate_sorts.o bubble_sort.o\
count_sort.o insertion_sort.o quick_sort.o selection_sort.o\
get_least_index.o swap.o printintv.o
	$(CC) $(CPPFLAGS) $^ $(CFLAGS) -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) -c $^ $(CFLAGS) -o $@

.PHONY: clean
clean:
	rm -f $(ALL)
	rm -f *.o
