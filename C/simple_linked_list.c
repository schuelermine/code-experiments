#include <stdlib.h>

typedef struct _sll_node {
    int value;
    struct _sll_node *next;
} sll_node;

typedef sll_node * sll;

sll sll_construct(int value, sll next) {
    sll list = malloc(sizeof(sll_node));
    list->value = value;
    list->next = next;
    return list;
}

sll sll_singleton(int value) {
    return sll_construct(value, NULL);
}

void sll_destruct(sll list) {
    sll current = list;
    sll next;
    while (next != NULL) {
        next = current->next;
        free(current);
        current = next;
    }
}

void sll_insert_head_i(sll *list, int value) {
    sll new_list = sll_construct(value, *list);
    *list = new_list;
}

sll sll_insert_head(sll list, int value) {
    sll new_list = sll_construct(value, list);
    return new_list;
}

void sll_insert_i_rec(sll *list, int value, size_t ix) {
    if (ix == 0) {
        sll_insert_head_i(list, value);
        return;
    }
    sll_insert_i_rec(&(*list)->next, value, ix - 1);
}

sll sll_insert_rec(sll list, int value, size_t ix) {
    if (ix == 0) {
        return sll_insert_head(list, value);
    }
    list->next = sll_insert_rec(list->next, value, ix - 1);
    return list;
}
