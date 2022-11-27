#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

struct _ll_node {
    int value;
    struct _ll_node *next;
};

typedef struct _ll_node * linked_list;

void _ll_insert_head(linked_list list, int value) {
    struct _ll_node *head_ = malloc(sizeof(struct _ll_node));
    *list = *head_;
}

void ll_insert_rec(linked_list list, size_t ix, int value) {
    if (ix == 0) {
        _ll_insert_head(list, value);
        return;
    }
    ll_insert_rec(list->next, ix - 1, value);
}

void ll_insert_lin(linked_list list, size_t ix, int value) {
    linked_list cur = list;
    for (size_t i = 0; i < ix; i++) {
        cur = cur->next;
        assert(cur != NULL);
    }
    _ll_insert_head(cur, value);
}

int _ll_pop_head(linked_list list) {
    int value = list->value;
    free(list);
    *list = *(list->next);
    return value;
}

void _ll_delete_head(linked_list list) {
    _ll_pop_head(list);
}

int ll_pop_rec(linked_list list, size_t ix) {
    if (ix == 0) {
        return _ll_pop_head(list);
    }
    return ll_pop_rec(list->next, ix - 1);
}

void ll_delete_rec(linked_list list, size_t ix) {
    ll_pop_rec(list, ix);
}
