#define DO_FREE(truth) DO_FREE_##truth
#define DO_FREE_true free(array);
#define DO_FREE_false ;

#define define_map(name1, type1, name2, type2, do_free)\
    inline type2* map_##name1##_##name2(type2 (*f)(type1), type1* array, size_t len) {\
        type2* array2 = malloc(len * sizeof(type2));\
        for (size_t i = 0; i < len; i++)\
            array2[i] = (*f)(array[i]);\
        DO_FREE(do_free);\
        return array2;\
    }
