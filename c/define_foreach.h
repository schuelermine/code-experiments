#define define_foreach(name, type)\
    inline void foreach_##name(void (*f)(type), type* array, size_t len) {\
        for (size_t i = 0; i < len; i++)\
            (*f)(array[i]);\
    }
