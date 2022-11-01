#define define_foreach(name, type)\
inline void foreach_##name(void (*f)(type), type *array, size_t count) {\
    for (size_t i = 0; i < count; i++) {\
        (*f)(array[i]);\
    }\
}
