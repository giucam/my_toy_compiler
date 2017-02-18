#include <string.h>
#include <stdlib.h>

struct vector
{
    void *data;
    int32_t used;
    int32_t allocated;
};

extern "C" void __base_lib__vector_append(vector *vec, void *elm, int32_t elmSize)
{
    if (vec->used >= vec->allocated) {
        int32_t newSize = vec->allocated ? vec->allocated * 1.5 : 10;
        vec->data = realloc(vec->data, newSize * elmSize);
        vec->allocated = newSize;
    }
    char *dest = (char *)vec->data + vec->used++ * elmSize;
    memcpy(dest, elm, elmSize);
}

extern "C" void __base_lib_vector_free(vector *vec)
{
    free(vec->data);
}

extern "C" void __base_lib__vector_clear(vector *vec)
{
    vec->used = 0;
}

extern "C" void *__base_lib__vector_element(vector *vec, int32_t elmSize, int32_t index)
{
    return (char *)vec->data + index * elmSize;
}

extern "C" int32_t __base_lib__vector_count(vector *vec)
{
    return vec->used;
}
