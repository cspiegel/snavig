#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

struct CImage;

enum CError {
    None,
    PaletteNotIndexed,
    UnableToOpenQBuffer,
    UnableToSavePNG,
};

struct Vector {
    unsigned char *data;
    size_t len;
    enum CError error;
};

struct CImage *new_image(const unsigned char *data, size_t len);
void delete_image(struct CImage *image);
struct Vector convert_palette_c(const struct CImage *apal_image, const struct CImage *palette);
void delete_vector(struct Vector vector);

#ifdef __cplusplus
}
#endif
