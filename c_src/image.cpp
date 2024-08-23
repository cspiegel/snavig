#include <algorithm>
#include <cstddef>
#include <new>
#include <utility>

#include <QBuffer>
#include <QByteArray>
#include <QIODevice>
#include <QImage>
#include <QtGlobal>

#include "image.h"

struct CImage {
    QImage image;
};

CImage *new_image(const unsigned char *data, std::size_t len)
{
    QImage image;

    if (!image.loadFromData(data, len, "PNG")) {
        return nullptr;
    }

    return new CImage { std::move(image) };
}

void delete_image(CImage *image) {
    delete image;
}

Vector *convert_palette_c(const CImage *apal_image_, const CImage *palette)
{
    auto *vector = new Vector;

    QImage apal_image = apal_image_->image;
    if (palette->image.format() != QImage::Format_Indexed8) {
        vector->error = PaletteNotIndexed;
        return vector;
    }

    auto dst = apal_image.colorTable();
    auto src = palette->image.colorTable();

    for (int i = 2; i < qMin(src.size(), dst.size()); i++) {
        dst[i] = src[i];
    }

    apal_image.setColorTable(dst);

    QByteArray ba;
    QBuffer buffer(&ba);

    if (!buffer.open(QIODevice::WriteOnly)) {
        vector->error = UnableToOpenQBuffer;
        return vector;
    }

    if (!apal_image.save(&buffer, "PNG", 100)) {
        vector->error = UnableToSavePNG;
        return vector;
    }

    try {
        vector->data = new unsigned char[ba.size()];
    } catch (const std::bad_alloc &) {
        delete vector;
        throw;
    }

    vector->error = None;
    vector->len = ba.size();
    std::copy(ba.begin(), ba.end(), vector->data);

    return vector;
}

void delete_vector(Vector *vector)
{
    delete [] vector->data;
    delete vector;
}
