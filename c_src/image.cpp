#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <exception>
#include <optional>
#include <utility>

#include <QBuffer>
#include <QByteArray>
#include <QIODevice>
#include <QImage>
#include <QtGlobal>

#include "image.h"

class InvalidPNGError : public std::exception {};
class ExistingGammaError : public std::exception {};

struct CImage {
    QImage image;
    std::optional<std::array<std::uint8_t, 16>> gamma_chunk;
};

static std::uint32_t be32(const uint8_t *base)
{
    return (static_cast<std::uint32_t>(base[0]) << 24) |
           (static_cast<std::uint32_t>(base[1]) << 16) |
           (static_cast<std::uint32_t>(base[2]) <<  8) |
           (static_cast<std::uint32_t>(base[3]) <<  0);
}

const std::optional<std::array<std::uint8_t, 16>> find_gamma_chunk(const std::uint8_t *png_data, std::size_t data_size)
{
    std::size_t position = 8;

    while (position + 8 < data_size) {
        std::uint32_t chunk_data_length = be32(&png_data[position]);
        const std::uint8_t *chunk_type = &png_data[position + 4];

        if (position + 12 + chunk_data_length > data_size) {
            return std::nullopt;
        }

        if (std::memcmp(chunk_type, "gAMA", 4) == 0) {
            std::array<std::uint8_t, 16> gamma;
            std::copy_n(&png_data[position], 16, gamma.begin());
            return gamma;
        }

        if (std::memcmp(chunk_type, "IEND", 4) == 0) {
            break;
        }

        position += 12 + chunk_data_length;
    }

    return std::nullopt;
}

std::vector<std::uint8_t> add_gamma_chunk(const std::vector<std::uint8_t> &png_data, const std::array<std::uint8_t, 16> &gamma_chunk)
{
    static constexpr std::size_t insertion_point = 8 + 4 + 4 + 13 + 4; // Signature + IHDR
    if (png_data.size() < insertion_point) {
        throw InvalidPNGError();
    }

    const std::array<std::uint8_t, 8> png_signature = {0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a};
    if (std::memcmp(png_data.data(), png_signature.data(), 8) != 0) {
        throw InvalidPNGError();
    }

    if (std::memcmp(&png_data[12], "IHDR", 4) != 0) {
        throw InvalidPNGError();
    }

    if (find_gamma_chunk(png_data.data(), png_data.size()).has_value()) {
        throw ExistingGammaError();
    }

    std::vector<std::uint8_t> new_png_data;
    new_png_data.reserve(png_data.size() + gamma_chunk.size());
    new_png_data.insert(new_png_data.end(), png_data.begin(), png_data.begin() + insertion_point);
    new_png_data.insert(new_png_data.end(), gamma_chunk.begin(), gamma_chunk.end());
    new_png_data.insert(new_png_data.end(), png_data.begin() + insertion_point, png_data.end());

    return new_png_data;
}

CImage *new_image(const unsigned char *data, std::size_t len)
{
    QImage image;

    if (!image.loadFromData(data, len, "PNG")) {
        return nullptr;
    }

    auto gamma_chunk = find_gamma_chunk(data, len);

    return new CImage { std::move(image), std::move(gamma_chunk) };
}

void delete_image(CImage *image)
{
    delete image;
}

Vector convert_palette_c(const CImage *apal_image_, const CImage *palette)
{
    Vector vector = {
        .data = nullptr,
        .len = 0,
        .error = None,
    };

    QImage apal_image = apal_image_->image;
    if (palette->image.format() != QImage::Format_Indexed8) {
        vector.error = PaletteNotIndexed;
        return vector;
    }

    auto dst = apal_image.colorTable();
    auto src = palette->image.colorTable();

    for (qsizetype i = 2; i < qMin(src.size(), dst.size()); i++) {
        dst[i] = src[i];
    }

    apal_image.setColorTable(dst);

    QByteArray ba;
    QBuffer buffer(&ba);

    if (!buffer.open(QIODevice::WriteOnly)) {
        vector.error = UnableToOpenQBuffer;
        return vector;
    }

    if (!apal_image.save(&buffer, "PNG", 100)) {
        vector.error = UnableToSavePNG;
        return vector;
    }

    if (palette->gamma_chunk.has_value()) {
        try {
            auto with_gamma = add_gamma_chunk(std::vector<std::uint8_t>(ba.begin(), ba.end()), *palette->gamma_chunk);

            vector.data = new unsigned char[with_gamma.size()];
            vector.len = with_gamma.size();
            std::copy(with_gamma.begin(), with_gamma.end(), vector.data);
        } catch (const InvalidPNGError &) {
            vector.error = InvalidPNG;
        } catch (const ExistingGammaError &) {
            vector.error = ExistingGamma;
        }
    } else {
        vector.data = new unsigned char[ba.size()];
        vector.len = ba.size();
        std::copy(ba.begin(), ba.end(), vector.data);
    }

    return vector;
}

void delete_vector(Vector vector)
{
    delete [] vector.data;
}
