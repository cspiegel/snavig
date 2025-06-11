use std::slice;

mod image_impl;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("image is not a PNG")]
    InvalidImage,
    #[error("palette source not indexed")]
    PaletteNotIndexed,
    #[error("unable to open QBuffer")]
    UnableToOpenQBuffer,
    #[error("unable to store image as PNG")]
    UnableToSavePNG,
    #[error("invalid PNG file")]
    InvalidPNG,
    #[error("image already has gamma information")]
    ExistingGamma,
    #[error("unknown error in image code")]
    Unknown,
}

pub struct Image {
    image: *mut image_impl::CImage,
}

impl Drop for Image {
    fn drop(&mut self) {
        unsafe { image_impl::delete_image(self.image) };
    }
}

pub fn new(data: &[u8]) -> Result<Image, Error> {
    let image = unsafe { image_impl::new_image(data.as_ptr(), data.len()) };
    if image.is_null() {
        return Err(Error::InvalidImage);
    }

    Ok(Image { image })
}

pub fn convert_palette(apal_image: &Image, palette: &Image) -> Result<Vec<u8>, Error> {
    let vec = unsafe { image_impl::convert_palette_c(apal_image.image, palette.image) };

    match vec.error {
        image_impl::CError_PaletteNotIndexed => return Err(Error::PaletteNotIndexed),
        image_impl::CError_UnableToOpenQBuffer => return Err(Error::UnableToOpenQBuffer),
        image_impl::CError_UnableToSavePNG => return Err(Error::UnableToSavePNG),
        image_impl::CError_InvalidPNG => return Err(Error::InvalidPNG),
        image_impl::CError_ExistingGamma => return Err(Error::ExistingGamma),
        image_impl::CError_None => (),
        _ => return Err(Error::Unknown),
    }

    let slice = unsafe { slice::from_raw_parts_mut(vec.data, vec.len) };
    let converted = Vec::from(slice);

    unsafe { image_impl::delete_vector(vec) };

    Ok(converted)
}
