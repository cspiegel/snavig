# The Bocfel Adaptive Palette Chunk

Because Glk does not give access to the underlying images in a Blorb file
(instead allowing you to refer to them by number only), using the Adaptive
Palette chunk from a Blorb file (see the [Blorb specification,
§11.3](https://www.eblong.com/zarf/blorb/blorb.html#s11.3)) is not possible:
you cannot read a palette from an image, and you cannot apply a palette to an
image.

To work around this, Bocfel introduces the Bocfel Adaptive Palette
Chunk, with type 'BPal'. All requirements for APal chunks apply to BPal
as well, and BPal serves as a replacement for APal when APal is not able
to be used.

    4 bytes         'BPal'          chunk ID
    4 bytes         num*12          chunk length
    num*12 bytes    ...             adaptive palette entries

Each entry is 12 bytes, of the form:

    4 bytes         current         current palette image number
    4 bytes         adaptive        requested APal image number
    4 bytes         replacement     the image with palette pre-applied

Blorb's adaptive palette works by applying the "Current Palette" (the
last-drawn non-adaptive image's palette) to a plotted image, if that
image is listed in the APal chunk.

The BPal chunk operates by pre-applying all possible palettes to APal
images, and storing these in the Blorb file. Each entry is a combination
of a current palette image (which must not be in the APal chunk), an
adaptive palette image (which is from the APal chunk), and the
replacement image ID, which has the palette pre-applied. In short,
"replacement" is the ID of an image representing "adaptive" with the
"current" palette applied.

In addition, if there is any gamma correction information in the palette
image (which effectively means the gAMA chunk in a PNG file), it should
also be copied into the replacement image.

The "Current Palette" should be tracked as in APal. Whenever a picture
is plotted, the BPal chunk should be consulted. If there is an entry
corresponding to the current palette and the requested image, the
replacement image should be plotted instead.

All replacement images must be the same size as the image they are
replacing. No new Reso entries are added for the replacement images,
even if the original image has a corresponding Reso entry. Instead,
interpreters must look up a Reso entry for the original image, and apply
it to the replacement.
