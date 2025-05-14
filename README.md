# Snavig - Blorb manipulation tool

Snavig performs a few operations on existing Blorb files. First and foremost it
was developed as a way to add BPal chunks (see [Blorb.md](Blorb.md)) to Blorb
files so that the adaptive palettes of Zork Zero and Arthur can be used with
Glk-based interpreters. Such Blorb files are backward compatible: they contain
all information in the original Blorb plus the BPal chunk.

In addition, Snavig allows you to bundle a story file into an existing Blorb.
This is not particularly useful for new authors, but for older games, such as
some of Infocom's later titles, you can create a Blorb file that is
self-contained.

On top of this, Snavig can losslessly recompress existing PNG images in the
Blorb file to reduce their size, using [oxipng](https://github.com/shssoichiro/oxipng).
You do not need to install oxipng in order to use it, as Snavig is built with
the oxipng crate. Generated BPal images are always compressed, so this applies
only to the existing images, if any.

Snavig is not currently intended to be a general-purpose Blorb tool. Rather, it
is meant to augment/update existing Blorb files in specific ways.

## Building

Requirements:

* Rust (1.77.0 or newer)
* A C++17 compiler
* Qt (5 or 6)
* A Unix-like environment, including MinGW

Snavig will probably also build fine in non-Unix environments, if `build.rs` is
modified to point to a Qt installation, but this is untested.

To build:

    cargo build --release

## Running

Snavig always builds a new Blorb: it will not modify the input Blorb, unless you
also specify that Blorb file as the output Blorb.

To run Snavig:

    snavig [OPTIONS] <BLORB>

Options are:

* `-a`, `--add-bpal`: Add a BPal entry
* `-s`, `--story <STORY>`: Add the specified story
* `-c`, `--compress-images`: Losslessly compress existing PNG images
* `-o`, `--outfile <OUTFILE>`: The Blorb file to create [default: out.blb]

Snavig will detect a lot of inconsistencies in the Blorb file, either aborting
if the issue is egregious enough, or warning otherwise. Warnings are indicative
of a real problem, though, even if a new Blorb file is generated, so they really
ought to be heeded.

If no flags or options are provided, Snavig will produce a functionally
equivalent Blorb file to the input, although there is no guarantee that it will
be laid out in the same way.
