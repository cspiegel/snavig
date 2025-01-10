use std::cmp;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fs;
use std::io::{self, IsTerminal, Read, Seek, Write};
use std::num;
use std::path;
use std::sync::atomic;

use clap::Parser;
use rayon::prelude::*;

mod image;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("not a blorb")]
    NotABlorb,
    #[error("RIdx mismatch")]
    RIdxMismatch,
    #[error("duplicate RIdx chunk found")]
    DuplicateRIdx,
    #[error("invalid resource usage: {0}")]
    InvalidResourceUsage(TypeID),
    #[error("two resources point to the same offset (legal, but unsupported)")]
    DuplicateResourceOffsets,
    #[error("two entries for the same resource: {0}")]
    DuplicateResources(Resource),
    #[error("duplicate Exec (story) resource chunks")]
    DuplicateExec,
    #[error("Exec resource chunk {0} at offset 0x{1:x} has id {2}, not 0")]
    InvalidExec(TypeID, u64, u32),
    #[error("resource {0} references non-existent chunk")]
    DanglingResource(Resource),
    #[error("no Pict entries")]
    NoPictures,
    #[error("this file already has a BPal chunk")]
    ExistingBPal,
    #[error("no APal chunk found")]
    MissingAPal,
    #[error("invalid APal size: {0}")]
    InvalidAPalSize(usize),
    #[error("APAl references image {0}, which does not exist")]
    MissingAPalImage(u32),
    #[error("no APAl images found")]
    NoAPalImages,
    #[error("Exec resource already exists")]
    ExecExists,
    #[error("unknown file type for Exec")]
    UnknownStoryType,
    #[error("integer overflow")]
    IntegerOverflow,
    #[error(transparent)]
    Image(#[from] image::Error),
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Png(#[from] oxipng::PngError),
    #[error(transparent)]
    Overflow(#[from] num::TryFromIntError),
}

trait CheckedWithResult: Sized {
    fn plus(self, other: Self) -> Result<Self, Error>;
    fn times(self, other: Self) -> Result<Self, Error>;
}

impl CheckedWithResult for u32 {
    fn plus(self, other: Self) -> Result<Self, Error> {
        self.checked_add(other).ok_or(Error::IntegerOverflow)
    }

    fn times(self, other: Self) -> Result<Self, Error> {
        self.checked_mul(other).ok_or(Error::IntegerOverflow)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct TypeID([u8; 4]);

impl TypeID {
    fn as_bytes(&self) -> &[u8; 4] {
        &self.0
    }
}

impl From<&[u8; 4]> for TypeID {
    fn from(bytes: &[u8; 4]) -> Self {
        TypeID(*bytes)
    }
}

impl fmt::Display for TypeID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let val = u32::from_be_bytes(self.0);
        write!(f, "{:08x} ({})", val, String::from_utf8_lossy(&self.0))
    }
}

trait BlorbReadOps {
    fn read32(&mut self) -> Result<u32, io::Error>;
    fn typeid(&mut self) -> Result<TypeID, io::Error>;
}

trait BlorbWriteOps {
    fn write32(&mut self, val: u32) -> Result<(), io::Error>;
}

impl<R: Read> BlorbReadOps for R {
    fn read32(&mut self) -> Result<u32, io::Error> {
        Ok(u32::from_be_bytes(*self.typeid()?.as_bytes()))
    }

    fn typeid(&mut self) -> Result<TypeID, io::Error> {
        let mut bytes = [0; 4];
        self.read_exact(&mut bytes)?;
        Ok(TypeID(bytes))
    }
}

impl<W: Write> BlorbWriteOps for W {
    fn write32(&mut self, val: u32) -> Result<(), io::Error> {
        self.write_all(&val.to_be_bytes())
    }
}

struct Chunk {
    typeid: TypeID,
    data: Vec<u8>,
}

impl Chunk {
    fn new(typeid: impl Into<TypeID>, data: impl Into<Vec<u8>>) -> Chunk {
        Chunk {
            typeid: typeid.into(),
            data: data.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ResourceUsage {
    Pict,
    Snd,
    Data,
    Exec,
}

impl ResourceUsage {
    fn as_bytes(&self) -> &'static [u8; 4] {
        match *self {
            ResourceUsage::Pict => b"Pict",
            ResourceUsage::Snd => b"Snd ",
            ResourceUsage::Data => b"Data",
            ResourceUsage::Exec => b"Exec",
        }
    }
}

impl fmt::Display for ResourceUsage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(self.as_bytes()))
    }
}

impl From<&ResourceUsage> for TypeID {
    fn from(value: &ResourceUsage) -> Self {
        value.as_bytes().into()
    }
}

impl From<ResourceUsage> for TypeID {
    fn from(value: ResourceUsage) -> Self {
        (&value).into()
    }
}

impl TryFrom<TypeID> for ResourceUsage {
    type Error = Error;

    fn try_from(value: TypeID) -> Result<Self, Self::Error> {
        match value.as_bytes() {
            b"Pict" => Ok(ResourceUsage::Pict),
            b"Snd " => Ok(ResourceUsage::Snd),
            b"Data" => Ok(ResourceUsage::Data),
            b"Exec" => Ok(ResourceUsage::Exec),
            _ => Err(Error::InvalidResourceUsage(value)),
        }
    }
}

#[derive(Debug, Clone)]
struct Resource {
    usage: ResourceUsage,
    number: u32,
    start: u32,
}

impl fmt::Display for Resource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "number {} ({}), offset 0x{:x}", self.number, self.usage, self.start)
    }
}

struct Blorb {
    ridx: Vec<Resource>,
    resources: BTreeMap<ResourceUsage, BTreeMap<u32, Chunk>>,
    chunks: Vec<Chunk>,
}

impl Blorb {
    fn new<A: AsRef<path::Path>>(path: A) -> Result<Blorb, Error> {
        let mut f = fs::File::open(path)?;

        if f.typeid()? != b"FORM".into() {
            return Err(Error::NotABlorb);
        }

        let size = f.read32()?;

        if f.typeid()? != b"IFRS".into() || f.typeid()? != b"RIdx".into() {
            return Err(Error::NotABlorb);
        }

        let n = f.read32()?;
        let num = f.read32()?;
        if n != num.times(12)?.plus(4)? {
            return Err(Error::RIdxMismatch);
        }

        let mut resource_by_offset = BTreeMap::new();

        let mut ridx: Vec<Resource> = vec![];
        for _ in 0..num {
            let usage = f.typeid()?.try_into()?;
            let number = f.read32()?;
            let start = f.read32()?;

            for resource in &ridx {
                if resource.start == start {
                    return Err(Error::DuplicateResourceOffsets);
                }

                if resource.usage == usage && resource.number == number {
                    return Err(Error::DuplicateResources(resource.clone()));
                }
            }

            resource_by_offset.insert(u64::from(start), (usage, number));

            ridx.push(Resource {
                usage,
                number,
                start,
            })
        }

        let mut resources: BTreeMap<ResourceUsage, BTreeMap<u32, Chunk>> = BTreeMap::new();
        let mut chunks = vec![];

        let known_chunks = [
            b"IFhd", b"Plte", b"Fspc", b"RDes", b"IFmd", b"RelN",
            b"Reso", b"APal", b"Loop", b"AUTH", b"(c) ", b"ANNO",
            b"SNam",
            b"BPal",
        ];

        // Per IFF:
        //
        // A "type ID", "property name", "FORM type", or any other IFF identifier is a 32-bit
        // value: the concatenation of four ASCII characters in the range R S (SP, hex 20) through
        // R~S (hex 7E). Spaces (hex 20) should not precede printing characters; trailing spaces
        // are ok. Control characters are forbidden.
        let is_valid_typeid = |typeid: &[u8; 4]| -> bool {
            typeid.iter().try_fold(false, |last_was_space, b| {
                if *b == b' ' {
                    Some(true)
                } else if *b >= 0x21 && *b <= 0x7e && !last_was_space {
                    Some(false)
                } else {
                    None
                }
            }).is_some()
        };

        let mut seen_unique_chunks = BTreeSet::new();
        let mut chunk_offsets = BTreeSet::new();

        while f.stream_position()? < u64::from(size) + 8 {
            let pos = f.stream_position()?;

            let chunktype = f.typeid()?;

            if !is_valid_typeid(chunktype.as_bytes()) {
                eprintln!("warning: invalid chunk type {chunktype} at offset 0x{pos:x}");
            }

            if chunktype == b"RIdx".into() {
                return Err(Error::DuplicateRIdx);
            }

            // For the time being, all known chunks must be unique.
            if known_chunks.contains(&chunktype.as_bytes()) {
                if seen_unique_chunks.contains(&chunktype) {
                    eprintln!("warning: found duplicate {chunktype} chunk at offset 0x{pos:x}");
                }

                seen_unique_chunks.insert(chunktype);
            }

            let size = f.read32()?;
            let mut chunk = vec![0; size.try_into()?];
            f.read_exact(&mut chunk)?;
            if size % 2 == 1 {
                f.seek(io::SeekFrom::Current(1))?;
            }

            let expected_usage = if chunktype == b"FORM".into() && chunk.len() >= 4 && &chunk[0..4] == b"AIFF" {
                Some(ResourceUsage::Snd)
            } else {
                match chunktype.as_bytes() {
                    b"PNG " | b"Rect" | b"JPEG" => Some(ResourceUsage::Pict),

                    b"OGGV" | b"MOD " | b"SONG" => Some(ResourceUsage::Snd),

                    b"TEXT" | b"BINA" => Some(ResourceUsage::Data),

                    b"ZCOD" | b"GLUL" | b"TAD2" | b"TAD3" | b"HUGO" | b"ALAN" |
                    b"ADRI" | b"LEVE" | b"AGT " | b"MAGS" | b"ADVS" | b"EXEC" => Some(ResourceUsage::Exec),

                    // Adrift 5 extensions
                    b"GIF " => Some(ResourceUsage::Pict),

                    // The Blorb specification says "WAV " but Adrift uses "WAVE"
                    b"WAVE" | b"WAV " | b"MIDI" | b"MP3 " => Some(ResourceUsage::Snd),

                    _ => None
                }
            };

            if let Some((usage, number)) = resource_by_offset.get(&pos) {
                if let Some(expected_usage) = expected_usage {
                    if expected_usage != *usage {
                        eprintln!("warning: RIdx specifies usage {} for resource {}, but expected usage is {}", TypeID::from(usage), number, expected_usage);
                    }
                } else {
                    eprintln!("warning: RIdx specifies usage {} for resource {}, but {} has no standard usage", TypeID::from(usage), number, chunktype);
                }

                if *usage == ResourceUsage::Exec {
                    if resources.get(usage).map_or(false, |map| !map.is_empty()) {
                        return Err(Error::DuplicateExec);
                    }

                    if *number != 0 {
                        return Err(Error::InvalidExec(chunktype, pos, *number));
                    }
                }

                chunk_offsets.insert(pos);

                resources.entry(*usage)
                    .or_default()
                    .insert(*number, Chunk::new(chunktype, chunk));
            } else {
                if let Some(expected_usage) = expected_usage {
                    eprintln!("warning: found {chunktype} chunk at offset 0x{pos:x}, but it is not referenced in RIdx (expected usage: {expected_usage})");
                } else if !known_chunks.contains(&chunktype.as_bytes()) {
                    if chunktype == b"FORM".into() && chunk.len() >= 4 {
                        let typedata = TypeID::from(&[chunk[0], chunk[1], chunk[2], chunk[3]]);
                        eprintln!("warning: copying unknown FORM chunk: {typedata} at offset 0x{pos:x}");
                    } else {
                        eprintln!("warning: copying unknown chunk: {chunktype} at offset 0x{pos:x}");
                    }
                }

                chunks.push(Chunk::new(chunktype, chunk));
            }
        }

        for resource in &ridx {
            if !chunk_offsets.contains(&resource.start.into()) {
                return Err(Error::DanglingResource(resource.clone()));
            }
        }

        Ok(Blorb {
            ridx,
            resources,
            chunks,
        })
    }

    fn add_bpal(&mut self) -> Result<(), Error> {
        let pict_resources = self.resources.get_mut(&ResourceUsage::Pict)
            .ok_or(Error::NoPictures)?;

        // Converted IDs start at 1000, unless the Blorb file contains
        // larger IDs, at which point the converted IDs start at the
        // largest ID plus one.
        let mut converted_id = 1000;

        for entry in &self.ridx {
            if entry.usage == ResourceUsage::Pict {
                converted_id = cmp::max(converted_id, entry.number.plus(1)?);
            }
        }

        for chunk in &self.chunks {
            if chunk.typeid == b"BPal".into() {
                return Err(Error::ExistingBPal);
            }
        }

        let mut apal_images = BTreeMap::new();
        for apal_id in Self::find_apal_images(&self.chunks)? {
            let chunk = pict_resources
                .get(&apal_id)
                .ok_or(Error::MissingAPalImage(apal_id))?;

            let image = image::new(&chunk.data)?;

            apal_images.insert(apal_id, image);
        }

        if apal_images.is_empty() {
            return Err(Error::NoAPalImages);
        }

        let mut converted_picts = BTreeMap::new();
        let mut image_cache = BTreeMap::new();
        let mut bpal_chunkdata = Vec::new();

        println!("Converting images...");
        for (id, chunk) in &mut *pict_resources {
            if chunk.typeid == b"PNG ".into() && !apal_images.contains_key(id) {
                let palette_image = image::new(&chunk.data)?;
                for (apal_id, apal_image) in &apal_images {
                    let converted = image::convert_palette(apal_image, &palette_image)?;

                    let new_id = match image_cache.get(&converted) {
                        Some(val) => *val,
                        None => {
                            converted_picts.insert(converted_id, Chunk::new(b"PNG ", converted.as_slice()));
                            converted_id += 1;
                            image_cache.insert(converted, converted_id - 1);
                            converted_id - 1
                        }
                    };

                    bpal_chunkdata.write32(*id)?;
                    bpal_chunkdata.write32(*apal_id)?;
                    bpal_chunkdata.write32(new_id)?;
                }
            }
        }

        let options = oxipng::Options::from_preset(6);
        Self::compress_png_chunks(converted_picts.values_mut(), &options, "Compressing BPal images")?;
        pict_resources.extend(converted_picts);

        self.chunks.push(Chunk::new(b"BPal", bpal_chunkdata));

        Ok(())
    }

    fn find_apal_images(chunks: &[Chunk]) -> Result<BTreeSet<u32>, Error> {
        let apal = chunks
            .iter()
            .find(|chunk| chunk.typeid == b"APal".into())
            .ok_or(Error::MissingAPal)?;

        if apal.data.len() % 4 != 0 {
            return Err(Error::InvalidAPalSize(apal.data.len()));
        }

        let mut apal_images = BTreeSet::new();
        let mut reader = io::Cursor::new(&apal.data);
        loop {
            match reader.read32() {
                Ok(id) => apal_images.insert(id),
                Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => break,
                Err(e) => return Err(e.into())
            };
        }

        Ok(apal_images)
    }

    pub fn add_exec(&mut self, data: &[u8]) -> Result<(), Error> {
        if let Some(v) = self.resources.get(&ResourceUsage::Exec) {
            if !v.is_empty() {
                return Err(Error::ExecExists);
            }
        }

        let magic = [
            (r"^[\x01-\x08].{17}\d{6}", b"ZCOD"),
            (r"^Glul", b"GLUL"),
            (r"^TADS2 bin\x0a\x0d\x1a", b"TAD2"),
            (r"^T3-image\x0d\x0a\x1a[\x01\x02]\x00", b"TAD3"),
            (r"^MaSc.{4}\x00\x00\x00\x2a\x00[\x00\x01\x02\x03\x04]", b"MAGS"),
            (r"^\x3c\x42\x3f\xc9\x6a\x87\xc2\xcf[\x92\x93\x94]\x45", b"ADRI"),
            (r"^\x58\xc7\xc1\x51", b"AGT "),
            (r"^.{2}\xa0\x9d\x8b\x8e\x88\x8e", b"ADVS"),
            (r"^[\x16\x18\x19\x1e\x1f].{2}\d\d-\d\d-\d\d", b"HUGO"),
            (r"^.{3}\x9b\x36\x21.{18}\xff", b"LEVE"),
            (r"^\x02(\x07\x05|\x08[\x01\x02\x03\x07])", b"ALAN"),
            (r"^ALAN\x03", b"ALAN"),
        ];

        let typeid = magic.into_iter().find(|(re, _)| {
            let re = regex::bytes::RegexBuilder::new(re)
                .dot_matches_new_line(true)
                .unicode(false)
                .build()
                .unwrap();

            re.is_match(data)
        });

        if let Some((_, typeid)) = typeid {
            let chunks = [
                (0, Chunk::new(typeid, data))
            ].into_iter().collect();
            self.resources.insert(ResourceUsage::Exec, chunks);
            Ok(())
        } else {
            Err(Error::UnknownStoryType)
        }
    }

    pub fn compress_images(&mut self) -> Result<(usize, usize), Error> {
        if let Some(picts) = self.resources.get_mut(&ResourceUsage::Pict) {
            let mut options = oxipng::Options::from_preset(6);

            // If there is a non-empty APal chunk, don't touch images'
            // palettes, as that will break its functionality. In the
            // absence of an APal chunk (or if the chunk is empty), the
            // specific layouts of images' palettes are not important.
            if matches!(Self::find_apal_images(&self.chunks), Ok(images) if !images.is_empty()) {
                options.color_type_reduction = false;
                options.palette_reduction = false;
            }

            Self::compress_png_chunks(picts.values_mut(), &options, "Compressing images")
        } else {
            Ok((0, 0))
        }
    }

    fn compress_png_chunks<'a, P>(picts: P, options: &oxipng::Options, msg: &str) -> Result<(usize, usize), Error>
    where
        P: IntoIterator<Item = &'a mut Chunk>
    {
        let mut picts = picts
            .into_iter()
            .filter(|chunk| chunk.typeid == b"PNG ".into())
            .collect::<Vec<_>>();

        if picts.is_empty() {
            return Ok((0, 0));
        }

        let pngs = picts.len();

        let original_size = atomic::AtomicUsize::new(0);
        let new_size = atomic::AtomicUsize::new(0);
        let i = atomic::AtomicUsize::new(0);

        // oxipng compresses in parallel, but it can be much faster to
        // run several compressions in parallel here anyway, to ensure
        // all cores are used.
        picts
            .par_iter_mut()
            .try_for_each(|chunk| -> Result<(), Error> {
                let i = i.fetch_add(1, atomic::Ordering::Relaxed) + 1;
                {
                    let mut stdout = io::stdout().lock();
                    if stdout.is_terminal() {
                        write!(stdout, "\x1b[K\r")?;
                        write!(stdout, "{msg}: {i}/{pngs}")?;
                        stdout.flush()?;
                    }
                }
                original_size.fetch_add(chunk.data.len(), atomic::Ordering::Relaxed);
                chunk.data = oxipng::optimize_from_memory(&chunk.data, options)?;
                new_size.fetch_add(chunk.data.len(), atomic::Ordering::Relaxed);

                Ok(())
            })?;

        if io::stdout().is_terminal() {
            println!();
        }

        let original_size = original_size.load(atomic::Ordering::Relaxed);
        let new_size = new_size.load(atomic::Ordering::Relaxed);
        Ok((original_size, new_size))
    }

    pub fn write<A: AsRef<path::Path>>(&self, filename: A) -> Result<(), Error> {
        let mut f = fs::File::create(filename)?;

        f.write_all(b"FORM....IFRSRIdx")?;

        let ridx_size = self.resources.values()
            .try_fold(0u32, |acc, chunks| {
                let size = u32::try_from(chunks.len()).ok()?;
                acc.checked_add(size)
            })
            .ok_or(Error::IntegerOverflow)?;

        f.write32(4.plus(ridx_size.times(12)?)?)?;
        f.write32(ridx_size)?;

        for (resource, chunks) in &self.resources {
            for id in chunks.keys() {
                f.write_all(resource.as_bytes())?;
                f.write32(*id)?;
                f.write32(0)?; // placeholder
            }
        }

        let write_chunk = |f: &mut fs::File, chunk: &Chunk| -> Result<(), Error> {
            f.write_all(chunk.typeid.as_bytes())?;
            f.write32(chunk.data.len().try_into()?)?;
            f.write_all(&chunk.data)?;
            if chunk.data.len() % 2 == 1 {
                f.write_all(b"\0")?;
            }

            Ok(())
        };

        for chunk in &self.chunks {
            write_chunk(&mut f, chunk)?;
        }

        for (i, (_, chunk)) in self.resources.values().flatten().enumerate() {
            let offset = f.stream_position()?;
            f.seek(io::SeekFrom::Start((0x20 + (i * 12)).try_into()?))?;
            f.write32(offset.try_into()?)?;
            f.seek(io::SeekFrom::Start(offset))?;

            write_chunk(&mut f, chunk)?;
        }

        f.seek(io::SeekFrom::End(0))?;
        let size = f.stream_position()?;
        f.seek(io::SeekFrom::Start(4))?;
        f.write32((size - 8).try_into()?)?;

        Ok(())
    }
}

#[derive(Parser)]
struct Options {
    /// Add a BPal entry
    #[arg(short, long)]
    add_bpal: bool,

    /// Add a story file as the Exec resource
    #[arg(short, long)]
    story: Option<path::PathBuf>,

    /// Losslessly compress existing PNG images (BPal images are always compressed)
    #[arg(short, long)]
    compress_images: bool,

    /// The Blorb file to create
    #[arg(short, long, name = "OUTFILE", default_value = "out.blb")]
    outfile: path::PathBuf,

    /// The Blorb file to parse
    #[arg(name = "BLORB")]
    blorb: path::PathBuf,
}

fn main() -> anyhow::Result<()> {
    let options = Options::parse();

    let mut blorb = Blorb::new(options.blorb)?;

    // This should happen before the BPal chunk is added, because that
    // compresses all BPal images, which would mean extra work for
    // nothing here.
    if options.compress_images {
        let (orig, new) = blorb.compress_images()?;
        if orig == 0 {
            println!("No PNG images found");
        } else if orig == new {
            println!("Images already fully compressed");
        } else if orig < new {
            println!("Compressed images are unexpectedly larger");
        } else {
            println!("Overall image size reduction from {orig} to {new}: {:.02}% of original", 100.0 * (new as f64) / (orig as f64));
        }
    }

    if options.add_bpal {
        println!("Adding BPal...");
        blorb.add_bpal()?;
    }

    if let Some(story) = options.story {
        blorb.add_exec(&fs::read(&story)?)?;
    }

    blorb.write(options.outfile)?;

    Ok(())
}
