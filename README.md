# EXRIO: OpenEXR I/O & Differential Geometry for Wolfram Language

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Wolfram Language: 13.0+](https://img.shields.io/badge/Wolfram%20Language-13.0%2B-orange.svg)](https://www.wolfram.com/language/)

`EXRIO` is a high-performance, modular Wolfram Language paclet for reading, inspecting, and computing differential geometry passes from multi-channel, multi-part OpenEXR image files (RenderMan, OpenUSD, VFX production pipelines). Grounded in the ecological optics of J.J. Gibson and second-order differential geometry (*Lappin, Norman, & Phillips 2011*; *Koenderink & van Doorn 1992*), `EXRIO` provides native screen-space curvature computation, Cryptomatte manifest extraction, camera coordinate reconstruction, and intuitive Wolfram `Image` accessors.

---

## Features

* **Multi-Part & Multi-Channel OpenEXR**: Fast pure-binary header inspection and native channel extraction across arbitrary AOV configurations.
* **Ergonomic Accessors**: Zero-friction indexing:
  ```mathematica
  exr = ReadEXR["render.exr"]
  exr["beauty"]                      (* -> Wolfram Image *)
  exr["Image", "beauty"]             (* -> Wolfram Image *)
  exr["Passes", "beauty", "Image"]   (* -> Wolfram Image *)
  exr["Data", "beauty"]              (* -> NumericArray *)
  exr["Passes"]                      (* -> List of available pass names *)
  ```
* **Weingarten Curvature Engine (`EXRComputed`)**:
  Calculates exact principal curvatures $(k_1, k_2)$, Mean curvature ($H$), Gaussian curvature ($K$), Koenderink Shape Index ($S \in [-1, 1]$), Curvedness ($C$), and Log-Curvedness ($\ln(1+C)$) from screen-space position ($\mathbf{P}$) and surface normals ($\mathbf{N}_n$).
* **Koenderink 9 Qualitative Shape Categories**:
  Categorizes continuous Shape Index into the 9 canonical shape classes:
  1. Spherical Cup ($S \in [-1.0, -0.875]$)
  2. Trough ($S \in [-0.875, -0.625]$)
  3. Cylindrical Rut ($S \in [-0.625, -0.375]$)
  4. Saddle Rut ($S \in [-0.375, -0.125]$)
  5. Symmetric Saddle ($S \in [-0.125, +0.125]$)
  6. Saddle Ridge ($S \in [+0.125, +0.375]$)
  7. Cylindrical Ridge ($S \in [+0.375, +0.625]$)
  8. Dome ($S \in [+0.625, +0.875]$)
  9. Spherical Cap ($S \in [+0.875, +1.0]$)
* **Cryptomatte Decoding (`EXRCryptomatte`)**:
  Automatic parsing of Cryptomatte float hashes, JSON manifest decoding, soft mask generation, and hard segmentation maps.
* **Camera Coordinate Frame & Frustum Plotting (`EXRCamera`)**:
  Extracts 4x4 `worldToCamera` and `worldToNDC` transformation matrices; generates 3D camera glyphs and interactive frustum visualizations.

---

## Installation

```mathematica
PacletInstall["https://github.com/ritmps/EXRIO/releases/latest/download/EXRIO.paclet"]
```
Or for local development:
```mathematica
PacletDirectoryLoad["/path/to/EXRIO"]
Needs["EXRIO`"]
```

---

## Quick Start

```mathematica
Needs["EXRIO`"]

(* Load EXR with on-the-fly curvature passes *)
exr = ReadEXR["scene_001_view_000.exr", "ComputeDerivedPasses" -> True]

(* Query passes directly as Wolfram Images *)
beautyImg = exr["beauty"]
albedoImg = exr["albedo"]
shapeIndexImg = exr["ShapeIndex"]
curvednessImg = exr["Curvedness"]

(* Inspect camera FOV and metadata *)
fov = exr["Metadata", "Camera", "FOV"]
cryptoMask = CryptomatteObjectMask[exr, "/World/Props/Vase"]
```

---

## Testing

Run the included verification test suite:
```bash
wolframscript -file Tests/RunTests.wl
```

---

## Theoretical Lineage

* Lappin, J.S., Norman, J.F. & Phillips, F. *Fechner, information, and shape perception*. Attention, Perception, & Psychophysics 73, 2353–2378 (2011).
* Koenderink, J.J. & van Doorn, A.J. *Surface shape and curvature scales*. Image and Vision Computing 10(8), 557–564 (1992).
* Gibson, J.J. *The Ecological Approach to Visual Perception*. Houghton Mifflin (1979).

---

## License

MIT © [Flip Phillips](https://github.com/flipphillips) / [RIT Motion Picture Science](https://github.com/ritmps)
