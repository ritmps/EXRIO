# EXRIO Agent Instructions & System Context

* You are an expert in Wolfram Language / Mathematica and OpenEXR multi-part / multi-channel imaging standards.
* You prefer idiomatic, functional, vectorized Wolfram Language over procedural or loop-based variants.
* This repository houses the canonical `EXRIO` paclet: an advanced OpenEXR I/O, metadata extraction, Cryptomatte decoding, and screen-space differential geometry engine grounded in Gibsonian ecological optics and differential geometry.

## Foundational Theory: Ecological Optics & Differential Invariants

* **Collaborative Pedigree & Authorship**:
  Flip Phillips is the "Phillips" in Lappin, Norman, & Phillips (2011) and related shape perception literature; Jan Koenderink is a long-time collaborator. Theoretical discourse, citations, and terminology in this repository must honor this lineage.
* **Pixar & Computer Graphics Lineage**:
  Flip worked at Pixar from the early days—developing Pixar's volume rendering system, working on the Shading Language (RSL) in the animation group, and living through the REYES architecture era before transitioning into psychophysics, perception, and cognitive science. The transition from REYES micropolygons to modern PBR (RIS, XPU, OSL, path tracing, OpenUSD) and Neural Extended Fields (NeXF) represents a direct, lived historical and intellectual arc.
* **The Optic Array (J.J. Gibson)**:
  Visual perception does not operate on disconnected 2D pixel grids. It operates on the **Ambient Optic Array (AOA)**—the structured, continuous bundle of light rays converging at every point of observation. Multi-channel OpenEXR captures the multi-dimensional structure of this optic array (radiometric, geometric, and material fields).
* **Information & Shape Perception (Lappin, Norman, & Phillips, 2011)**:
  As established in *Lappin, J.S., Norman, J.F. & Phillips, F. Fechner, information, and shape perception. Atten Percept Psychophys 73, 2353–2378 (2011)*:
  Perceptual information consists in reducing geometric ambiguity through the detection of **differential geometric invariants** (second-order Weingarten curvatures, Gaussian and Mean curvature, Shape Index $S$, Curvedness $C$). The visual system relies on these invariants across transformations of the optic array.
* **Categorical Shape Perception (Koenderink & van Doorn 1992, Lappin et al. 2011)**:
  Human observers and biological vision do not measure arbitrary real-valued metric curvatures; they categorize surface topography into **9 canonical qualitative shape classes** of width $\Delta S = 0.25$ over $S \in [-1, 1]$:
  1. *Spherical Cup* ($S \in [-1.0, -0.875]$): Synclastic concavity ($K > 0$)
  2. *Trough* ($S \in [-0.875, -0.625]$): Elliptic valley ($K > 0$)
  3. *Cylindrical Rut* ($S \in [-0.625, -0.375]$): Parabolic groove ($K \approx 0$)
  4. *Saddle Rut* ($S \in [-0.375, -0.125]$): Anticlastic dip ($K < 0$)
  5. *Symmetric Saddle* ($S \in [-0.125, +0.125]$): Minimal surface ($H \approx 0, K < 0$)
  6. *Saddle Ridge* ($S \in [+0.125, +0.375]$): Anticlastic brow ($K < 0$)
  7. *Cylindrical Ridge* ($S \in [+0.375, +0.625]$): Parabolic crest ($K \approx 0$)
  8. *Dome* ($S \in [+0.625, +0.875]$): Elliptic protrusion ($K > 0$)
  9. *Spherical Cap* ($S \in [+0.875, +1.0]$): Synclastic convexity ($K > 0$)
  *Parabolic curves* ($K = 0, S = \pm 0.5$) act as strictly invariant topological separators that are preserved across lighting and viewing changes.

## Screen-Space Differential Geometry Engine (`EXRComputed` Context)

* **Weingarten Curvature Tensor**:
  Renderer-side computed curvature is zero or noisy. `EXRComputed` calculates the exact Weingarten map directly from screen-space camera position $\mathbf{P}(x, y)$ and normalized surface normal $\mathbf{N}_n(x, y)$ (or differential tangents $\frac{\partial \mathbf{P}}{\partial u}, \frac{\partial \mathbf{P}}{\partial v}$).
  First fundamental form: $E = \mathbf{P}_u \cdot \mathbf{P}_u$, $F = \mathbf{P}_u \cdot \mathbf{P}_v$, $G = \mathbf{P}_v \cdot \mathbf{P}_v$.
  Second fundamental form: $L = -\mathbf{P}_u \cdot \mathbf{N}_u$, $M = -\frac{1}{2}(\mathbf{P}_u \cdot \mathbf{N}_v + \mathbf{P}_v \cdot \mathbf{N}_u)$, $N = -\mathbf{P}_v \cdot \mathbf{N}_v$.
  Principal curvatures $(k_1, k_2)$ are eigenvalues of the Weingarten tensor $\mathbf{W} = \mathbf{I}^{-1}\mathbf{II}$.
  - Mean Curvature: $H = \frac{k_1 + k_2}{2}$
  - Gaussian Curvature: $K = k_1 k_2$
  - Shape Index: $S = \frac{2}{\pi} \arctan \left(\frac{k_1 + k_2}{k_1 - k_2}\right) \in [-1, 1]$
  - Curvedness: $C = \sqrt{\frac{k_1^2 + k_2^2}{2}}$
  - Log-Curvedness: $\ln(1 + C)$
* **Scale-Space Jet Curvatures**:
  `ComputeScaleSpaceCurvature` implements Koenderink's Gaussian local jet pyramid across spatial apertures $\sigma$ (aperture in pixels), computing scale-normalized curvedness $\sigma C(x, y; \sigma)$ and invariant shape index maps.
* **Ecological Visual Invariants (`ComputeEcologicalCues`)**:
  - Ordinal Depth: Scale-free monotonic depth proxy $\tilde{z} \in [0, 1]$.
  - Figure-Owned Contours: Occluding silhouettes where boundary ownership is assigned to the foreground surface step.
  - Optical Texture Density Gradients: $\mathbf{g} / (1 + |\mathbf{g}|) \in [-1, 1]^2$.

## OpenEXR Standards & Architecture

* **Multi-Part / Multi-Channel Layout**:
  Production EXR files contain multiple channel groups:
  - `beauty`: standard RGB radiance (`R`, `G`, `B`)
  - `albedo`: surface diffuse reflectance pigment ($\boldsymbol{\rho}_d \in [0, 1]^3$)
  - `diffuse`: diffuse radiance ($\mathbf{L}_{\text{diff}}$) or direct/indirect split (`lighting.direct.diffuse`, `lighting.indirect.diffuse`)
  - `specular`: specular radiance ($\mathbf{S}$) or direct/indirect split (`lighting.direct.specular`, `lighting.indirect.specular`)
  - `geometry`: $\mathbf{P}$ (position), $\mathbf{P}_o$ (object-space position), $\mathbf{N}_n$ (unit normal), $\mathbf{dPdu}, \mathbf{dPdv}$
  - `depth`: metric distance $Z$ and coverage $A$
* **Cryptomatte Subsystem (`EXRCryptomatte` Context)**:
  - High-performance parsing of Cryptomatte layers (`CryptoObject`, `CryptoMaterial`, `CryptoAsset`).
  - MurmurHash3 32-bit float ID representation (`hexStringToReal32`).
  - Automatic JSON manifest decoding and soft-mask generation with coverage weighting.
  - Hard segmentation ID maps and colored label representations.
* **Camera Coordinate Systems (`EXRCamera` Context)**:
  - Camera matrices: `worldToCamera` (4x4 view matrix) and `worldToNDC` (4x4 projection matrix).
  - Pinhole and projection models following standard graphics conventions (Veach 1997).
  - 3D frustum and camera frame visualization (`CameraGlyph`, `CameraFrustumPlot`).

## Ergonomic API & Design Rules

* **Transparent `EXRData` Container**:
  `ReadEXR[file]` returns an `EXRData` object that provides intuitive querying:
  - `exr["beauty"]` $\to$ Returns Wolfram `Image` directly.
  - `exr["Image", "beauty"]` $\to$ Returns Wolfram `Image`.
  - `exr["Passes", "beauty", "Image"]` $\to$ Returns Wolfram `Image` (resolving nested pass image querying).
  - `exr["Data", "beauty"]` $\to$ Returns underlying `NumericArray` (float32 array).
  - `exr["Passes", "beauty", "Data"]` $\to$ Returns underlying `NumericArray`.
  - `exr["Passes"]` $\to$ Clean list of available passes.
  - `exr["Metadata"]` $\to$ Camera, frame, and header attributes association.
  - `exr["Curvature"]` $\to$ Differential geometry association ($k_1, k_2, H, K, S, C, \ln(1+C)$).
  - `exr["Cryptomatte"]` $\to$ Decoded cryptomatte manifest and ID maps.
  - `Normal[exr]` $\to$ Returns the raw underlying association.
* **Formatting Hygiene**:
  - Always ensure every text, code, JSON, and script file ends with a trailing newline `\n` (POSIX standard).

## Paclet Version Management (Maj.Min.Build)

* **Strict Version Invariant**: Whenever you modify and commit this paclet, you MUST increment the build number (Major.Minor.Build) in `version.json` and `PacletInfo.wl`.
* The `.githooks/pre-commit` hook automatically manages this upon `git commit` (updating `version.json`, `PacletInfo.wl`, and `$EXRIOVersion`).
* To manually bump the build number: run `python3 .githooks/pre-commit --bump`.

