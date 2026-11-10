(* ::Package:: *)

BeginPackage["EXRCamera`", {"EXRBoxes`"}]

CameraGlyph::usage = "CameraGlyph[exr, opts] or CameraGlyph[matrix, opts] returns 3D graphics primitives for the camera coordinate frame and field-of-view frustum pyramid.";

CameraFrustumPlot::usage = "CameraFrustumPlot[exrsOrMatrices, opts] creates a 3D graphic visualizing camera coordinate frames and viewing frustums for one or more scenes.";

Begin["`Private`"]

isEXRDataQ[expr_] :=
  MatchQ[expr, _[ _Association ]] && MatchQ[Head[expr], Symbol] && (SymbolName[Head[expr]] === "EXRData");

unwrapEXR[expr_] :=
  If[isEXRDataQ[expr],
    First[expr]
    ,
    expr
  ];

Options[CameraGlyph] = {
  "Scale" -> 0.5,
  "FOV" -> 48.,
  "AspectRatio" -> 1.0,
  "FrustumOpacity" -> 0.15,
  "ShowAxes" -> True,
  "ShowFrustum" -> True,
  "ShowSphere" -> True,
  "FrameScale" -> 0.35,
  "Color" -> Automatic
};

(* Overload for 4x4 matrix *)
CameraGlyph[matrix_?MatrixQ, opts : OptionsPattern[]] :=
  Module[{scale, fov, ar, opacity, showAxes, showFrustum, showSphere,
    frameScale, col, inv, pos, rx, ry, rz, hw, hh, center,
    p1, p2, p3, p4, upNotch, prims},

    (* Guard against non-numeric, singular, or NaN matrices *)
    If[!MatrixQ[matrix, NumericQ] || Det[matrix] == 0,
      Return[{}]
    ];

    scale = OptionValue[CameraGlyph, {opts}, "Scale"];
    fov = OptionValue[CameraGlyph, {opts}, "FOV"];
    ar = OptionValue[CameraGlyph, {opts}, "AspectRatio"];
    opacity = OptionValue[CameraGlyph, {opts}, "FrustumOpacity"];
    showAxes = OptionValue[CameraGlyph, {opts}, "ShowAxes"];
    showFrustum = OptionValue[CameraGlyph, {opts}, "ShowFrustum"];
    showSphere = OptionValue[CameraGlyph, {opts}, "ShowSphere"];
    frameScale = OptionValue[CameraGlyph, {opts}, "FrameScale"];
    col = OptionValue[CameraGlyph, {opts}, "Color"];

    If[!NumericQ[fov] || fov <= 0, fov = 48.];
    If[!NumericQ[ar] || ar <= 0, ar = 1.0];

    inv = Inverse[matrix];
    pos = inv[[4, 1 ;; 3]];
    rx = Normalize[inv[[1, 1 ;; 3]]]; (* Right   +X *)
    ry = Normalize[inv[[2, 1 ;; 3]]]; (* Up      +Y *)
    rz = Normalize[inv[[3, 1 ;; 3]]]; (* Forward +Z (RenderMan view axis) *)

    hw = scale * Tan[fov * Degree / 2.0];
    hh = hw / ar;
    center = pos + scale * rz;

    (* 4 corners of base at distance 'scale' *)
    p1 = center - hw * rx - hh * ry; (* bottom-left  *)
    p2 = center + hw * rx - hh * ry; (* bottom-right *)
    p3 = center + hw * rx + hh * ry; (* top-right    *)
    p4 = center - hw * rx + hh * ry; (* top-left     *)

    (* Small apex indicator on top edge to indicate camera "up" *)
    upNotch = center + (hh * 1.2) * ry;

    prims = {};

    (* 1. Field-of-view Frustum Pyramid *)
    If[TrueQ[showFrustum],
      prims = Join[prims, {
        If[col === Automatic, GrayLevel[0.25], col],
        AbsoluteThickness[1.2],
        (* Rays from camera origin to 4 corners *)
        Line[{pos, p1}], Line[{pos, p2}], Line[{pos, p3}], Line[{pos, p4}],
        (* Base frame *)
        Line[{p1, p2, p3, p4, p1}],
        (* Up notch on top edge *)
        Line[{ (p4 + p3) / 2.0, upNotch }],
        (* Optional translucent facets *)
        If[NumericQ[opacity] && opacity > 0,
          {
            Opacity[opacity],
            If[col === Automatic, Cyan, col],
            Polygon[{pos, p1, p2}],
            Polygon[{pos, p2, p3}],
            Polygon[{pos, p3, p4}],
            Polygon[{pos, p4, p1}],
            Polygon[{p1, p2, p3, p4}]
          },
          Nothing
        ]
      }]
    ];

    (* 2. Coordinate Frame (RGB triad) *)
    If[TrueQ[showAxes],
      prims = Join[prims, {
        {Red,   Arrowheads[0.02], Arrow[{pos, pos + frameScale * rx}]},
        {Green, Arrowheads[0.02], Arrow[{pos, pos + frameScale * ry}]},
        {Blue,  Arrowheads[0.02], Arrow[{pos, pos + frameScale * rz}]}
      }]
    ];

    (* 3. Camera Apex Sphere *)
    If[TrueQ[showSphere],
      prims = Join[prims, {
        Darker[Gray, 0.2], Sphere[pos, frameScale * 0.12]
      }]
    ];

    prims
  ];

(* Overload for EXRData or Association *)
CameraGlyph[exr : _Association | _?isEXRDataQ, opts : OptionsPattern[]] :=
  Module[{assoc, camMeta, matrix, fov, ar, passData, dims},
    assoc = unwrapEXR[exr];
    If[KeyExistsQ[assoc, "CameraMatrix"],
      matrix = assoc["CameraMatrix"];
      If[!MatrixQ[matrix, NumericQ] || Det[matrix] == 0, Return[{}]];
      fov = Lookup[assoc, "FOV", OptionValue[CameraGlyph, {opts}, "FOV"]];
      If[!NumericQ[fov] || fov <= 0, fov = OptionValue[CameraGlyph, {opts}, "FOV"]];
      dims = Lookup[assoc, "Dimensions", {512, 512}];
      ar = OptionValue[CameraGlyph, {opts}, "AspectRatio"];
      If[ar === Automatic,
        ar = If[dims[[2]] > 0, dims[[1]] / dims[[2]], 1.0]
      ];
      Return[CameraGlyph[matrix, "FOV" -> fov, "AspectRatio" -> ar, opts]]
    ];

    camMeta = Lookup[Lookup[assoc, "Metadata", <||>], "Camera", <||>];
    matrix = Lookup[camMeta, "WorldToCamera", Missing[]];
    If[MissingQ[matrix],
      Return[{}]
    ];

    fov = Lookup[camMeta, "FOV", OptionValue[CameraGlyph, {opts}, "FOV"]];
    If[!NumericQ[fov] || fov <= 0,
      fov = OptionValue[CameraGlyph, {opts}, "FOV"]
    ];

    ar = OptionValue[CameraGlyph, {opts}, "AspectRatio"];
    If[ar === Automatic,
      passData = Lookup[Lookup[assoc, "Passes", <||>], "beauty", Missing[]];
      If[!MissingQ[passData] && KeyExistsQ[passData, "Data"],
        dims = Dimensions[passData["Data"]];
        If[Length[dims] >= 2 && dims[[1]] > 0,
          ar = dims[[2]] / dims[[1]],
          ar = 1.0
        ],
        ar = 1.0
      ]
    ];

    CameraGlyph[matrix, "FOV" -> fov, "AspectRatio" -> ar, opts]
  ];

(* Fallback for invalid input *)
CameraGlyph[___] := {};

Options[CameraFrustumPlot] = Join[
  Options[CameraGlyph],
  {
    "CenterTarget" -> {0, 0, 0},
    "ShowCenter" -> True,
    "TargetRadius" -> 0.25,
    Axes -> True,
    AxesLabel -> {"X", "Y", "Z"},
    Boxed -> True,
    ImageSize -> Large
  }
];

CameraFrustumPlot[items_List, opts : OptionsPattern[]] :=
  Module[{glyphOpts, gOpts, center, showCenter, r, glyphs},
    glyphOpts = FilterRules[{opts}, Options[CameraGlyph]];
    gOpts = FilterRules[{opts}, Options[Graphics3D]];
    center = OptionValue[CameraFrustumPlot, {opts}, "CenterTarget"];
    showCenter = OptionValue[CameraFrustumPlot, {opts}, "ShowCenter"];
    r = OptionValue[CameraFrustumPlot, {opts}, "TargetRadius"];

    glyphs = CameraGlyph[#, Sequence @@ glyphOpts] & /@ items;

    Graphics3D[{
      glyphs,
      If[TrueQ[showCenter],
        {Black, Opacity[0.6], Sphere[center, r]},
        Nothing
      ]
    }, Sequence @@ gOpts]
  ];

CameraFrustumPlot[item : (_Association | _?isEXRDataQ | _?MatrixQ), opts : OptionsPattern[]] :=
  CameraFrustumPlot[{item}, opts];

End[];
EndPackage[];
