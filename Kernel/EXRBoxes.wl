(* ::Package:: *)

BeginPackage["EXRBoxes`"];

ClearAll[EXRData, MakeEXRData, PassToImage];
EXRData::usage = "EXRData[data] is a wrapper around a ReadEXR-style association with rich pass querying and custom summary box display.";
MakeEXRData::usage = "MakeEXRData[data] wraps a ReadEXR-style association in EXRData.";
PassToImage::usage = "PassToImage[pass] converts an EXR pass association to a Wolfram Image.";

Begin["`Private`"];

ClearAll[
  exrLookup,
  exrPasses,
  exrMetadata,
  exrCryptomatte,
  exrFile,
  exrComputed,
  exrAllPasses,
  findPass,
  passToImage,
  queryEXRData,
  exrPassDataDimensions,
  exrPassImageDimensions,
  exrPassElementType,
  exrPassKind,
  exrPassSummaryAssociation,
  exrPassPreview,
  exrCompactGrid,
  exrBeautyThumbnail,
  exrMetadataSummary,
  exrCryptomatteSummary,
  exrTopSummaryItems,
  exrExpandedSummaryItems,
  exrPreviewImageQ
];

MakeEXRData[data_Association] := EXRData[data];
MakeEXRData[other_] := other;

exrLookup[data_Association, key_String] := Lookup[data, key, Missing["NotAvailable"]];
exrPasses[data_Association] := Replace[exrLookup[data, "Passes"], _Missing -> <||>];
exrMetadata[data_Association] := Replace[exrLookup[data, "Metadata"], _Missing -> <||>];
exrCryptomatte[data_Association] := Replace[exrLookup[data, "Cryptomatte"], _Missing -> <||>];
exrFile[data_Association] := exrLookup[data, "File"];
exrComputed[data_Association] := Replace[exrLookup[data, "Computed"], _Missing -> <||>];

exrAllPasses[data_Association] := Join[exrPasses[data], exrComputed[data]];

findPass[data_Association, name_String] := Module[{passes = exrPasses[data], comp = exrComputed[data]},
  If[KeyExistsQ[passes, name],
    passes[name],
    If[KeyExistsQ[comp, name],
      comp[name],
      Missing["KeyAbsent", name]
    ]
  ]
];

passToImage[pass_Association] := Module[{type, d},
  If[KeyExistsQ[pass, "Image"] && MatchQ[pass["Image"], _Image],
    Return[pass["Image"]]
  ];
  type = Lookup[pass, "Type", "Color"];
  d = Lookup[pass, "Data", None];
  If[d === None, Return[$Failed]];
  If[MatchQ[d, _NumericArray], d = Normal[d]];
  Which[
    !ArrayQ[d],
      $Failed,
    type === "Color" || (ArrayDepth[d] == 3 && Last[Dimensions[d]] == 3),
      Image[Clip[N[d], {0., 1.}], "Real32", ColorSpace -> "RGB"],
    type === "Scalar" || ArrayDepth[d] == 2,
      Image[Rescale[N[d]], "Real32", ColorSpace -> "Grayscale"],
    type === "Vector3",
      Image[Rescale[N[d], {-1., 1.}, {0., 1.}], "Real32", ColorSpace -> "RGB"],
    True,
      Image[N[d], "Real32"]
  ]
];
passToImage[_] := $Failed;
PassToImage[pass_] := passToImage[pass];

queryEXRData[data_Association, "File"] := exrFile[data];
queryEXRData[data_Association, "Metadata"] := exrMetadata[data];
queryEXRData[data_Association, "Cryptomatte"] := exrCryptomatte[data];
queryEXRData[data_Association, "Computed"] := exrComputed[data];
queryEXRData[data_Association, "RawPasses"] := exrPasses[data];
queryEXRData[data_Association, "Passes"] := Keys[exrAllPasses[data]];
queryEXRData[data_Association, "Properties"] := Union[Keys[data], Keys[exrAllPasses[data]], {"RawPasses", "Properties", "Curvature"}];

queryEXRData[data_Association, "Curvature"] := Module[{comp = exrComputed[data], p = exrPasses[data], curKeys = {"k1", "k2", "H", "K", "ShapeIndex", "Curvedness", "LogCurvedness"}},
  KeyTake[Join[comp, p], Select[curKeys, KeyExistsQ[Join[comp, p], #] &]]
];

(* Single pass query by name: exr["beauty"] -> Image *)
queryEXRData[data_Association, name_String] := Module[{pass = findPass[data, name]},
  If[!MissingQ[pass],
    passToImage[pass],
    Lookup[data, name, Missing["KeyAbsent", name]]
  ]
];

(* Two-arg queries *)
queryEXRData[data_Association, "Image", name_String] := Module[{pass = findPass[data, name]},
  If[!MissingQ[pass], passToImage[pass], Missing["KeyAbsent", name]]
];
queryEXRData[data_Association, name_String, "Image"] := queryEXRData[data, "Image", name];

queryEXRData[data_Association, "Data", name_String] := Module[{pass = findPass[data, name]},
  If[!MissingQ[pass], Lookup[pass, "Data", Missing["KeyAbsent", "Data"]], Missing["KeyAbsent", name]]
];
queryEXRData[data_Association, name_String, "Data"] := queryEXRData[data, "Data", name];

queryEXRData[data_Association, "Type", name_String] := Module[{pass = findPass[data, name]},
  If[!MissingQ[pass], Lookup[pass, "Type", Missing["KeyAbsent", "Type"]], Missing["KeyAbsent", name]]
];
queryEXRData[data_Association, name_String, "Type"] := queryEXRData[data, "Type", name];

queryEXRData[data_Association, "Passes", name_String] := Module[{pass = findPass[data, name]},
  If[!MissingQ[pass],
    Join[pass, <|"Image" -> passToImage[pass]|>],
    Missing["KeyAbsent", name]
  ]
];

queryEXRData[data_Association, "Metadata", rest___] := data[["Metadata", rest]];
queryEXRData[data_Association, "Cryptomatte", rest___] := data[["Cryptomatte", rest]];

(* Three-arg queries: exr["Passes", "beauty", "Image"] *)
queryEXRData[data_Association, "Passes", name_String, "Image"] := queryEXRData[data, "Image", name];
queryEXRData[data_Association, "Passes", name_String, "Data"] := queryEXRData[data, "Data", name];
queryEXRData[data_Association, "Passes", name_String, "Type"] := queryEXRData[data, "Type", name];

(* Fallback for general association querying *)
queryEXRData[data_Association, args___] := Quiet @ Check[data[args], Missing["KeyAbsent", {args}]];

EXRData /: Normal[EXRData[data_Association]] := data;
EXRData /: Part[EXRData[data_Association], args___] := queryEXRData[data, args];
EXRData /: Keys[EXRData[data_Association]] := Keys[data];
EXRData /: Lookup[EXRData[data_Association], key_, default_: Missing["KeyAbsent", key]] :=
  With[{res = queryEXRData[data, key]}, If[MissingQ[res], default, res]];
EXRData[data_Association][args___] := queryEXRData[data, args];

(* Support legacy WXF deserialized EXRIO`EXRData transparently *)
Quiet[
  Off[EXRData::shdw];
  EXRIO`EXRData /: Normal[EXRIO`EXRData[data_]] := data;
  EXRIO`EXRData /: Part[EXRIO`EXRData[data_], args___] := queryEXRData[data, args];
  EXRIO`EXRData /: Keys[EXRIO`EXRData[data_]] := Keys[data];
  EXRIO`EXRData /: Lookup[EXRIO`EXRData[data_], key_, default_: Missing["KeyAbsent", key]] :=
    With[{res = queryEXRData[data, key]}, If[MissingQ[res], default, res]];
  EXRIO`EXRData[data_Association][args___] := queryEXRData[data, args];
];

exrPreviewImageQ[expr_] := MatchQ[expr, _Image];

exrPassDataDimensions[pass_Association] := Which[
  KeyExistsQ[pass, "Data"], Quiet @ Check[Dimensions @ Normal @ pass["Data"], Missing["Unavailable"]],
  KeyExistsQ[pass, "Image"] && exrPreviewImageQ[pass["Image"]], Reverse @ ImageDimensions @ pass["Image"],
  True, Missing["Unavailable"]
];

exrPassImageDimensions[pass_Association] := Which[
  KeyExistsQ[pass, "Image"] && exrPreviewImageQ[pass["Image"]], ImageDimensions @ pass["Image"],
  KeyExistsQ[pass, "Data"],
    Module[{dims = Quiet @ Check[Dimensions @ Normal @ pass["Data"], {}]},
      If[Length[dims] >= 2, {dims[[2]], dims[[1]]}, Missing["Unavailable"]]
    ],
  True, Missing["Unavailable"]
];

exrPassElementType[pass_Association] := Which[
  KeyExistsQ[pass, "Data"], Quiet @ Check[Developer`PackedArrayQ @ Normal @ pass["Data"]; Head[FirstCase[Normal @ pass["Data"], _?(NumericQ[#] || True &), Missing["Unavailable"], Infinity]], Missing["Unavailable"]],
  KeyExistsQ[pass, "Image"] && exrPreviewImageQ[pass["Image"]], ImageType @ pass["Image"],
  True, Missing["Unavailable"]
];

exrPassKind[pass_Association] := Which[
  KeyExistsQ[pass, "Image"] && KeyExistsQ[pass, "Data"], "Image+Data",
  KeyExistsQ[pass, "Image"], "Image",
  KeyExistsQ[pass, "Data"], "Data",
  True, "Other"
];

exrPassPreview[pass_Association] := Module[{img},
  img = If[KeyExistsQ[pass, "Image"],
    pass["Image"],
    If[KeyExistsQ[pass, "Data"],
      Quiet @ Check[passToImage[pass], Missing["NotAvailable"]],
      Missing["NotAvailable"]
    ]
  ];
  If[exrPreviewImageQ[img],
    Show[img, ImageSize -> {64, 64}, PlotRangePadding -> None, Background -> GrayLevel[.15]],
    Style["\[LongDash]", 14, GrayLevel[.55]]
  ]
];

exrPassSummaryAssociation[passes_Association] :=
  KeyValueMap[
    Function[{name, pass},
      name -> <|
        "Kind" -> exrPassKind[pass],
        "Keys" -> Keys[pass],
        "DataDimensions" -> exrPassDataDimensions[pass],
        "ImageDimensions" -> exrPassImageDimensions[pass]
      |>
    ],
    passes
  ];

exrCompactGrid[rules_List] := Grid[
  rules /. (label_ -> value_) :> {
    Style[label, GrayLevel[.35]],
    Pane[value, ImageSizeAction -> "ShrinkToFit"]
  },
  Alignment -> {Left, Top},
  Spacings -> {1.2, .7},
  BaseStyle -> {ShowStringCharacters -> False}
];

exrBeautyThumbnail[data_] := Module[{passes = exrPasses[data], beauty = Lookup[exrPasses[data], "beauty", Missing["NotAvailable"]], img},
  img = If[AssociationQ[beauty], passToImage[beauty], Missing["NotAvailable"]];
  If[exrPreviewImageQ[img],
    Show[img, ImageSize -> {80, 80}, PlotRangePadding -> None, Background -> GrayLevel[.15]],
    Graphics[{FaceForm[GrayLevel[.92]], EdgeForm[GrayLevel[.8]], Rectangle[]}, ImageSize -> {80, 80}]
  ]
];

exrMetadataSummary[data_] := Module[{meta = exrMetadata[data], material, fractal, rawKeys},
  material = Lookup[meta, "Material", Missing["NotAvailable"]];
  fractal = Lookup[meta, "Fractal", Missing["NotAvailable"]];
  rawKeys = Keys @ Lookup[meta, "RawAttributes", <||>];
  exrCompactGrid @ {
    "Top-level keys" -> Keys[meta],
    "Raw attribute count" -> Length[rawKeys],
    "Material" -> If[AssociationQ[material], material, material],
    "Fractal" -> If[AssociationQ[fractal], fractal, fractal]
  }
];

exrCryptomatteSummary[data_] := Module[{crypto = exrCryptomatte[data], layers, groups},
  layers = Lookup[crypto, "Layers", Missing["NotAvailable"]];
  groups = Lookup[crypto, "MetadataGroups", Missing["NotAvailable"]];
  exrCompactGrid @ {
    "Keys" -> Keys[crypto],
    "Layers" -> layers,
    "Metadata groups" -> groups,
    "Manifest entries" -> Replace[Lookup[crypto, "Manifest", Missing["NotAvailable"]], a_Association :> Length[a]]
  }
];

exrTopSummaryItems[data_] := Module[{passes = exrPasses[data], computed = exrComputed[data], meta = exrMetadata[data], crypto = exrCryptomatte[data]},
  {
    BoxForm`SummaryItem[{"File: ", exrFile[data]}],
    BoxForm`SummaryItem[{"Passes: ", Length @ passes}],
    BoxForm`SummaryItem[{"Computed: ", Length @ computed}],
    BoxForm`SummaryItem[{"Metadata keys: ", Length @ meta}],
    BoxForm`SummaryItem[{"Cryptomatte keys: ", Length @ crypto}]
  }
];

exrExpandedSummaryItems[data_] := Module[{passes = exrPasses[data], computed = exrComputed[data]},
  {
    BoxForm`SummaryItem[{"Beauty: ", exrBeautyThumbnail[data]}],
    BoxForm`SummaryItem[{"Pass names: ", Pane[Keys[passes], ImageSize -> {{220, 420}, Automatic}, Scrollbars -> Automatic]}],
    BoxForm`SummaryItem[{"Computed names: ", Pane[Keys[computed], ImageSize -> {{220, 420}, Automatic}, Scrollbars -> Automatic]}],
    BoxForm`SummaryItem[{"Pass summary: ", Pane[Dataset @ exrPassSummaryAssociation[passes], ImageSize -> {Automatic, 240}, Scrollbars -> Automatic]}],
    BoxForm`SummaryItem[{"Computed summary: ", Pane[Dataset @ exrPassSummaryAssociation[computed], ImageSize -> {Automatic, 240}, Scrollbars -> Automatic]}],
    BoxForm`SummaryItem[{"Metadata: ", Pane[exrMetadataSummary[data], ImageSize -> {Automatic, 180}, Scrollbars -> Automatic]}],
    BoxForm`SummaryItem[{"Cryptomatte: ", Pane[exrCryptomatteSummary[data], ImageSize -> {Automatic, 180}, Scrollbars -> Automatic]}]
  }
];

EXRData /: MakeBoxes[exr : EXRData[data_Association], form_ : StandardForm] :=
  If[TrueQ[$Notebooks],
    BoxForm`ArrangeSummaryBox[
      EXRData,
      exr,
      exrBeautyThumbnail[data],
      exrTopSummaryItems[data],
      exrExpandedSummaryItems[data],
      form,
      "Interpretable" -> Automatic
    ],
    RowBox[{"EXRData", "[", ToString[exrFile[data]], ", ", ToString[Length[exrPasses[data]]], " passes]"}]
  ];

End[];
EndPackage[];
