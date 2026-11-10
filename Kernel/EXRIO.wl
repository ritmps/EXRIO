(* ::Package:: *)

Module[{dir = DirectoryName[$InputFileName]},
  If[StringQ[dir] && dir =!= "",
    Get[FileNameJoin[{dir, "EXRCryptomatte.wl"}]];
    Get[FileNameJoin[{dir, "EXRComputed.wl"}]];
    Get[FileNameJoin[{dir, "EXRBoxes.wl"}]];
    Get[FileNameJoin[{dir, "EXRPreview.wl"}]];
    Get[FileNameJoin[{dir, "EXRCamera.wl"}]];
    Get[FileNameJoin[{dir, "ImageMetrics.wl"}]];
  ]
];

BeginPackage["EXRIO`", {
  "EXRCryptomatte`",
  "EXRComputed`",
  "EXRBoxes`",
  "EXRPreview`",
  "EXRCamera`",
  "ImageMetrics`"
}];

$EXRIOVersion::usage = "$EXRIOVersion is the version string of EXRIO.";
$EXRIOVersion = "2.0.5";

ReadEXR::usage = "ReadEXR[file, opts] imports a multi-channel OpenEXR file into a structured EXRData container with differential geometry and metadata.";
WriteEXR::usage = "WriteEXR[file, exr, opts] exports an EXRData container or association of passes to a multi-channel OpenEXR file.";
CryptomatteObjectMask::usage = "CryptomatteObjectMask[exr, name] builds a soft mask image for a named cryptomatte object.";
CryptomatteObjectMasks::usage = "CryptomatteObjectMasks[exr] builds soft masks for all named cryptomatte objects.";
CryptomatteLabelImage::usage = "CryptomatteLabelImage[exr] builds a label image from the strongest cryptomatte match per pixel.";

Begin["`Private`"];

isEXRDataQ[expr_] :=
  MatchQ[expr, _[ _Association ]] && MatchQ[Head[expr], _Symbol] && (SymbolName[Head[expr]] === "EXRData");

unwrapEXR[expr_] :=
  If[isEXRDataQ[expr], First[expr], expr];

wrapEXR[orig_, newAssoc_] :=
  If[isEXRDataQ[orig], EXRBoxes`EXRData[newAssoc], newAssoc];

Options[CryptomatteObjectMask] = {"Tolerance" -> 0., "Return" -> "Image"};

CryptomatteObjectMask[exr : _Association | _?isEXRDataQ, objectName_String, opts : OptionsPattern[]] :=
  Module[{assoc = unwrapEXR[exr], crypto, tol, mask},
    crypto = Lookup[assoc, "Cryptomatte", Missing["NoCryptomatte"]];
    If[MissingQ[crypto], Return[Failure["NoCryptomatte", <||>]]];
    tol = OptionValue[CryptomatteObjectMask, {opts}, "Tolerance"];
    mask = EXRCryptomatte`CryptomatteMask[crypto, objectName, If[tol == 0., 10^-6, tol]];
    If[mask === $Failed, Return[Missing["UnknownObject", objectName]]];
    Which[
      OptionValue[CryptomatteObjectMask, {opts}, "Return"] === "Data",
        NumericArray[mask, "Real32"],
      True,
        Image[mask, "Real32", ColorSpace -> "Grayscale"]
    ]
  ];

Options[CryptomatteObjectMasks] = Options[CryptomatteObjectMask];

CryptomatteObjectMasks[exr : _Association | _?isEXRDataQ, opts : OptionsPattern[]] :=
  Module[{assoc = unwrapEXR[exr], crypto, names, tol},
    crypto = Lookup[assoc, "Cryptomatte", Missing["NoCryptomatte"]];
    If[MissingQ[crypto], Return[Failure["NoCryptomatte", <||>]]];
    names = Keys @ Lookup[crypto, "NameToID", <||>];
    tol = OptionValue[CryptomatteObjectMasks, {opts}, "Tolerance"];
    AssociationMap[
      <|"Type" -> "Mask", "Data" -> CryptomatteObjectMask[assoc, #, "Return" -> "Data", "Tolerance" -> tol]|> &,
      names
    ]
  ];

CryptomatteLabelImage[exr : _Association | _?isEXRDataQ] :=
  Module[{assoc = unwrapEXR[exr], crypto, seg},
    crypto = Lookup[assoc, "Cryptomatte", Missing["NoCryptomatte"]];
    If[MissingQ[crypto], Return[$Failed]];
    seg = EXRCryptomatte`CryptomatteSegmentationMap[crypto];
    If[seg === $Failed, Return[$Failed]];
    Image @ Colorize[seg["SegmentationMap"]]
  ];

findEXRTool[name_String] := Module[{p},
  p = Which[
    FileExistsQ[FileNameJoin[{"/usr", "bin", name}]], FileNameJoin[{"/usr", "bin", name}],
    FileExistsQ[FileNameJoin[{"/usr", "local", "bin", name}]], FileNameJoin[{"/usr", "local", "bin", name}],
    FileExistsQ[FileNameJoin[{"/opt", "homebrew", "bin", name}]], FileNameJoin[{"/opt", "homebrew", "bin", name}],
    True, name
  ];
  p
];

readBinaryEXRHeaderAttributes[path_String] := Module[{
  stream, magic, byte, name, type, size, startPos, endPos, attrs = <||>
},
  stream = Quiet @ OpenRead[path, BinaryFormat -> True];
  If[stream === $Failed, Return[<||>]];

  magic = BinaryReadList[stream, "Byte", 4];
  If[magic =!= {118, 47, 49, 1},
    Close[stream];
    Return[<||>];
  ];
  BinaryReadList[stream, "Byte", 4];

  While[True,
    name = "";
    While[(byte = BinaryRead[stream, "Byte"]) =!= EndOfFile && byte != 0,
      name = name <> FromCharacterCode[byte];
    ];
    If[byte === EndOfFile || name == "", Break[]];

    type = "";
    While[(byte = BinaryRead[stream, "Byte"]) =!= EndOfFile && byte != 0,
      type = type <> FromCharacterCode[byte];
    ];
    If[byte === EndOfFile, Break[]];

    size = BinaryRead[stream, "UnsignedInteger32", ByteOrdering -> -1];
    If[size === EndOfFile, Break[]];

    startPos = StreamPosition[stream];
    endPos = startPos + size;

    Switch[type,
      "m44f",
        Module[{floats = BinaryReadList[stream, "Real32", 16, ByteOrdering -> -1]},
          If[Length[floats] == 16,
            attrs[name] = Developer`ToPackedArray[N @ Partition[floats, 4]];
          ];
        ],
      "v3f",
        Module[{vec = BinaryReadList[stream, "Real32", 3, ByteOrdering -> -1]},
          If[Length[vec] == 3,
            attrs[name] = Developer`ToPackedArray[N @ vec];
          ];
        ],
      "float",
        Module[{val = BinaryRead[stream, "Real32", ByteOrdering -> -1]},
          If[NumericQ[val], attrs[name] = N[val]];
        ],
      "int",
        Module[{ival = BinaryRead[stream, "Integer32", ByteOrdering -> -1]},
          If[IntegerQ[ival], attrs[name] = ival];
        ],
      "string",
        Module[{chars = BinaryReadList[stream, "Byte", size]},
          attrs[name] = FromCharacterCode[chars];
        ],
      _,
        Null
    ];
    SetStreamPosition[stream, endPos];
  ];

  Close[stream];
  attrs
];

getEXRHeaderText[path_String] :=
  Quiet @ Check[RunProcess[{findEXRTool["exrheader"], path}, "StandardOutput"], ""];

parseSciNumberString[str_String] := Module[{normalized, val},
  normalized = StringReplace[StringTrim[str], 
    RegularExpression["(?i)e\\+?(-?)0*(\\d+)"] :> "*^$1$2"
  ];
  val = Quiet @ Check[ToExpression @ normalized, Missing["NotANumber", str]];
  If[NumericQ[val], val, Indeterminate]
];

parseEXRVector[headerText_String, key_String] := Module[{match, vec},
  match = StringCases[headerText, RegularExpression["(?mi)^\\Q" <> key <> "\\E\\s*\\(type\\s+v3f\\):\\s*\\(([^\\)]*)\\)"] :> "$1"];
  If[match === {},
    Missing["KeyAbsent", key],
    vec = parseSciNumberString /@ StringSplit[StringTrim @ First[match], Whitespace..];
    If[VectorQ[vec, NumericQ], vec, {0., 0., 0.}]
  ]
];

parseEXRMatrix[headerText_String, key_String] := Module[{match, rows, mat},
  match = StringCases[headerText, RegularExpression["(?mis)^\\Q" <> key <> "\\E\\s*\\(type\\s+m44f\\):\\s*\\((.*?)\\)"] :> "$1"];
  If[match === {},
    Missing["KeyAbsent", key],
    rows = StringSplit[StringTrim @ First[match], "\n"];
    mat = parseSciNumberString /@ StringSplit[StringTrim[#], Whitespace..] & /@ rows;
    If[MatrixQ[mat, NumericQ] && Det[mat] != 0, mat, IdentityMatrix[4]]
  ]
];

parseFilenameMetadata[path_String] := Module[{base = FileBaseName[path], cases},
  base = StringDelete[base, ".crypto"];
  cases = StringCases[base, "scene_" ~~ s : DigitCharacter.. ~~ "_view_" ~~ v : DigitCharacter.. :> <|"SceneID" -> ToExpression[s], "ViewID" -> ToExpression[v]|>];
  If[cases =!= {}, Return[First[cases]]];
  cases = StringCases[base, "object_" ~~ j : DigitCharacter.. ~~ "_mat_" ~~ mat : Except["_"].. ~~ "_pose_" ~~ k : DigitCharacter.. ~~ "_view_" ~~ i : DigitCharacter.. :> <|"ObjectID" -> ToExpression[j], "MaterialName" -> mat, "PoseID" -> ToExpression[k], "ViewID" -> ToExpression[i]|>];
  If[cases =!= {}, First[cases], <||>]
];

buildMetadata[path_String, attrs_Association] := Module[{binAttrs, headerText, filenameMeta},
  binAttrs = readBinaryEXRHeaderAttributes[path];
  headerText = getEXRHeaderText[path];
  filenameMeta = parseFilenameMetadata[path];
  <|
    "RawAttributes" -> attrs,
    "Camera" -> <|
      "FOV" -> Lookup[attrs, "Fov", Lookup[binAttrs, "fov", Missing["KeyAbsent", "Fov"]]],
      "Eye" -> Replace[Lookup[attrs, "Cam_eye", Lookup[binAttrs, "cam_eye", Missing["KeyAbsent", "Cam_eye"]]], _Missing :> parseEXRVector[headerText, "cam_eye"]],
      "At" -> Replace[Lookup[attrs, "Cam_at", Lookup[binAttrs, "cam_at", Missing["KeyAbsent", "Cam_at"]]], _Missing :> parseEXRVector[headerText, "cam_at"]],
      "Up" -> Replace[Lookup[attrs, "Cam_up", Lookup[binAttrs, "cam_up", Missing["KeyAbsent", "Cam_up"]]], _Missing :> parseEXRVector[headerText, "cam_up"]],
      "WorldToCamera" -> Lookup[binAttrs, "worldToCamera", parseEXRMatrix[headerText, "worldToCamera"]],
      "WorldToNDC" -> Lookup[binAttrs, "worldToNDC", parseEXRMatrix[headerText, "worldToNDC"]]
    |>,
    "Material" -> <|
      "Name" -> Lookup[attrs, "Material_name", Lookup[filenameMeta, "MaterialName", Missing["KeyAbsent", "Material_name"]]],
      "Roughness" -> Lookup[attrs, "Mat_roughness", Missing["KeyAbsent", "Mat_roughness"]]
    |>,
    "FrameInfo" -> Join[filenameMeta, <|
      "ObjectID" -> Lookup[attrs, "Object_id", Lookup[filenameMeta, "ObjectID", Missing["KeyAbsent", "ObjectID"]]],
      "PoseID" -> Lookup[attrs, "Pose_id", Lookup[filenameMeta, "PoseID", Missing["KeyAbsent", "PoseID"]]],
      "ViewID" -> Lookup[attrs, "View_id", Lookup[filenameMeta, "ViewID", Missing["KeyAbsent", "ViewID"]]],
      "FrameID" -> Lookup[attrs, "Frame_id", Missing["KeyAbsent", "FrameID"]]
    |>]
  |>
];

$KnownPassSpecs = <|
  "beauty" -> <|"Type" -> "Color"|>,
  "denoised" -> <|"Type" -> "Color"|>,
  "albedo" -> <|"Type" -> "Color"|>,
  "diffuse" -> <|"Type" -> "Color"|>,
  "specular" -> <|"Type" -> "Color"|>,
  "diffuse.direct" -> <|"Type" -> "Color"|>,
  "diffuse.indirect" -> <|"Type" -> "Color"|>,
  "specular.direct" -> <|"Type" -> "Color"|>,
  "specular.indirect" -> <|"Type" -> "Color"|>,
  "A" -> <|"Type" -> "Scalar"|>,
  "Z" -> <|"Type" -> "Scalar"|>,
  "sampleCount" -> <|"Type" -> "Scalar"|>,
  "roughness" -> <|"Type" -> "Scalar"|>,
  "P" -> <|"Type" -> "Vector3"|>,
  "Po" -> <|"Type" -> "Vector3"|>,
  "Nn" -> <|"Type" -> "Vector3"|>,
  "NL" -> <|"Type" -> "Vector3"|>,
  "dPdu" -> <|"Type" -> "Vector3"|>,
  "dPdv" -> <|"Type" -> "Vector3"|>
|>;

hasPassQ[channelNames_, "beauty"] :=
  (KeyExistsQ[channelNames, "default"] && SubsetQ[channelNames["default"], {"R", "G", "B"}]) ||
  (KeyExistsQ[channelNames, "beauty"] && SubsetQ[channelNames["beauty"], {"R", "G", "B"}]);

hasPassQ[channelNames_, "denoised"] :=
  KeyExistsQ[channelNames, "denoised"] && SubsetQ[channelNames["denoised"], {"R", "G", "B"}];

hasPassQ[channelNames_, "A"] :=
  KeyExistsQ[channelNames, "default"] && MemberQ[channelNames["default"], "A"];

hasPassQ[channelNames_, "Z"] :=
  KeyExistsQ[channelNames, "default"] && MemberQ[channelNames["default"], "Z"];

hasPassQ[channelNames_, "sampleCount"] :=
  KeyExistsQ[channelNames, "default"] && MemberQ[channelNames["default"], "sampleCount"];

hasPassQ[channelNames_, "roughness"] :=
  (KeyExistsQ[channelNames, "default"] && MemberQ[channelNames["default"], "roughness"]) ||
  (KeyExistsQ[channelNames, "roughness"] && (MemberQ[channelNames["roughness"], "roughness"] || MemberQ[channelNames["roughness"], "z"] || MemberQ[channelNames["roughness"], "Z"]));

hasPassQ[channelNames_, pass : "albedo" | "diffuse" | "specular"] :=
  KeyExistsQ[channelNames, pass] && (SubsetQ[channelNames[pass], {"r", "g", "b"}] || SubsetQ[channelNames[pass], {"R", "G", "B"}]);

hasPassQ[channelNames_, "diffuse.direct"] :=
  KeyExistsQ[channelNames, "lighting"] && SubsetQ[channelNames["lighting"], {"direct.diffuse.r", "direct.diffuse.g", "direct.diffuse.b"}];

hasPassQ[channelNames_, "diffuse.indirect"] :=
  KeyExistsQ[channelNames, "lighting"] && SubsetQ[channelNames["lighting"], {"indirect.diffuse.r", "indirect.diffuse.g", "indirect.diffuse.b"}];

hasPassQ[channelNames_, "specular.direct"] :=
  KeyExistsQ[channelNames, "lighting"] && SubsetQ[channelNames["lighting"], {"direct.specular.r", "direct.specular.g", "direct.specular.b"}];

hasPassQ[channelNames_, "specular.indirect"] :=
  KeyExistsQ[channelNames, "lighting"] && SubsetQ[channelNames["lighting"], {"indirect.specular.r", "indirect.specular.g", "indirect.specular.b"}];

hasPassQ[channelNames_, pass : "P" | "Po" | "dPdu" | "dPdv"] :=
  KeyExistsQ[channelNames, "geometry"] && SubsetQ[channelNames["geometry"], (pass <> "." <> # & /@ {"x", "y", "z"})];

hasPassQ[channelNames_, "Nn"] :=
  KeyExistsQ[channelNames, "geometry"] && SubsetQ[channelNames["geometry"], {"Nn.x", "Nn.y", "Nn.z"}];

hasPassQ[channelNames_, "NL"] :=
  KeyExistsQ[channelNames, "geometry"] && SubsetQ[channelNames["geometry"], {"NL.r", "NL.g", "NL.b"}];

hasPassQ[channelNames_, _] := False;

sliceChannels[array_, positions_List] :=
  If[Length[positions] == 1,
    Normal[array[[All, All, positions[[1]]]]],
    Normal[array[[All, All, positions]]]
  ];

extractPass[data_Association, indexAssoc_Association, passName_String] := Module[{group, idx, positions, arr},
  Switch[passName,
    "beauty",
      group = If[KeyExistsQ[indexAssoc, "denoised"] && SubsetQ[Keys[indexAssoc["denoised"]], {"R", "G", "B"}],
        "denoised",
        If[KeyExistsQ[indexAssoc, "beauty"], "beauty", "default"]
      ];
      idx = indexAssoc[group];
      positions = idx /@ {"R", "G", "B"};
      arr = sliceChannels[data[group], positions];
      <|"Type" -> "Color", "Data" -> NumericArray[arr, "Real32"]|>,
    "A" | "Z" | "sampleCount" | "roughness",
      group = If[KeyExistsQ[indexAssoc, "roughness"], "roughness", "default"];
      idx = indexAssoc[group];
      positions = {If[KeyExistsQ[idx, passName], idx[passName], First[Values[idx]]]};
      arr = sliceChannels[data[group], positions];
      <|"Type" -> "Scalar", "Data" -> NumericArray[arr, "Real32"]|>,
    "albedo" | "diffuse" | "specular" | "denoised",
      group = passName;
      idx = indexAssoc[group];
      positions = idx /@ (If[SubsetQ[Keys[idx], {"R", "G", "B"}], {"R", "G", "B"}, {"r", "g", "b"}]);
      arr = sliceChannels[data[group], positions];
      <|"Type" -> "Color", "Data" -> NumericArray[arr, "Real32"]|>,
    "diffuse.direct" | "diffuse.indirect" | "specular.direct" | "specular.indirect",
      group = "lighting";
      idx = indexAssoc[group];
      positions = With[{parts = StringSplit[passName, "."]},
        idx /@ (parts[[2]] <> "." <> parts[[1]] <> "." <> # & /@ {"r", "g", "b"})
      ];
      arr = sliceChannels[data[group], positions];
      <|"Type" -> "Color", "Data" -> NumericArray[arr, "Real32"]|>,
    "P" | "Po" | "Nn" | "NL" | "dPdu" | "dPdv",
      group = "geometry";
      idx = indexAssoc[group];
      positions = If[passName === "NL",
        idx /@ {"NL.r", "NL.g", "NL.b"},
        idx /@ (passName <> "." <> # & /@ {"x", "y", "z"})
      ];
      arr = sliceChannels[data[group], positions];
      If[passName === "Nn" && SubsetQ[Keys[idx], {"P.x", "P.y", "P.z"}],
        Module[{pArr, signFlip},
          pArr = sliceChannels[data[group], idx /@ {"P.x", "P.y", "P.z"}];
          signFlip = 1.0 - 2.0 * UnitStep[Total[pArr * arr, {3}]];
          arr = arr * Transpose[ConstantArray[signFlip, 3], {3, 1, 2}];
        ]
      ];
      <|"Type" -> "Vector3", "Data" -> NumericArray[arr, "Real32"]|>,
    _,
      Nothing
  ]
];

addDerivedPasses[exr : _Association | _?isEXRDataQ, envMap_] := Module[{
  result, passes, computed, diffArr, albArr, irradArr, logZ, qLow, qHigh, ordZ, envSampleImg, envSampleData
},
  result = unwrapEXR[exr];
  result = EXRComputed`ComputeCurvature[result];
  result = EXRComputed`ComputeEcologicalCues[result];
  passes = Lookup[result, "Passes", <||>];
  computed = Lookup[result, "Computed", <||>];

  (* Synthesize diffuse and specular sums if present via direct/indirect *)
  If[!KeyExistsQ[passes, "diffuse"] && KeyExistsQ[passes, "diffuse.direct"] && KeyExistsQ[passes, "diffuse.indirect"],
    passes["diffuse"] = <|"Type" -> "Color", "Data" -> NumericArray[N[Normal[passes["diffuse.direct", "Data"]] + Normal[passes["diffuse.indirect", "Data"]]], "Real32"]|>;
  ];
  If[!KeyExistsQ[passes, "specular"] && KeyExistsQ[passes, "specular.direct"] && KeyExistsQ[passes, "specular.indirect"],
    passes["specular"] = <|"Type" -> "Color", "Data" -> NumericArray[N[Normal[passes["specular.direct", "Data"]] + Normal[passes["specular.indirect", "Data"]]], "Real32"]|>;
  ];

  (* Demodulated Diffuse Irradiance: E_d = diffuse / Max[albedo, 0.02] *)
  If[KeyExistsQ[passes, "diffuse"] && KeyExistsQ[passes, "albedo"],
    diffArr = N @ Normal[passes["diffuse", "Data"]];
    albArr = N @ Normal[passes["albedo", "Data"]];
    irradArr = Clip[diffArr / (0.02 + Ramp[albArr - 0.02]), {0., 15.}];
    computed["DiffuseIrradiance"] = <|"Type" -> "Color", "Data" -> NumericArray[irradArr, "Real32"]|>;
  ];

  (* Scale-free Ordinal Depth in [0, 1] *)
  If[KeyExistsQ[computed, "LogDepth"],
    logZ = N @ Normal[computed["LogDepth", "Data"]];
    {qLow, qHigh} = Quantile[Flatten[logZ], {0.02, 0.98}];
    ordZ = Clip[(qHigh - logZ) / Max[qHigh - qLow, 10^-5], {0., 1.}];
    computed["OrdinalDepth"] = <|"Type" -> "Scalar", "Data" -> NumericArray[ordZ, "Real32"]|>;
  ];

  (* Fallback raytraced AO & roughness *)
  If[KeyExistsQ[computed, "Occlusion"], computed["RaytracedAO"] = computed["Occlusion"]];
  If[KeyExistsQ[passes, "roughness"], computed["Roughness"] = passes["roughness"]];

  result["Passes"] = passes;
  result["Computed"] = computed;

  If[envMap =!= None && KeyExistsQ[result["Passes"], "Nn"],
    envSampleImg = EXRComputed`SampleEnvironment[Normal[result["Passes", "Nn", "Data"]], envMap];
    If[!(FailureQ[envSampleImg] || envSampleImg === $Failed),
      envSampleData = If[ImageQ[envSampleImg], NumericArray[ImageData[envSampleImg, "Real32"], "Real32"], NumericArray[envSampleImg, "Real32"]];
      result["Computed", "EnvironmentSample"] = <|"Type" -> "Scalar", "Data" -> envSampleData|>;
    ];
  ];

  wrapEXR[exr, result]
];

Options[ReadEXR] = {
  "EnvironmentMap" -> None,
  "ComputeDerivedPasses" -> True,
  "AttachCryptomatte" -> True
};

ReadEXR[exr : _Association | _?isEXRDataQ, opts : OptionsPattern[]] := exr;

ReadEXR[files_List, opts : OptionsPattern[]] :=
  Map[ReadEXR[#, opts] &, files];

ReadEXR[f_File, opts : OptionsPattern[]] :=
  ReadEXR[First[f], opts];

ReadEXR[file_String, opts : OptionsPattern[]] := Module[{
  resolved = ExpandFileName[file], channelNames, data, attrs, indexAssoc, passNames, passes, exr, attachCrypto, compDerived
},
  If[!FileExistsQ[resolved],
    Return[Failure["FileNotFound", <|"File" -> file|>]]
  ];

  channelNames = Quiet @ Import[resolved, "ChannelNames"];
  data = Quiet @ Import[resolved, "Data"];
  attrs = Quiet @ Check[Import[resolved, "Attributes"], <||>];

  If[!AssociationQ[channelNames] || !AssociationQ[data],
    Return[Failure["EXRImportFailed", <|"File" -> resolved, "ChannelNames" -> channelNames, "Data" -> data|>]]
  ];

  indexAssoc = AssociationMap[AssociationThread[channelNames[#] -> Range[Length[channelNames[#]]]] &, Keys[channelNames]];
  passNames = Select[Keys[$KnownPassSpecs], hasPassQ[channelNames, #] &];
  passes = AssociationMap[extractPass[data, indexAssoc, #] &, passNames];

  exr = <|
    "File" -> resolved,
    "Passes" -> passes,
    "Computed" -> <||>,
    "Metadata" -> buildMetadata[resolved, If[AssociationQ[attrs], attrs, <||>]]
  |>;

  attachCrypto = OptionValue[ReadEXR, {opts}, "AttachCryptomatte"];
  If[TrueQ[attachCrypto],
    exr = EXRCryptomatte`AttachCryptomatte[exr];
  ];

  compDerived = OptionValue[ReadEXR, {opts}, "ComputeDerivedPasses"];
  MakeEXRData @ If[TrueQ[compDerived],
    addDerivedPasses[exr, OptionValue[ReadEXR, {opts}, "EnvironmentMap"]],
    exr
  ]
];

(* Multi-channel OpenEXR exporter *)
Options[WriteEXR] = {};

WriteEXR[file_String, exr_?isEXRDataQ, opts : OptionsPattern[]] :=
  WriteEXR[file, Normal[exr], opts];

WriteEXR[file_String, assoc_Association, opts : OptionsPattern[]] := Module[{passes = Lookup[assoc, "Passes", <||>], exportAssoc = <||>},
  KeyValueMap[
    Function[{name, pass},
      Module[{arr = Normal[Lookup[pass, "Data", None]]},
        If[ArrayQ[arr],
          exportAssoc[name] = arr;
        ]
      ]
    ],
    passes
  ];
  Export[file, exportAssoc, "EXR"]
];

End[];

EndPackage[];
