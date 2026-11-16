(* ::Package:: *)

Module[{dir = DirectoryName[$InputFileName]},
  If[StringQ[dir] && dir =!= "",
    If[!MemberQ[$Packages, "EXRCryptomatte`"], Get[FileNameJoin[{dir, "EXRCryptomatte.wl"}]]];
    If[!MemberQ[$Packages, "EXRComputed`"], Get[FileNameJoin[{dir, "EXRComputed.wl"}]]];
    If[!MemberQ[$Packages, "EXRBoxes`"], Get[FileNameJoin[{dir, "EXRBoxes.wl"}]]];
    If[!MemberQ[$Packages, "EXRPreview`"], Get[FileNameJoin[{dir, "EXRPreview.wl"}]]];
    If[!MemberQ[$Packages, "EXRCamera`"], Get[FileNameJoin[{dir, "EXRCamera.wl"}]]];
    If[!MemberQ[$Packages, "ImageMetrics`"], Get[FileNameJoin[{dir, "ImageMetrics.wl"}]]];
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
$EXRIOVersion = "2.1.13";

ReadEXR::usage = "ReadEXR[file, opts] imports a multi-channel OpenEXR file into a structured EXRData container with differential geometry and metadata.";
WriteEXR::usage = "WriteEXR[file, exr, opts] exports an EXRData container or association of passes to a multi-channel OpenEXR file.";
AddComputedPass::usage = "AddComputedPass[exr, name, arrayOrFn, opts] stores an ad-hoc computed pass under \"Computed\"[name] in an EXRData container.\nAddComputedPass[name -> fn, opts][exr] is the operator form for pipeline composition.";
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
    If[size === EndOfFile || !IntegerQ[size], Break[]];

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

parseFilenameMetadata[path_String] := Module[{base = FileBaseName[path], cases},
  base = StringDelete[base, ".crypto"];
  cases = StringCases[base, "scene_" ~~ s : DigitCharacter.. ~~ "_view_" ~~ v : DigitCharacter.. :> <|"SceneID" -> ToExpression[s], "ViewID" -> ToExpression[v]|>];
  If[cases =!= {}, Return[First[cases]]];
  cases = StringCases[base, "object_" ~~ j : DigitCharacter.. ~~ "_mat_" ~~ mat : Except["_"].. ~~ "_pose_" ~~ k : DigitCharacter.. ~~ "_view_" ~~ i : DigitCharacter.. :> <|"ObjectID" -> ToExpression[j], "MaterialName" -> mat, "PoseID" -> ToExpression[k], "ViewID" -> ToExpression[i]|>];
  If[cases =!= {}, First[cases], <||>]
];

buildMetadata[path_String, attrs_Association] := Module[{binAttrs, filenameMeta},
  binAttrs = readBinaryEXRHeaderAttributes[path];
  filenameMeta = parseFilenameMetadata[path];
  <|
    "RawAttributes" -> attrs,
    "Camera" -> <|
      "FOV" -> Lookup[attrs, "Fov", Lookup[binAttrs, "fov", Missing["KeyAbsent", "Fov"]]],
      "Eye" -> Lookup[attrs, "Cam_eye", Lookup[binAttrs, "cam_eye", Missing["KeyAbsent", "Cam_eye"]]],
      "At" -> Lookup[attrs, "Cam_at", Lookup[binAttrs, "cam_at", Missing["KeyAbsent", "Cam_at"]]],
      "Up" -> Lookup[attrs, "Cam_up", Lookup[binAttrs, "cam_up", Missing["KeyAbsent", "Cam_up"]]],
      "WorldToCamera" -> Lookup[binAttrs, "worldToCamera", Missing["KeyAbsent", "worldToCamera"]],
      "WorldToNDC" -> Lookup[binAttrs, "worldToNDC", Missing["KeyAbsent", "worldToNDC"]]
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

colorCandidates[name_String] := {
  {name, {"R", "G", "B"}},
  {name, {"r", "g", "b"}},
  {name, {"Channel1", "Channel2", "Channel3"}}
};

lpeCandidates[group_String, sub_String] := {
  {group, sub <> "." <> # & /@ {"Channel1", "Channel2", "Channel3"}},
  {group <> "." <> sub, {"Channel1", "Channel2", "Channel3"}},
  {"lighting", sub <> "." <> group <> "." <> # & /@ {"r", "g", "b"}}
};

geomCandidates[name_String] := {
  {"geometry", name <> "." <> # & /@ {"Channel1", "Channel2", "Channel3"}},
  {"geometry." <> name, {"Channel1", "Channel2", "Channel3"}},
  {name, {"Channel1", "Channel2", "Channel3"}},
  {"geometry", If[name === "NL", {"NL.r", "NL.g", "NL.b"}, name <> "." <> # & /@ {"x", "y", "z"}]}
};

$KnownPassSpecs = <|
  "beauty" -> <|"Type" -> "Color", "Candidates" -> Join[colorCandidates["denoised"], colorCandidates["beauty"], colorCandidates["default"]]|>,
  "denoised" -> <|"Type" -> "Color", "Candidates" -> colorCandidates["denoised"]|>,
  "albedo" -> <|"Type" -> "Color", "Candidates" -> colorCandidates["albedo"]|>,
  "diffuse" -> <|"Type" -> "Color", "Candidates" -> colorCandidates["diffuse"]|>,
  "diffuse_irrad" -> <|"Type" -> "Color", "Candidates" -> colorCandidates["diffuse_irrad"]|>,
  "recon_beauty" -> <|"Type" -> "Color", "Candidates" -> colorCandidates["recon_beauty"]|>,
  "specular" -> <|"Type" -> "Color", "Candidates" -> colorCandidates["specular"]|>,
  "diffuse.direct" -> <|"Type" -> "Color", "Candidates" -> lpeCandidates["diffuse", "direct"]|>,
  "diffuse.indirect" -> <|"Type" -> "Color", "Candidates" -> lpeCandidates["diffuse", "indirect"]|>,
  "specular.direct" -> <|"Type" -> "Color", "Candidates" -> lpeCandidates["specular", "direct"]|>,
  "specular.indirect" -> <|"Type" -> "Color", "Candidates" -> lpeCandidates["specular", "indirect"]|>,
  "A" -> <|"Type" -> "Scalar", "Candidates" -> {{"A", {"Channel1"}}, {"default", {"A"}}}|>,
  "Z" -> <|"Type" -> "Scalar", "Candidates" -> {{"Z", {"Channel1"}}, {"default", {"Z"}}}|>,
  "sampleCount" -> <|"Type" -> "Scalar", "Candidates" -> {{"sampleCount", {"Channel1"}}, {"default", {"sampleCount"}}}|>,
  "roughness" -> <|"Type" -> "Scalar", "Candidates" -> {{"roughness", {"Channel1"}}, {"roughness", {"roughness"}}, {"roughness", {"z"}}, {"roughness", {"Z"}}, {"default", {"roughness"}}}|>,
  "P" -> <|"Type" -> "Vector3", "Candidates" -> geomCandidates["P"]|>,
  "Po" -> <|"Type" -> "Vector3", "Candidates" -> geomCandidates["Po"]|>,
  "Nn" -> <|"Type" -> "Vector3", "Candidates" -> geomCandidates["Nn"]|>,
  "NL" -> <|"Type" -> "Vector3", "Candidates" -> geomCandidates["NL"]|>,
  "dPdu" -> <|"Type" -> "Vector3", "Candidates" -> geomCandidates["dPdu"]|>,
  "dPdv" -> <|"Type" -> "Vector3", "Candidates" -> geomCandidates["dPdv"]|>
|>;

resolvePassSpec[indexAssoc_Association, passName_String] := Module[{spec, match},
  spec = Lookup[$KnownPassSpecs, passName, Missing["UnknownPass", passName]];
  If[MissingQ[spec], Return[Missing["UnknownPass", passName]]];
  match = SelectFirst[
    spec["Candidates"],
    KeyExistsQ[indexAssoc, #[[1]]] && SubsetQ[Keys[indexAssoc[#[[1]]]], #[[2]]] &,
    Missing["Absent", passName]
  ];
  If[MissingQ[match], match, <|spec, "Group" -> match[[1]], "Channels" -> match[[2]]|>]
];

sliceChannels[array_, positions_List] :=
  If[Length[positions] == 1,
    Normal[array[[All, All, positions[[1]]]]],
    Normal[array[[All, All, positions]]]
  ];

extractPass[data_Association, indexAssoc_Association, passName_String] := Module[{
  resolved, group, idx, positions, arr, pArr, signFlip
},
  resolved = resolvePassSpec[indexAssoc, passName];
  If[MissingQ[resolved], Return[Nothing]];
  group = resolved["Group"];
  idx = indexAssoc[group];
  positions = idx /@ resolved["Channels"];
  arr = sliceChannels[data[group], positions];
  If[passName === "Nn" && SubsetQ[Keys[idx], {"P.x", "P.y", "P.z"}],
    pArr = sliceChannels[data[group], idx /@ {"P.x", "P.y", "P.z"}];
    signFlip = 1.0 - 2.0 * UnitStep[Total[pArr * arr, {3}]];
    arr = arr * Transpose[ConstantArray[signFlip, 3], {3, 1, 2}];
  ];
  <|"Type" -> resolved["Type"], "Data" -> NumericArray[arr, "Real32"]|>
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
  "AttachCryptomatte" -> True,
  "Passes" -> All
};

ReadEXR[exr : _Association | _?isEXRDataQ, opts : OptionsPattern[]] := exr;

ReadEXR[files_List, opts : OptionsPattern[]] :=
  Map[ReadEXR[#, opts] &, files];

ReadEXR[f_File, opts : OptionsPattern[]] :=
  ReadEXR[First[f], opts];

ReadEXR[file_String, opts : OptionsPattern[]] := Module[{
  resolved = ExpandFileName[file], imported, channelNames, data, attrs,
  indexAssoc, requestedPasses, passes, exr, attachCrypto, compDerived
},
  If[!FileExistsQ[resolved],
    Return[Failure["FileNotFound", <|"File" -> file|>]]
  ];

  imported = Quiet @ Import[resolved, {"OpenEXR", {"ChannelNames", "Data", "Attributes"}}];
  If[!ListQ[imported] || Length[imported] =!= 3 || !AssociationQ[imported[[1]]] || !AssociationQ[imported[[2]]],
    Return[Failure["EXRImportFailed", <|"File" -> resolved, "Imported" -> imported|>]]
  ];
  {channelNames, data, attrs} = imported;

  indexAssoc = AssociationMap[AssociationThread[channelNames[#] -> Range[Length[channelNames[#]]]] &, Keys[channelNames]];
  requestedPasses = OptionValue[ReadEXR, {opts}, "Passes"];
  If[requestedPasses === All,
    requestedPasses = Keys[$KnownPassSpecs],
    requestedPasses = DeleteDuplicates @ Select[Flatten[{requestedPasses}], StringQ]
  ];
  passes = Select[
    AssociationMap[extractPass[data, indexAssoc, #] &, requestedPasses],
    AssociationQ
  ];

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

(* Ad-hoc computed pass insertion *)
Options[AddComputedPass] = {"Type" -> Automatic};

AddComputedPass[name_String -> arrayOrFn_, opts : OptionsPattern[]][exr : _Association | _?isEXRDataQ] :=
  AddComputedPass[exr, name, arrayOrFn, opts];

AddComputedPass[exr : _Association | _?isEXRDataQ, name_String, arrayOrFn_, opts : OptionsPattern[]] := Module[{
  assoc = unwrapEXR[exr], val, arr, dims, passType, reqType, computed
},
  val = If[MatchQ[arrayOrFn, _List | _NumericArray | _Image], arrayOrFn, arrayOrFn[exr]];
  If[ImageQ[val],
    arr = ImageData[val, "Real32"],
    If[MatchQ[val, _NumericArray],
      arr = Normal[val],
      arr = val
    ]
  ];
  If[!ArrayQ[arr, _, NumericQ] || !MemberQ[{2, 3}, ArrayDepth[arr]],
    Return[Failure["InvalidPassData", <|"Name" -> name, "Depth" -> ArrayDepth[arr]|>]]
  ];
  dims = Dimensions[arr];
  reqType = OptionValue[AddComputedPass, {opts}, "Type"];
  passType = If[reqType =!= Automatic,
    reqType,
    Which[
      Length[dims] == 2, "Scalar",
      dims[[3]] == 3, "Color",
      True, "Vector" <> ToString[dims[[3]]]
    ]
  ];
  computed = Lookup[assoc, "Computed", <||>];
  computed[name] = <|
    "Type" -> passType,
    "Data" -> If[MatchQ[val, _NumericArray] && NumericArrayType[val] === "Real32", val, NumericArray[N[arr], "Real32"]]
  |>;
  assoc["Computed"] = computed;
  wrapEXR[exr, assoc]
];

(* Multi-channel OpenEXR exporter *)
Options[WriteEXR] = {};

WriteEXR[file_String, exr_?isEXRDataQ, opts : OptionsPattern[]] :=
  WriteEXR[file, Normal[exr], opts];

WriteEXR[file_String, assoc_Association, opts : OptionsPattern[]] := Module[{
  passes = Lookup[assoc, "Passes", <||>],
  meta = Lookup[assoc, "Metadata", <||>],
  modelInfo, exrAttrs = <||>,
  exportAssoc = <||>, outFile = ExpandFileName[file], outDir
},
  KeyValueMap[
    Function[{name, pass},
      Module[{raw = Lookup[pass, "Data", None], arr, heck, rman},
        If[MatchQ[raw, _NumericArray] || ArrayQ[raw],
          arr = If[MatchQ[raw, _NumericArray] && NumericArrayType[raw] === "Real32",
            raw,
            NumericArray[Normal[raw], "Real32"]
          ];
          exportAssoc[name] = arr;
        ];
        If[AssociationQ[pass],
          heck = Lookup[pass, "Heckbert", None];
          rman = Lookup[pass, "RenderManLPE", None];
          If[StringQ[heck] && StringLength[heck] > 0,
            exrAttrs["lpe_layer_" <> name <> "_heckbert"] = heck;
          ];
          If[StringQ[rman] && StringLength[rman] > 0,
            exrAttrs["lpe_layer_" <> name <> "_renderman"] = rman;
          ];
        ];
      ]
    ],
    passes
  ];
  If[AssociationQ[meta],
    If[StringQ[Lookup[meta, "Provenance", None]],
      exrAttrs["lpe_provenance"] = meta["Provenance"];
    ];
    modelInfo = Lookup[meta, "Model", <||>];
    If[AssociationQ[modelInfo] && Length[modelInfo] > 0,
      If[StringQ[Lookup[modelInfo, "PacletVersion", None]],
        exrAttrs["lpe_version"] = modelInfo["PacletVersion"];
      ];
      If[StringQ[Lookup[modelInfo, "Source", None]],
        exrAttrs["lpe_model_source"] = modelInfo["Source"];
      ];
      If[StringQ[Lookup[modelInfo, "Path", None]],
        exrAttrs["lpe_model_path"] = modelInfo["Path"];
      ];
      If[DateObjectQ[Lookup[modelInfo, "Date", None]],
        exrAttrs["lpe_model_date"] = DateString[modelInfo["Date"], "ISODateTime"];
      ];
    ];
  ];
  outDir = DirectoryName[outFile];
  If[StringLength[outDir] > 0 && !DirectoryQ[outDir],
    CreateDirectory[outDir, CreateIntermediateDirectories -> True]
  ];
  If[Length[exrAttrs] > 0,
    Export[outFile, exportAssoc, "OpenEXR", "Attributes" -> exrAttrs],
    Export[outFile, exportAssoc, "OpenEXR"]
  ]
];

End[];

EndPackage[];
