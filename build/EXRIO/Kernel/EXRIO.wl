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
$EXRIOVersion = "2.1.18";

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
  {group <> "." <> sub, {"R", "G", "B"}},
  {group, sub <> "." <> # & /@ {"R", "G", "B"}},
  {group, sub <> "." <> # & /@ {"Channel1", "Channel2", "Channel3"}},
  {group <> "." <> sub, {"Channel1", "Channel2", "Channel3"}},
  {"lighting", sub <> "." <> group <> "." <> # & /@ {"r", "g", "b"}}
};

geomCandidates[name_String] := {
  {"geometry." <> name, {"X", "Y", "Z"}},
  {"geometry." <> name, {"R", "G", "B"}},
  {name, {"X", "Y", "Z"}},
  {name, {"R", "G", "B"}},
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

(* Multi-channel OpenEXR exporter (standard OpenEXR 2.0 scanline format) *)
writeEXRAttr[stream_, name_String, type_String, payload_ByteArray] := Module[{
  safeName = StringTake[name, UpTo[31]],
  nameBytes, typeBytes
},
  nameBytes = ToCharacterCode[safeName, "UTF-8"];
  typeBytes = ToCharacterCode[type, "UTF-8"];
  BinaryWrite[stream, nameBytes, "Byte"];
  BinaryWrite[stream, 0, "Byte"];
  BinaryWrite[stream, typeBytes, "Byte"];
  BinaryWrite[stream, 0, "Byte"];
  BinaryWrite[stream, Length[payload], "Integer32", ByteOrdering -> -1];
  BinaryWrite[stream, payload];
];

int32Bytes[vals_List] := ByteArray[Flatten[IntegerDigits[#, 256, 4] // Reverse & /@ vals]];

writeStandardEXR[outFile_String, channelAssoc_Association, strAttrs_Association] := Module[{
  names = Sort[Keys[channelAssoc]],
  firstArr, height, width, numChans,
  chlistStream, chlistBytes,
  dwBytes, zeroBytes4 = {0, 0, 0, 0},
  stream, offsetTablePos, dataStartPos, pixelDataSize, stride, offsets,
  chanCube, y
},
  If[names === {}, Return[$Failed]];
  firstArr = channelAssoc[First[names]];
  {height, width} = Dimensions[firstArr];
  numChans = Length[names];

  (* Build chlist attribute payload (channel names <= 31 bytes for OpenEXR 2.0 compatibility) *)
  chlistBytes = ByteArray[Flatten[{
    Table[
      {
        ToCharacterCode[StringTake[cname, UpTo[31]], "UTF-8"], 0,
        {2, 0, 0, 0}, (* pixelType = FLOAT (2) *)
        {0, 0, 0, 0}, (* pLinear=0, reserved 3 bytes *)
        {1, 0, 0, 0}, (* xSampling = 1 *)
        {1, 0, 0, 0}  (* ySampling = 1 *)
      },
      {cname, names}
    ],
    0
  }]];

  (* Pack {numChans, height, width} -> {height, numChans, width} Real32 *)
  chanCube = Developer`ToPackedArray[
    Transpose[Developer`ToPackedArray[N[channelAssoc /@ names], Real], {2, 1, 3}],
    Real
  ];

  stream = OpenWrite[outFile, BinaryFormat -> True];
  If[stream === $Failed, Return[$Failed]];

  (* Magic (0x01312f76) + Version 2 (single-part scanline) *)
  BinaryWrite[stream, {118, 47, 49, 1, 2, 0, 0, 0}, "Byte"];

  (* Required OpenEXR header attributes *)
  writeEXRAttr[stream, "channels", "chlist", chlistBytes];
  writeEXRAttr[stream, "compression", "compression", ByteArray[{0}]]; (* NO_COMPRESSION *)

  (* dataWindow & displayWindow: box2i {0, 0, width-1, height-1} *)
  dwBytes = Module[{tmp = OpenWrite[BinaryFormat -> True]},
    Close[tmp];
    ByteArray @ Flatten[
      Function[v, IntegerDigits[BitAnd[v, 4294967295], 256, 4] // Reverse] /@ {0, 0, width - 1, height - 1}
    ]
  ];
  writeEXRAttr[stream, "dataWindow", "box2i", dwBytes];
  writeEXRAttr[stream, "displayWindow", "box2i", dwBytes];
  writeEXRAttr[stream, "lineOrder", "lineOrder", ByteArray[{0}]]; (* INCREASING_Y *)

  (* pixelAspectRatio = 1.0f, screenWindowCenter = {0,0}, screenWindowWidth = 1.0f *)
  writeEXRAttr[stream, "pixelAspectRatio", "float", ByteArray[{0, 0, 128, 63}]];
  writeEXRAttr[stream, "screenWindowCenter", "v2f", ByteArray[{0, 0, 0, 0, 0, 0, 0, 0}]];
  writeEXRAttr[stream, "screenWindowWidth", "float", ByteArray[{0, 0, 128, 63}]];

  (* Optional OpenEXR preview thumbnail from R, G, B[, A] *)
  If[KeyExistsQ[channelAssoc, "R"] && KeyExistsQ[channelAssoc, "G"] && KeyExistsQ[channelAssoc, "B"],
    Module[{maxDim = 128, pw, ph, rImg, aImg, rgbData, rgbaBytes, prevBytes},
      If[width >= height,
        pw = Min[width, maxDim]; ph = Max[1, Round[height * (pw / N[width])]],
        ph = Min[height, maxDim]; pw = Max[1, Round[width * (ph / N[height])]]
      ];
      rgbData = Transpose[{channelAssoc["R"], channelAssoc["G"], channelAssoc["B"]}, {3, 1, 2}];
      rgbData = Clip[N[rgbData], {0.0, 1.0}]^(1.0 / 2.2);
      rImg = ImageResize[Image[rgbData, "Real32"], {pw, ph}];
      aImg = If[KeyExistsQ[channelAssoc, "A"],
        ImageResize[Image[Clip[N[channelAssoc["A"]], {0.0, 1.0}], "Real32"], {pw, ph}],
        Image[ConstantArray[1.0, {ph, pw}], "Real32"]
      ];
      rgbaBytes = Flatten[ImageData[SetAlphaChannel[rImg, aImg], "Byte"]];
      prevBytes = ByteArray[Join[
        IntegerDigits[pw, 256, 4] // Reverse,
        IntegerDigits[ph, 256, 4] // Reverse,
        rgbaBytes
      ]];
      writeEXRAttr[stream, "preview", "preview", prevBytes];
    ]
  ];

  (* Custom string metadata *)
  KeyValueMap[
    Function[{k, v},
      If[StringQ[k] && StringQ[v],
        writeEXRAttr[stream, k, "string", ByteArray[ToCharacterCode[v, "UTF-8"]]]
      ]
    ],
    strAttrs
  ];

  (* End of header *)
  BinaryWrite[stream, 0, "Byte"];

  (* Scanline offset table *)
  offsetTablePos = StreamPosition[stream];
  pixelDataSize = numChans * width * 4;
  stride = 8 + pixelDataSize;
  dataStartPos = offsetTablePos + height * 8;
  offsets = Table[dataStartPos + (y - 1) * stride, {y, 1, height}];
  BinaryWrite[stream, offsets, "Integer64", ByteOrdering -> -1];

  (* Scanline blocks *)
  Do[
    BinaryWrite[stream, {y - 1, pixelDataSize}, "Integer32", ByteOrdering -> -1];
    BinaryWrite[stream, Flatten[chanCube[[y]]], "Real32", ByteOrdering -> -1],
    {y, 1, height}
  ];

  Close[stream];
  outFile
];

Options[WriteEXR] = {};

WriteEXR[file_String, exr_?isEXRDataQ, opts : OptionsPattern[]] :=
  WriteEXR[file, Normal[exr], opts];

WriteEXR[file_String, assoc_Association, opts : OptionsPattern[]] := Module[{
  passes = Lookup[assoc, "Passes", <||>],
  meta = Lookup[assoc, "Metadata", <||>],
  modelInfo, exrAttrs = <||>, layerSpecs,
  channelAssoc = <||>, outFile = ExpandFileName[file], outDir
},
  layerSpecs = {};
  KeyValueMap[
    Function[{name, pass},
      Module[{raw = Lookup[pass, "Data", None], passType = Lookup[pass, "Type", Automatic], arr, dims, nCh, heck, rman},
        If[MatchQ[raw, _NumericArray] || ArrayQ[raw],
          arr = N[Normal[raw]];
          dims = Dimensions[arr];
          Which[
            Length[dims] == 2,
              channelAssoc[name] = arr,
            Length[dims] == 3,
              nCh = dims[[3]];
              Which[
                name === "beauty",
                  If[nCh >= 1, channelAssoc["R"] = arr[[All, All, 1]]];
                  If[nCh >= 2, channelAssoc["G"] = arr[[All, All, 2]]];
                  If[nCh >= 3, channelAssoc["B"] = arr[[All, All, 3]]];
                  If[nCh >= 4, channelAssoc["A"] = arr[[All, All, 4]]],
                passType === "Vector3" || name === "geometry.Nn" || name === "Nn",
                  If[nCh >= 1, channelAssoc[name <> ".X"] = arr[[All, All, 1]]];
                  If[nCh >= 2, channelAssoc[name <> ".Y"] = arr[[All, All, 2]]];
                  If[nCh >= 3, channelAssoc[name <> ".Z"] = arr[[All, All, 3]]],
                nCh == 1,
                  channelAssoc[name] = arr[[All, All, 1]],
                True,
                  If[nCh >= 1, channelAssoc[name <> ".R"] = arr[[All, All, 1]]];
                  If[nCh >= 2, channelAssoc[name <> ".G"] = arr[[All, All, 2]]];
                  If[nCh >= 3, channelAssoc[name <> ".B"] = arr[[All, All, 3]]];
                  If[nCh >= 4, channelAssoc[name <> ".A"] = arr[[All, All, 4]]]
              ]
          ]
        ];
        If[AssociationQ[pass],
          heck = Lookup[pass, "Heckbert", None];
          rman = Lookup[pass, "RenderManLPE", None];
          If[(StringQ[heck] && StringLength[heck] > 0) || (StringQ[rman] && StringLength[rman] > 0),
            AppendTo[layerSpecs, name <> ": heckbert=" <> ToString[Replace[heck, Except[_String] -> ""]] <> ", renderman=" <> ToString[Replace[rman, Except[_String] -> ""]]];
          ];
        ];
      ]
    ],
    passes
  ];
  If[Length[layerSpecs] > 0,
    exrAttrs["lpe_layers"] = StringRiffle[layerSpecs, "; "];
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
  writeStandardEXR[outFile, channelAssoc, exrAttrs]
];

End[];

EndPackage[];
