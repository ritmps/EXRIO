(* ::Package:: *)

BeginPackage["EXRCryptomatte`"]

ReadCryptomatte::usage="ReadCryptomatte[file] reads Cryptomatte sidecar data and returns an association.";
AttachCryptomatte::usage="AttachCryptomatte[exr] attaches Cryptomatte data to an EXR association when possible.";

CryptomatteMask::usage="CryptomatteMask[crypto, name] returns a soft mask for a named Cryptomatte object.";
CryptomatteMasks::usage="CryptomatteMasks[crypto] returns soft masks for all named Cryptomatte objects.";
CryptomatteSegmentationMap::usage="CryptomatteSegmentationMap[crypto] returns a hard segmentation map and label metadata.";
CryptomatteWinnerCoverage::usage="CryptomatteWinnerCoverage[crypto] returns the winning coverage value at each pixel.";
CryptomatteLabelMask::usage="CryptomatteLabelMask[crypto, label] returns a binary mask for a segmentation label.";
CryptomatteSegmentationImage::usage="CryptomatteSegmentationImage[crypto] returns a colorized segmentation image.";
CryptomatteMaskImage::usage="CryptomatteMaskImage[crypto, name] returns an image for a named soft mask.";

Begin["`Private`"]

isEXRDataQ[expr_] := MatchQ[expr, _[ _Association ]] && MatchQ[Head[expr], Symbol] && (SymbolName[Head[expr]] === "EXRData")
unwrapEXR[expr_] := If[isEXRDataQ[expr], First[expr], expr]
wrapEXR[orig_, newAssoc_] := If[isEXRDataQ[orig], Head[orig][newAssoc], newAssoc]

hexStringToReal32[hex_String]:=Module[
  {int,bytes},
  int=FromDigits[hex,16];
  bytes=IntegerDigits[int,256,4];
  Quiet[First[ImportByteArray[ByteArray[bytes],"Real32",ByteOrdering->1]]]
]

hexStringToReal32[x_]:=x

decodeManifest[manifest_String]:=Module[{decoded},
  decoded=Quiet @ Check[ImportString[manifest,"RawJSON"],$Failed];
  If[AssociationQ[decoded],decoded,<||>]
]

decodeManifest[manifest_Association]:=manifest

decodeManifest[_]:=<||>

combineManifestAssociations[metadataGroups_Association]:=Module[
  {manifests},
  manifests=Cases[
    Values[metadataGroups],
    assoc_Association:>Lookup[assoc,"Manifest",Nothing]
  ];

  If[manifests==={}||!AllTrue[manifests,AssociationQ],
    <||>,
    Merge[manifests,First]
  ]
]

combineManifestAssociations[_]:=<||>

buildNameAndIDMaps[metadataGroups_Association]:=Module[
  {nameToHex,nameToID,idToName},

  nameToHex=combineManifestAssociations[metadataGroups];
  nameToID=Map[hexStringToReal32,nameToHex];
  idToName=Association @ KeyValueMap[#2->#1 &,nameToID];

  <|
    "Manifest"->nameToHex,
    "NameToID"->nameToID,
    "IDToName"->idToName
  |>
]

buildNameAndIDMaps[_]:=<|
  "Manifest"-><||>,
  "NameToID"-><||>,
  "IDToName"-><||>
|>

findCryptomatteSidecar[file_String]:=Module[{dir,base},
  dir=DirectoryName[file];
  base=FileBaseName[file];
  FileNameJoin[{dir,base <> ".crypto.exr"}]
]

extractCryptomatteMetadata[attrs_Association]:=Module[
  {cryptoEntries,parsed,grouped},

  cryptoEntries=Select[
    Normal[attrs],
    StringContainsQ[ToLowerCase[First[#]],"cryptomatte"] &
  ];

  parsed=Cases[
    cryptoEntries,
    (key_String->value_):>Module[{parts,loweredParts,groupKey,field},
      parts=StringSplit[key,"/"];
      loweredParts=ToLowerCase /@ parts;
      If[
        Length[parts]>=3&&First[loweredParts]==="cryptomatte",
        groupKey=parts[[2]];
        field=ToLowerCase[parts[[3]]];
        groupKey-><|field->value|>,
        Nothing
      ]
    ]
  ];

  grouped=GroupBy[parsed,First->Last,Merge[#,Last] &];

  Association @ KeyValueMap[
    #1->If[
      KeyExistsQ[#2,"manifest"],
      Append[#2,"Manifest"->decodeManifest[#2["manifest"]]],
      #2
    ] &,
    grouped
  ]
]

extractCryptomatteMetadata[_]:=<||>

findCryptomatteLayers[channelNames_Association]:=Keys[channelNames]
findCryptomatteLayers[channelNames_List]:=channelNames
findCryptomatteLayers[_]:={}

extractCryptomattePairs[data_Association,layers_List]:=Module[
  {validLayers},
  validLayers=Select[layers,KeyExistsQ[data,#] &];
  Flatten[
    Map[
      Module[{layer,arr},
        layer=#;
        arr=Normal[data[layer]];
        If[!ArrayQ[arr,3]||Dimensions[arr][[3]]!=4,
          {},
          {
            <|
              "Layer"->layer,
              "PairIndex"->1,
              "ID"->arr[[All,All,4]],
              "Coverage"->arr[[All,All,3]]
            |>,
            <|
              "Layer"->layer,
              "PairIndex"->2,
              "ID"->arr[[All,All,2]],
              "Coverage"->arr[[All,All,1]]
            |>
          }
        ]
      ] &,
      validLayers
    ],
    1
  ]
]

extractCryptomattePairs[_,_]:={}

ReadCryptomatte[file_String]:=Module[
  {cryptoFile,attrs,channelNames,data,metadataGroups,layers,pairs,manifest,maps},

  cryptoFile=findCryptomatteSidecar[file];

  If[!FileExistsQ[cryptoFile],
    Return[<|
      "File"->file,
      "CryptomatteFile"->cryptoFile,
      "MetadataGroups"-><||>,
      "Layers"->{},
      "Pairs"->{},
      "Manifest"-><||>,
      "NameToID"-><||>,
      "IDToName"-><||>
    |>]
  ];

  attrs=Quiet @ Check[Import[cryptoFile,"Attributes"],<||>];
  channelNames=Quiet @ Check[Import[cryptoFile,"ChannelNames"],<||>];
  data=Quiet @ Check[Import[cryptoFile,"Data"],<||>];

  metadataGroups=extractCryptomatteMetadata[attrs];
  layers=findCryptomatteLayers[channelNames];
  pairs=extractCryptomattePairs[data,layers];
  manifest=combineManifestAssociations[metadataGroups];
  maps=buildNameAndIDMaps[metadataGroups];

  <|
    "File"->file,
    "CryptomatteFile"->cryptoFile,
    "Attributes"->attrs,
    "ChannelNames"->channelNames,
    "Data"->data,
    "MetadataGroups"->metadataGroups,
    "Layers"->layers,
    "Pairs"->pairs,
    "Manifest"->manifest,
    "NameToID"->maps["NameToID"],
    "IDToName"->maps["IDToName"]
  |>
]

AttachCryptomatte[exr : _Association | _?isEXRDataQ] := Module[{file, crypto, assoc},
  assoc = unwrapEXR[exr];
  file=Lookup[assoc,"File",Missing["KeyAbsent","File"]];
  If[!StringQ[file],Return[exr]];
  crypto=ReadCryptomatte[file];
  wrapEXR[exr, Append[assoc,"Cryptomatte"->crypto]]
]

AttachCryptomatte[expr_]:=expr

CryptomatteMask[crypto_Association,name_String,tolerance_ : 10^-6]:=Module[
  {targetID,pairs},
  targetID=Lookup[Lookup[crypto,"NameToID",<||>],name,Missing["UnknownName",name]];
  pairs=Lookup[crypto,"Pairs",{}];

  If[MissingQ[targetID]||pairs==={},Return[$Failed]];

  Total[
    Map[
      UnitStep[tolerance - Abs[#["ID"] - targetID]] * #["Coverage"] &,
      pairs
    ]
  ]
]

CryptomatteMasks[crypto_Association]:=AssociationMap[
  CryptomatteMask[crypto,#] &,
  Keys @ Lookup[crypto,"NameToID",<||>]
]

CryptomatteWinnerCoverage[crypto_Association]:=Module[
  {pairs,coverages},
  pairs=Lookup[crypto,"Pairs",{}];
  If[pairs==={},Return[$Failed]];

  coverages=Lookup[pairs,"Coverage"];
  Apply[MapThread[Max,{##},2] &,coverages]
]

CryptomatteSegmentationMap[crypto_Association]:=Module[
  {pairs,ids,coverages,bestIndex,segIDs,uniqueIDs,idToLabel,labelToName,seg},

  pairs=Lookup[crypto,"Pairs",{}];
  If[pairs==={},Return[$Failed]];

  ids=Lookup[pairs,"ID"];
  coverages=Lookup[pairs,"Coverage"];

  bestIndex=Apply[
    MapThread[First @ Ordering[{##},-1] &,{##},2] &,
    coverages
  ];

  segIDs=MapIndexed[
    With[{i=#2[[1]],j=#2[[2]],k=#1},
      If[
        Max[Map[Part[#,i,j] &,coverages]]<=0.,
        0.,
        ids[[k,i,j]]
      ]
    ] &,
    bestIndex,
    {2}
  ];

  uniqueIDs=DeleteCases[DeleteDuplicates @ Flatten[segIDs],0.];

  idToLabel=AssociationThread[uniqueIDs,Range[Length[uniqueIDs]]];

  labelToName=Association @ KeyValueMap[
    #2->Lookup[Lookup[crypto,"IDToName",<||>],#1,ToString[#1]] &,
    idToLabel
  ];

  seg=Map[Lookup[idToLabel,#,0] &,segIDs,{2}];

  <|
    "SegmentationMap"->seg,
    "SegmentIDs"->segIDs,
    "IDToLabel"->idToLabel,
    "LabelToName"->labelToName
  |>
]

cryptomatteLabelAssociation[crypto_Association]:=AssociationThread[
  Range[Length @ Lookup[crypto,"IDToName",<||>]],
  Values @ Lookup[crypto,"IDToName",<||>]
]

CryptomatteLabelMask[crypto_Association,label_Integer]:=Module[
  {seg},
  seg=CryptomatteSegmentationMap[crypto];
  If[seg===$Failed,Return[$Failed]];
  1 - Unitize[seg["SegmentationMap"] - label]
]

CryptomatteSegmentationImage[crypto_Association]:=Module[
  {seg},
  seg=CryptomatteSegmentationMap[crypto];
  If[seg===$Failed,Return[$Failed]];
  Image @ Colorize[seg["SegmentationMap"]]
]

CryptomatteMaskImage[crypto_Association,name_String,tolerance_ : 10^-6]:=Module[
  {mask},
  mask=CryptomatteMask[crypto,name,tolerance];
  If[mask===$Failed,Return[$Failed]];
  Image[mask]
]

End[]

EndPackage[]


(* ::ChatInput:: *)
(*I think too many things are exposed here, API wise. Should we clean this up a bit? The main use of this file is via EXRIO.wl in this same directory*)
