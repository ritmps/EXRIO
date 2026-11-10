(* ::Package:: *)

BeginPackage["EXRPreview`", {"EXRBoxes`"}]

PreviewEXR::usage="PreviewEXR[exr] makes a preview grid for available passes.";

Begin["`Private`"]

isEXRDataQ[expr_] := MatchQ[expr, _[ _Association ]] && MatchQ[Head[expr], Symbol] && (SymbolName[Head[expr]] === "EXRData");
unwrapEXR[expr_] := If[isEXRDataQ[expr], First[expr], expr];

signedScalarPassQ[name_String]:=MemberQ[{"k1","k2","H","K","ContrastOfCurvature"},name];
positiveScalarPassQ[name_String]:=MemberQ[{"Curvedness","A","Z","sampleCount"},name];

finiteValues[data_]:=Select[
  Flatten @ N @ Normal[data],
  NumericQ[#]&&#=!=Indeterminate&&#=!=Infinity&&#=!=-Infinity &
]

robustAbsScale[data_,q_: 0.98,fallback_: 1.*^-6]:=Module[{vals,s},
  vals=finiteValues[data];
  If[vals==={},fallback,s=Quantile[Abs[vals],q]; Max[s,fallback]]
]

robustPositiveScale[data_,q_: 0.98,fallback_: 1.*^-6]:=Module[{vals,s},
  vals=Select[finiteValues[data],#>=0 &];
  If[vals==={},fallback,s=Quantile[vals,q]; Max[s,fallback]]
]

makeSignedRainbowPreview[data_,imageSize_,q_: 0.98]:=Module[{arr,s,cf},
  arr=N @ Normal[data];
  s=robustAbsScale[arr,q];

  cf=Function[x,
    Blend[
      {
        RGBColor[0.3,0.0,0.7],
        RGBColor[0.0,0.3,1.0],
        RGBColor[0.5,1.0,1.0],
        RGBColor[1.0,1.0,1.0],
        RGBColor[1.0,1.0,0.3],
        RGBColor[1.0,0.5,0.0],
        RGBColor[0.8,0.0,0.0]
      },
      x
    ]
  ];

  ImageResize[
    Colorize[
      Image[Rescale[Clip[arr,{-s,s}],{-s,s},{0.,1.}],"Real32"],
      ColorFunction->cf,
      ColorFunctionScaling->False
    ],
    imageSize
  ]
]

makePositivePreview[data_,imageSize_,q_: 0.98]:=Module[{arr,s,mapped},
  arr=Clip[N @ Normal[data],{0.,Infinity}];
  s=robustPositiveScale[arr,q];
  mapped=Log[1.+9. Clip[arr,{0.,s}]/s]/Log[10.];
  ImageResize[
    Colorize[
      Image[mapped,"Real32"],
      ColorFunction->"GrayTones",
      ColorFunctionScaling->False
    ],
    imageSize
  ]
]

makeShapeIndexPreview[data_,imageSize_]:=Module[{arr},
  arr=Clip[N @ Normal[data],{-1.,1.}];
  ImageResize[
    Colorize[
      Image[arr,"Real32"],
      ColorFunction->Function[x,Blend[{Blue,Cyan,Green,Yellow,Orange,Red},Rescale[x,{-1.,1.}]]],
      ColorFunctionScaling->False
    ],
    imageSize
  ]
]

makeDefaultScalarPreview[data_,imageSize_]:=Module[{arr},
  arr=N @ Normal[data];
  ImageResize[
    ImageAdjust @ Image[arr,"Real32",ColorSpace->"Grayscale"],
    imageSize
  ]
]

makeLabelPreview[data_,imageSize_]:=Module[{arr},
  arr=Round @ Normal[data];
  ImageResize[Colorize[arr],imageSize]
]

cryptomattePreviewCells[exr : _Association | _?isEXRDataQ,imageSize_ : 180,maxObjects_ : 6]:=Module[
  {crypto,labelImg,names,maskCells,labelCell={},selectedNames,assoc},
  
  assoc=unwrapEXR[exr];
  crypto=Lookup[assoc,"Cryptomatte",Missing["NoCryptomatte"]];
  If[MissingQ[crypto],Return[{}]];
  
  labelImg=CryptomatteLabelImage[assoc];
  If[MatchQ[labelImg,_Image],
    labelCell={
      Labeled[
        ImageResize[labelImg,imageSize],
        Style["Cryptomatte Labels",12,Bold],
        Top
      ]
    }
  ];
  
  names=Keys @ Lookup[crypto,"NameToID",<||>];
  selectedNames=Take[names,UpTo[maxObjects]];
  
  maskCells=Replace[
    Table[
      Module[{maskImg},
        maskImg=CryptomatteObjectMask[assoc,name,"Return"->"Image"];
        If[MatchQ[maskImg,_Image],
          Labeled[
            ImageResize[ImageAdjust[maskImg],imageSize],
            Style["Crypto: " <> name,12,Bold],
            Top
          ],
          Nothing
        ]
      ],
      {name,selectedNames}
    ],
    Nothing->Sequence[],
    {1}
  ];
  
  Join[labelCell,maskCells]
]

previewImage[name_String,passAssoc_Association,imageSize_: 180]:=Module[{arr},
  Which[
    KeyExistsQ[passAssoc,"Data"],
      arr=Normal[passAssoc["Data"]];
      Which[
        ArrayDepth[arr]==2&&Lookup[passAssoc,"Type",None]==="Label",
          makeLabelPreview[arr,imageSize],
        ArrayDepth[arr]==2&&name==="ShapeIndex",
          makeShapeIndexPreview[arr,imageSize],
        ArrayDepth[arr]==2&&signedScalarPassQ[name],
          makeSignedRainbowPreview[arr,imageSize],
        ArrayDepth[arr]==2&&positiveScalarPassQ[name],
          makePositivePreview[arr,imageSize],
        ArrayDepth[arr]==2,
          makeDefaultScalarPreview[arr,imageSize],
        ArrayDepth[arr]==3&&Last[Dimensions[arr]]==3,
          ImageResize[ImageAdjust @ Image[arr,"Real32"],imageSize],
        True,
          Style["not previewable",Gray]
      ],
    KeyExistsQ[passAssoc,"Image"]&&MatchQ[passAssoc["Image"],_Image],
      ImageResize[ImageAdjust[passAssoc["Image"]],imageSize],
    True,
      Style["not previewable",Gray]
  ]
]

PreviewEXR[inputs_List, imageSize_ : 180] :=
  Map[PreviewEXR[#, imageSize] &, inputs];

PreviewEXR[f_File, imageSize_ : 180] :=
  PreviewEXR[First[f], imageSize];

PreviewEXR[file_String, imageSize_ : 180] :=
  Module[{exr = Quiet @ Check[EXRIO`ReadEXR[file], $Failed]},
    If[exr =!= $Failed, PreviewEXR[exr, imageSize], $Failed]
  ];

PreviewEXR[img_Image, imageSize_ : 180] :=
  Module[{exr = Quiet @ Check[EXRIO`ReadEXR[img], $Failed]},
    If[exr =!= $Failed, PreviewEXR[exr, imageSize], $Failed]
  ];

PreviewEXR[exr : _Association | _?isEXRDataQ,imageSize_ : 180]:=Module[
  {passes,names,cells,cryptoCells,assoc},
  
  assoc=unwrapEXR[exr];
  passes=Join[
    Lookup[assoc,"Passes",<||>],
    Lookup[assoc,"Computed",<||>]
  ];
  names=Keys[passes];
  
  cells=Labeled[
      previewImage[#,passes[#],imageSize],
      Style[#,12,Bold],
      Top
    ] & /@ names;
  
  cryptoCells=cryptomattePreviewCells[assoc,imageSize];
  
  Grid[
    Partition[Join[cells,cryptoCells],UpTo[3]],
    Spacings->{2,2},
    Alignment->Top
  ]
]

End[]
EndPackage[]
