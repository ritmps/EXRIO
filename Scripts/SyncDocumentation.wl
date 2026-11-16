(* ::Package:: *)

(* SyncDocumentation.wl
   Maintain EXRIO Wolfram Documentation notebooks in Reference.nb format:
     - Reformat existing Symbol ReferencePages (Usage, SeeAlso, Options tables, MoreAbout)
     - Clean up stub descriptions and missing constants in Guides/EXRIO.nb
     - Scaffold clean Reference.nb templates for newly exported symbols
     - Flag or prune stale Symbol pages and Guide links
*)

BeginPackage["EXRIOSyncDocs`"];

SyncEXRIODocumentation::usage =
  "SyncEXRIODocumentation[opts] synchronizes EXRIO documentation notebooks with exported symbols.";

Begin["`Private`"];

$PacletRoot = DirectoryName[DirectoryName[$InputFileName]];
$DocsRoot = FileNameJoin[{$PacletRoot, "Documentation", "English"}];
$SymbolsDir = FileNameJoin[{$DocsRoot, "ReferencePages", "Symbols"}];
$GuidePath = FileNameJoin[{$DocsRoot, "Guides", "EXRIO.nb"}];

$EXRIOContexts = {
  "EXRIO`",
  "EXRComputed`",
  "EXRCryptomatte`",
  "EXRCamera`",
  "EXRBoxes`",
  "EXRPreview`",
  "ImageMetrics`"
};

$FallbackUsage = <|
  "EXRData" -> "EXRData[assoc] represents a structured multi-pass OpenEXR container with custom summary box formatting.",
  "$EXRIOVersion" -> "$EXRIOVersion gives the version string of the loaded EXRIO paclet.",
  "$KoenderinkShapeClasses" -> "$KoenderinkShapeClasses lists the 9 canonical Koenderink & van Doorn qualitative shape index categories.",
  "$EcologicalCueNames" -> "$EcologicalCueNames lists the derived ecological cue pass names produced by ComputeEcologicalCues."
|>;

$GuideFallbacks = <|
  "WriteEXR" -> "exports an EXRData container or pass association to a multi-channel OpenEXR file",
  "AddComputedPass" -> "stores an ad-hoc computed pass under \"Computed\"[name] in an EXRData container",
  "PassToImage" -> "converts an EXR pass association or array to a Wolfram Image",
  "ComputeScaleSpaceCurvature" -> "computes Koenderink scale-space differential invariants across spatial apertures",
  "KoenderinkShapeCategory" -> "classifies continuous Shape Index values into the 9 canonical qualitative shape categories",
  "ColorizeShapeIndex" -> "renders a continuous or 9-class discrete Koenderink Shape Index color map Image",
  "ComputeEcologicalCues" -> "derives Gibsonian ecological cue passes (contours, AO, texture gradients, rim sign) from geometry",
  "$EXRIOVersion" -> "version string of the loaded EXRIO paclet",
  "$KoenderinkShapeClasses" -> "list of the 9 canonical Koenderink qualitative shape categories",
  "$EcologicalCueNames" -> "list of derived ecological cue pass names"
|>;

loadPaclet[] := Module[{},
  PacletDirectoryLoad[$PacletRoot];
  Needs["EXRIO`"];
];

exportedSymbolNames[] := Sort @ DeleteDuplicates @ Flatten[
  Map[Function[ctx, Select[Names[ctx <> "*"], !StringContainsQ[#, "`"] &]], $EXRIOContexts]
];

symbolUsageString[name_String] := Module[{usg},
  usg = Quiet @ Check[
    ToExpression[name <> "::usage"],
    Missing["NotAvailable"]
  ];
  If[StringQ[usg] && StringLength[StringTrim[usg]] > 0,
    StringTrim[usg],
    Lookup[$FallbackUsage, name, name <> " is an exported symbol in EXRIO`."]
  ]
];

symbolOptionsList[name_String] := Module[{sym, opts},
  sym = Quiet @ Symbol[name];
  opts = Quiet @ Check[Options[Evaluate[sym]], {}];
  If[ListQ[opts], opts, {}]
];

(* Convert a single pattern string like "ReadEXR[file, opts]" or "F[a][b]" into clean InlineFormula boxes *)
formatArgList[inner_String] := Module[{args, argBoxes},
  args = StringTrim /@ StringSplit[inner, ","];
  argBoxes = Riffle[
    Map[Function[a,
      If[StringContainsQ[a, "->"],
        With[{parts = StringTrim /@ StringSplit[a, "->", 2]},
          RowBox[{StyleBox[parts[[1]], "TI"], " ", "\[Rule]", " ", StyleBox[parts[[2]], "TI"]}]
        ],
        StyleBox[a, "TI"]
      ]
    ], args],
    ","
  ];
  If[Length[argBoxes] === 1, First[argBoxes], RowBox[argBoxes]]
];

patternToBoxes[pat_String, symName_String] := Module[{groups, expr},
  If[StringStartsQ[pat, symName <> "["] && StringEndsQ[pat, "]"],
    groups = StringCases[StringDrop[pat, StringLength[symName]], "[" ~~ Shortest[inner___] ~~ "]" :> inner];
    If[Length[groups] >= 1,
      expr = symName;
      Do[
        expr = RowBox[{expr, "[", formatArgList[g], "]"}]
      , {g, groups}];
      Return[expr]
    ]
  ];
  If[StringMatchQ[pat, "$" ~~ __] || pat === symName,
    pat,
    RowBox[{pat}]
  ]
];

(* Parse a usage string into proper Reference.nb Usage Cell *)
buildUsageCell[symName_String, usageStr_String] := Module[
  {lines, parsedLines, flatItems},
  lines = Select[StringTrim /@ StringSplit[usageStr, "\n"], StringLength[#] > 0 &];
  parsedLines = Map[
    Function[line,
      Module[{m, pat, desc},
        (* Match leading Call[...][...] or Call[...] or $Constant *)
        m = StringCases[line,
          StartOfString ~~ p : (symName ~~ ("[" ~~ Shortest[___] ~~ "]") ..) ~~ Whitespace ~~ d : __ :> {p, d}
        ];
        If[m === {},
          m = StringCases[line,
            StartOfString ~~ p : (symName) ~~ Whitespace ~~ d : __ :> {p, d}
          ];
        ];
        If[Length[m] >= 1,
          {pat, desc} = First[m];
          {
            Cell["   ", "ModInfo"],
            Cell[BoxData[patternToBoxes[pat, symName]], "InlineFormula"],
            " \[LineSeparator]" <> StringTrim[desc]
          },
          {
            Cell["   ", "ModInfo"],
            Cell[BoxData[symName], "InlineFormula"],
            " \[LineSeparator]" <> line
          }
        ]
      ]
    ],
    lines
  ];
  flatItems = Flatten[Riffle[parsedLines, {"\n"}], 1];
  Cell[TextData[flatItems], "Usage"]
];

(* Build a proper RefLink TemplateBox *)
makeRefLink[symName_String] := Cell[BoxData[
  TemplateBox[
    {Cell[TextData[symName]], "paclet:EXRIO/ref/" <> symName},
    "RefLink",
    BaseStyle -> {"InlineFormula"}
  ]
], "InlineSeeAlsoFunction"];

buildSeeAlsoSection[related_List, validSyms_List] := Module[{clean, items},
  clean = Select[DeleteDuplicates[related], MemberQ[validSyms, #] &];
  If[clean === {}, Return[Nothing]];
  items = Riffle[
    makeRefLink /@ clean,
    StyleBox[" \[FilledVerySmallSquare] ", "InlineSeparator"]
  ];
  Cell[CellGroupData[{
    Cell["See Also", "SeeAlsoSection"],
    Cell[TextData[items], "SeeAlso"]
  }, Open]]
];

buildMoreAboutSection[] := Cell[CellGroupData[{
  Cell["Related Guides", "MoreAboutSection"],
  Cell[TextData[
    ButtonBox["EXRIO", BaseStyle -> "Link", ButtonData -> "paclet:EXRIO/guide/EXRIO"]
  ], "MoreAbout"]
}, Open]];

(* Extract symbol names from an old SeeAlsoSection (whether StyleBox or RefLink) *)
extractSeeAlsoSymbols[nb_Notebook] := Module[{cells},
  cells = Cases[nb, Cell[__, "SeeAlsoText" | "SeeAlso", ___], Infinity];
  DeleteDuplicates @ Flatten @ Map[
    Function[c,
      Join[
        Cases[c, StyleBox[s_String, "InlineFormula", ___] :> s, Infinity],
        Cases[c, TemplateBox[{Cell[TextData[s_String], ___], ___}, "RefLink", ___] :> s, Infinity]
      ]
    ],
    cells
  ]
];

(* Upgrade old "TableText" option GridBox cells to Reference.nb "2ColumnTableMod" *)
upgradeOptionsTables[expr_] := expr /. {
  Cell[BoxData[GridBox[rows_List, opts___]], "TableText", rest___] :>
    Module[{dataRows, modRows},
      (* Drop header row if it's {"Option", "Default Value"} *)
      dataRows = If[Length[rows] >= 1 && !FreeQ[rows[[1]], "Option"], Rest[rows], rows];
      modRows = Map[
        Function[r,
          Module[{optName, defVal},
            optName = First[Cases[r[[1]], s_String :> s, {0, Infinity}], ToString[r[[1]]]];
            defVal = First[Cases[r[[2]], s_String :> s, {0, Infinity}], ToString[r[[2]]]];
            {Cell["      ", "ModInfo"], optName, Cell[defVal, "TableText"]}
          ]
        ],
        dataRows
      ];
      Cell[BoxData[GridBox[modRows]], "2ColumnTableMod", rest]
    ]
};

(* Reformat an existing Symbol ReferencePage notebook while preserving Notes & Examples *)
reformatSymbolNotebook[path_String, validSyms_List] := Module[
  {symName, nb, topCells, catGroup, mainGroup, mainCells,
   objNameCell, notesGroup, examplesGroup, relatedSyms, newMainCells, newTopCells, newNb},
  symName = FileBaseName[path];
  nb = Get[path];
  If[!MatchQ[nb, _Notebook],
    Print["WARNING: Could not parse ", path];
    Return[False]
  ];
  topCells = First[nb];
  catGroup = FirstCase[topCells, c : Cell[CellGroupData[{Cell["Categorization", "CategorizationSection", ___], ___}, ___]] :> c, Nothing];
  mainGroup = FirstCase[topCells, c : Cell[CellGroupData[{Cell[_, "ObjectName", ___], ___}, ___]] :> c, $Failed];
  If[mainGroup === $Failed,
    Print["WARNING: No ObjectName group in ", path];
    Return[False]
  ];
  mainCells = First[First[mainGroup]];
  objNameCell = FirstCase[mainCells, c : Cell[_, "ObjectName", ___] :> c, Cell[symName, "ObjectName"]];
  notesGroup = FirstCase[mainCells, c : Cell[CellGroupData[{Cell[_, "NotesSection", ___], ___}, ___]] :> upgradeOptionsTables[c], Nothing];
  examplesGroup = FirstCase[mainCells, c : Cell[CellGroupData[{Cell[_, "PrimaryExamplesSection", ___], ___}, ___]] :> c, Nothing];

  relatedSyms = DeleteCases[extractSeeAlsoSymbols[nb], symName];
  newMainCells = DeleteCases[{
    objNameCell,
    buildUsageCell[symName, symbolUsageString[symName]],
    notesGroup,
    examplesGroup,
    buildSeeAlsoSection[relatedSyms, validSyms],
    buildMoreAboutSection[]
  }, Nothing];

  newTopCells = DeleteCases[{
    catGroup,
    Cell[CellGroupData[newMainCells, Open]]
  }, Nothing];

  newNb = Notebook[
    newTopCells,
    Sequence @@ Rest[nb]
  ];
  Export[path, newNb, "NB"];
  True
];

(* Scaffold a brand-new Reference.nb template for an undocumented symbol *)
scaffoldSymbolNotebook[symName_String, validSyms_List] := Module[
  {path, usg, opts, optRows, notesGroup, catGroup, mainCells, nb},
  path = FileNameJoin[{$SymbolsDir, symName <> ".nb"}];
  If[FileExistsQ[path], Return[False]];
  usg = symbolUsageString[symName];
  opts = symbolOptionsList[symName];

  catGroup = Cell[CellGroupData[{
    Cell["Categorization", "CategorizationSection"],
    Cell["Symbol", "Categorization", CellLabel -> "Entity Type"],
    Cell["EXRIO", "Categorization", CellLabel -> "Paclet Name"],
    Cell["EXRIO`", "Categorization", CellLabel -> "Context"],
    Cell["EXRIO/ref/" <> symName, "Categorization", CellLabel -> "URI"]
  }, Closed]];

  notesGroup = If[Length[opts] > 0,
    optRows = Map[
      {Cell["      ", "ModInfo"], SymbolName[First[#]], Cell[ToString[Last[#], InputForm], "TableText"]} &,
      opts
    ];
    Cell[CellGroupData[{
      Cell["Details and Options", "NotesSection"],
      Cell["The following options can be given:", "Notes"],
      Cell[BoxData[GridBox[optRows]], "2ColumnTableMod"]
    }, Closed]],
    Nothing
  ];

  mainCells = DeleteCases[{
    Cell[symName, "ObjectName"],
    buildUsageCell[symName, usg],
    notesGroup,
    Cell[CellGroupData[{
      Cell["Examples", "PrimaryExamplesSection"],
      Cell[CellGroupData[{
        Cell["Basic Examples", "ExampleSection"],
        Cell[BoxData[RowBox[{"Needs", "[", "\"EXRIO`\"", "]"}]], "Input"]
      }, Open]]
    }, Open]],
    buildSeeAlsoSection[Take[DeleteCases[validSyms, symName], UpTo[4]], validSyms],
    buildMoreAboutSection[]
  }, Nothing];

  nb = Notebook[
    {catGroup, Cell[CellGroupData[mainCells, Open]]},
    TaggingRules -> <|
      "Metadata" -> <|
        "context" -> "EXRIO`",
        "type" -> "Symbol",
        "paclet" -> "EXRIO",
        "language" -> "en",
        "title" -> symName,
        "windowtitle" -> symName,
        "uri" -> "EXRIO/ref/" <> symName,
        "summary" -> usg,
        "index" -> True
      |>
    |>,
    StyleDefinitions -> "Reference.nb"
  ];
  Export[path, nb, "NB"];
  True
];

(* Clean up stub descriptions in Guides/EXRIO.nb and optionally append missing symbols *)
cleanGuideNotebook[validSyms_List, syncMissing_: False] := Module[
  {nb, updated, existingSyms, missingSyms, constSyms, otherSyms, newSubsections},
  nb = Get[$GuidePath];
  If[!MatchQ[nb, _Notebook], Return[False]];

  (* Replace "<Sym> function." stubs with real descriptions *)
  updated = nb /. (Cell[TextData[{
    c : Cell[BoxData[TemplateBox[{Cell[TextData[symName_String], ___], ___}, "RefLink", ___]], ___],
    " \[LongDash] ",
    desc_String
  }], "GuideText", rest___] /; desc === (symName <> " function.")) :> Cell[TextData[{
    c,
    " \[LongDash] ",
    Lookup[$GuideFallbacks, symName, symbolUsageString[symName]]
  }], "GuideText", rest];

  If[syncMissing,
    existingSyms = Cases[updated, TemplateBox[{Cell[TextData[s_String], ___], ___}, "RefLink", ___] :> s, Infinity];
    missingSyms = Select[validSyms, !MemberQ[existingSyms, #] &];
    If[Length[missingSyms] > 0,
      constSyms = Select[missingSyms, StringStartsQ[#, "$"] &];
      otherSyms = Select[missingSyms, !StringStartsQ[#, "$"] &];
      newSubsections = {};
      If[Length[otherSyms] > 0,
        AppendTo[newSubsections, Cell[CellGroupData[
          Prepend[
            Map[guideTextCell, otherSyms],
            Cell["Image Quality & Distribution Metrics", "GuideFunctionsSubsection"]
          ], Open]]]
      ];
      If[Length[constSyms] > 0,
        AppendTo[newSubsections, Cell[CellGroupData[
          Prepend[
            Map[guideTextCell, constSyms],
            Cell["Constants & Metadata", "GuideFunctionsSubsection"]
          ], Open]]]
      ];
      updated = updated /. Cell[CellGroupData[{Cell["Functions", "GuideFunctionsSection", r1___], subs___}, Open]] :>
        Cell[CellGroupData[{Cell["Functions", "GuideFunctionsSection", r1], subs, Sequence @@ newSubsections}, Open]];
    ];
  ];

  Export[$GuidePath, updated, "NB"];
  True
];

guideTextCell[symName_String] := Cell[TextData[{
  Cell[BoxData[
    TemplateBox[
      {Cell[TextData[symName]], "paclet:EXRIO/ref/" <> symName},
      "RefLink",
      BaseStyle -> {"InlineFormula"}
    ]
  ]],
  " \[LongDash] ",
  Lookup[$GuideFallbacks, symName, symbolUsageString[symName]]
}], "GuideText"];

Options[SyncEXRIODocumentation] = {
  "FormatExisting" -> True,
  "CleanGuide" -> True,
  "ScaffoldMissing" -> False,
  "SyncGuide" -> False,
  "PruneStale" -> False
};

SyncEXRIODocumentation[OptionsPattern[]] := Module[
  {validSyms, nbFiles, docSyms, staleSyms, missingSyms, formatted = 0, scaffolded = 0, pruned = 0},
  loadPaclet[];
  validSyms = exportedSymbolNames[];
  nbFiles = Sort @ FileNames["*.nb", $SymbolsDir];
  docSyms = FileBaseName /@ nbFiles;

  staleSyms = Select[docSyms, !MemberQ[validSyms, #] &];
  missingSyms = Select[validSyms, !MemberQ[docSyms, #] &];

  If[TrueQ[OptionValue["PruneStale"]] && Length[staleSyms] > 0,
    Do[
      DeleteFile[FileNameJoin[{$SymbolsDir, s <> ".nb"}]];
      pruned++;
    , {s, staleSyms}];
    nbFiles = Sort @ FileNames["*.nb", $SymbolsDir];
  ];

  If[TrueQ[OptionValue["FormatExisting"]],
    Do[
      If[reformatSymbolNotebook[f, validSyms], formatted++];
    , {f, nbFiles}];
  ];

  If[TrueQ[OptionValue["ScaffoldMissing"]],
    Do[
      If[scaffoldSymbolNotebook[s, validSyms], scaffolded++];
    , {s, missingSyms}];
  ];

  If[TrueQ[OptionValue["CleanGuide"]] || TrueQ[OptionValue["SyncGuide"]],
    cleanGuideNotebook[validSyms, TrueQ[OptionValue["SyncGuide"]]];
  ];

  <|
    "ExportedSymbols" -> Length[validSyms],
    "FormattedExisting" -> formatted,
    "ScaffoldedNew" -> scaffolded,
    "PrunedStale" -> pruned,
    "StaleFound" -> staleSyms,
    "MissingFound" -> missingSyms
  |>
];

End[];
EndPackage[];

If[$ScriptCommandLine =!= {} && MemberQ[$ScriptCommandLine, "--run"],
  With[{
    doFormat = MemberQ[$ScriptCommandLine, "--format-existing"],
    doScaffold = MemberQ[$ScriptCommandLine, "--scaffold"],
    doSyncGuide = MemberQ[$ScriptCommandLine, "--sync-guide"],
    doPrune = MemberQ[$ScriptCommandLine, "--prune"]
  },
    Print[EXRIOSyncDocs`SyncEXRIODocumentation[
      "FormatExisting" -> doFormat,
      "CleanGuide" -> True,
      "ScaffoldMissing" -> doScaffold,
      "SyncGuide" -> doSyncGuide,
      "PruneStale" -> doPrune
    ]];
  ]
];
