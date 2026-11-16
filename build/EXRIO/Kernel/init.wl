(* ::Package:: *)

Module[{kernelDir = DirectoryName[$InputFileName]},
  Get[FileNameJoin[{kernelDir, "EXRCryptomatte.wl"}]];
  Get[FileNameJoin[{kernelDir, "EXRComputed.wl"}]];
  Get[FileNameJoin[{kernelDir, "EXRBoxes.wl"}]];
  Get[FileNameJoin[{kernelDir, "EXRCamera.wl"}]];
  Get[FileNameJoin[{kernelDir, "EXRPreview.wl"}]];
  Get[FileNameJoin[{kernelDir, "ImageMetrics.wl"}]];
  Get[FileNameJoin[{kernelDir, "EXRIO.wl"}]];
];

BeginPackage["EXRIO`", {
  "EXRCryptomatte`",
  "EXRComputed`",
  "EXRBoxes`",
  "EXRCamera`",
  "EXRPreview`",
  "ImageMetrics`"
}];

EndPackage[];
