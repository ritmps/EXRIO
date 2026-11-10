VerificationTest[
  Needs["EXRIO`"],
  Null,
  TestID -> "exrio-load-package"
]

VerificationTest[
  StringQ[EXRIO`$EXRIOVersion] && EXRIO`$EXRIOVersion === "2.0.5",
  True,
  TestID -> "exrio-version-check"
]

VerificationTest[
  sample = "/home/flip/renders/procedural_dataset/scene_031_view_000.exr";
  FileExistsQ[sample],
  True,
  TestID -> "exrio-sample-exists"
]

VerificationTest[
  exr = EXRIO`ReadEXR[sample, "ComputeDerivedPasses" -> False];
  Head[exr] === EXRBoxes`EXRData,
  True,
  TestID -> "exrio-read-exr-head"
]

VerificationTest[
  Sort[Keys[Normal[exr]]],
  Sort[{"File", "Passes", "Computed", "Metadata", "Cryptomatte"}],
  TestID -> "exrio-top-keys"
]

VerificationTest[
  MemberQ[exr["Passes"], "beauty"],
  True,
  TestID -> "exrio-passes-list-contains-beauty"
]

VerificationTest[
  MatchQ[exr["beauty"], _Image],
  True,
  TestID -> "exrio-direct-beauty-image"
]

VerificationTest[
  MatchQ[exr["Image", "beauty"], _Image],
  True,
  TestID -> "exrio-image-beauty"
]

VerificationTest[
  MatchQ[exr["Passes", "beauty", "Image"], _Image],
  True,
  TestID -> "exrio-nested-beauty-image"
]

VerificationTest[
  MatchQ[exr["Data", "beauty"], _NumericArray],
  True,
  TestID -> "exrio-data-beauty-numeric-array"
]

VerificationTest[
  MatchQ[exr["Metadata", "Camera", "FOV"], _?NumericQ],
  True,
  TestID -> "exrio-camera-fov-numeric"
]
