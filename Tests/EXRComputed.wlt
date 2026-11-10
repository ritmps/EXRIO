VerificationTest[
  Needs["EXRIO`"],
  Null,
  TestID -> "exrcomputed-load"
]

VerificationTest[
  sample = "/home/flip/renders/procedural_dataset/scene_031_view_000.exr";
  exr = EXRIO`ReadEXR[sample, "ComputeDerivedPasses" -> True];
  MemberQ[exr["Passes"], "ShapeIndex"],
  True,
  TestID -> "exrcomputed-shape-index-exists"
]

VerificationTest[
  MatchQ[exr["ShapeIndex"], _Image],
  True,
  TestID -> "exrcomputed-shape-index-image"
]

VerificationTest[
  MemberQ[exr["Passes"], "LogCurvedness"],
  True,
  TestID -> "exrcomputed-logcurvedness-exists"
]

VerificationTest[
  MatchQ[exr["Curvature"], _Association],
  True,
  TestID -> "exrcomputed-curvature-association"
]

VerificationTest[
  SubsetQ[Keys[exr["Curvature"]], {"k1", "k2", "H", "K", "ShapeIndex", "Curvedness", "LogCurvedness"}],
  True,
  TestID -> "exrcomputed-curvature-keys"
]
