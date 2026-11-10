(* ::Package:: *)

Needs["MUnit`"];

testDir = DirectoryName[$InputFileName];
pacletDir = DirectoryName[testDir];

PacletDirectoryLoad[pacletDir];
Needs["EXRIO`"];

Print["Running EXRIO Test Suite..."];
r1 = TestRun[FileNameJoin[{testDir, "EXRIO.wlt"}]];
r2 = TestRun[FileNameJoin[{testDir, "EXRComputed.wlt"}]];

Print["EXRIO Test Results: ", r1];
Print["EXRComputed Test Results: ", r2];

If[MemberQ[{TestFailure, TestError}, Head[r1]] || MemberQ[{TestFailure, TestError}, Head[r2]],
  Exit[1],
  Exit[0]
];
