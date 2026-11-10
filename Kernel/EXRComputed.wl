(* ::Package:: *)

BeginPackage["EXRComputed`"]

ComputeCurvature::usage = "ComputeCurvature[pImg, nImg] computes differential geometry passes from position and normal images. ComputeCurvature[exr] computes curvature passes for an EXR association.";

SampleEnvironment::usage = "SampleEnvironment[nImg, envMap] samples an environment map using the normal vector.";

ComputeEcologicalCues::usage = "ComputeEcologicalCues[exr] derives Decomp v3 Gibsonian cue passes from camera-space geometry.P and geometry.Nn (no re-render needed): LogDepth (ordinal-ranking proxy, never supervised metrically), Contour (occluding edge owned by the near/figure side), TextureGradient (grad ln of optical texture density, compressed to the unit disk), TexValid (mask excluding depth discontinuities), Occlusion (screen-space horizon AO accessibility, 1 = open) and RimSign (Koenderink contour-convexity sign on owned contours: +1 convex/elliptic, -1 concave/hyperbolic, 0 inflection or off-contour).";

$EcologicalCueNames::usage = "Names of the passes produced by ComputeEcologicalCues.";
$EcologicalCueNames = {"LogDepth", "Contour", "TextureGradient", "TexValid", "Occlusion", "RimSign"};

ComputeScaleSpaceCurvature::usage = "ComputeScaleSpaceCurvature[exr, sigma, opts] computes Koenderink scale-space / local-jet differential invariants (ShapeIndex, LogCurvedness, ScaleNormalizedCurvedness, k1, k2, H, K, KoenderinkCategory) at spatial aperture sigma (in pixels, or list of scales {s1, s2, ...}), using either \"Method\" -> \"WeingartenJet\" (3D manifold 1-jet on P and Nn) or \"Method\" -> \"MongeJet\" (2D Gaussian 2-jet J^2_sigma on depth Z).";
KoenderinkShapeCategory::usage = "KoenderinkShapeCategory[shapeIndex, mask] classifies continuous Shape Index S in [-1, 1] into the 9 canonical Koenderink & van Doorn (1992) qualitative shape categories (1 = Spherical Cup ... 5 = Symmetric Saddle ... 9 = Spherical Cap; 0 = background).";
ColorizeShapeIndex::usage = "ColorizeShapeIndex[shapeIndex, mask, opts] renders a continuous or 9-class discrete Koenderink Shape Index color map Image.";
$KoenderinkShapeClasses::usage = "Canonical 9 qualitative shape classes of Koenderink & van Doorn (1992) over S in [-1, 1].";

Begin["`Private`"]

isEXRDataQ[expr_] := MatchQ[expr, _[ _Association ]] && MatchQ[Head[expr], _Symbol] && (SymbolName[Head[expr]] === "EXRData")
unwrapEXR[expr_] := If[isEXRDataQ[expr], First[expr], expr]
wrapEXR[orig_, newAssoc_] := If[isEXRDataQ[orig], Head[orig][newAssoc], newAssoc]

safeNormalize[v_] :=
  Module[{u = N @ Flatten @ {v}},
    If[Length[u] == 3 && VectorQ[u, NumericQ],
      With[{n = Norm[u]},
        If[n > 0.,
          u / n
          ,
          {0., 0., 1.}
        ]
      ]
      ,
      {0., 0., 1.}
    ]
  ]

safeDivide[num_, den_] :=
  MapThread[
    If[Abs[#2] > 10^-8,
      #1 / #2
      ,
      0.0
    ]&
    ,
    {num, den}
    ,
    2
  ]

objectMaskFromLabels[passes_Association] :=
  Module[{labels},
    If[!KeyExistsQ[passes, "ObjectLabel"],
      Return[$Failed]
    ];
    labels = Normal[passes["ObjectLabel", "Data"]];
    Unitize[labels]
  ]

computeCurvatureCore[p_, n_, dpdu_, dpdv_, mask_] :=
  Module[{px, nx, pu, pv, nu, nv, e, f, g, l, m, nCoeff, detI, w11, w12,
     w21, w22, trW, detW, disc, sqrtDisc, k1, k2, h, k, shapeIndex, curvedness,
     contrast, ux, vx, uy, vy, detJ, pux, pvx, puy, pvy, pxDeriv, pyDeriv,
     nxDeriv, nyDeriv, a11, b11, a12, b12, normSq11, normSq12, useFirst1,
     a1, b1, v1Raw, e1, e2x, e2y, e2z, e2, mask3D, k1Vec, k2Vec},
    px = GaussianFilter[p, 1.0];
    nx = Map[safeNormalize, GaussianFilter[n, 1.0], {2}];
    pxDeriv = DerivativeFilter[px, {1, 0}];
    pyDeriv = DerivativeFilter[px, {0, 1}];
    nxDeriv = DerivativeFilter[nx, {1, 0}];
    nyDeriv = DerivativeFilter[nx, {0, 1}];
    If[dpdu === None || dpdv === None,
      pu = pxDeriv;
      pv = pyDeriv;
      nu = nxDeriv;
      nv = nyDeriv
      ,
      pu = GaussianFilter[dpdu, 1.0];
      pv = GaussianFilter[dpdv, 1.0];
      e = Total[pu * pu, {3}];
      f = Total[pu * pv, {3}];
      g = Total[pv * pv, {3}];
      detI = e * g - f^2;
      pux = Total[pu * pxDeriv, {3}];
      pvx = Total[pv * pxDeriv, {3}];
      puy = Total[pu * pyDeriv, {3}];
      pvy = Total[pv * pyDeriv, {3}];
      ux = safeDivide[g * pux - f * pvx, detI];
      vx = safeDivide[-f * pux + e * pvx, detI];
      uy = safeDivide[g * puy - f * pvy, detI];
      vy = safeDivide[-f * puy + e * pvy, detI];
      detJ = ux * vy - uy * vx;
      nu =
        MapThread[
          If[Abs[#3] > 10^-8,
            (#1 * #2 - #4 * #5) / #3
            ,
            {0., 0., 0.}
          ]&
          ,
          {vy, nxDeriv, detJ, vx, nyDeriv}
          ,
          2
        ];
      nv =
        MapThread[
          If[Abs[#3] > 10^-8,
            (-#1 * #2 + #4 * #5) / #3
            ,
            {0., 0., 0.}
          ]&
          ,
          {uy, nxDeriv, detJ, ux, nyDeriv}
          ,
          2
        ];
    ];
    e = Total[pu * pu, {3}];
    f = Total[pu * pv, {3}];
    g = Total[pv * pv, {3}];
    l = -Total[pu * nu, {3}];
    m = -0.5 * (Total[pu * nv, {3}] + Total[pv * nu, {3}]);
    nCoeff = -Total[pv * nv, {3}];
    detI = e * g - f^2;
    w11 = safeDivide[g * l - f * m, detI];
    w12 = safeDivide[g * m - f * nCoeff, detI];
    w21 = safeDivide[-f * l + e * m, detI];
    w22 = safeDivide[-f * m + e * nCoeff, detI];
    trW = w11 + w22;
    detW = w11 * w22 - w12 * w21;
    disc = trW^2 - 4 detW;
    sqrtDisc = Sqrt[Map[Max[#, 0.]&, disc, {2}]];
    k1 = 0.5 * (trW + sqrtDisc) * mask;
    k2 = 0.5 * (trW - sqrtDisc) * mask;
    h = 0.5 * trW * mask;
    k = detW * mask;
    shapeIndex =
      MapThread[
          If[#1 > 10^-8,
            -(2.0 / Pi) ArcTan[#2 / #1]
            ,
            0.0
          ]&
          ,
          {k1 - k2, k1 + k2}
          ,
          2
        ] * mask;
    curvedness = Sqrt[0.5 * (k1^2 + k2^2)] * mask;
    contrast = (k1 - k2) * mask;

    a11 = w12;
    b11 = k1 - w11;
    a12 = k1 - w22;
    b12 = w21;
    normSq11 = a11^2 + b11^2;
    normSq12 = a12^2 + b12^2;
    useFirst1 = MapThread[If[#1 >= #2, 1.0, 0.0]&, {normSq11, normSq12}, 2];
    a1 = useFirst1 * a11 + (1.0 - useFirst1) * a12;
    b1 = useFirst1 * b11 + (1.0 - useFirst1) * b12;
    v1Raw = a1 * pu + b1 * pv;

    e1 =
      MapThread[
        Module[{v = #1, nVec = #2, puVec = #3, pvVec = #4, vNorm, pProj, pNorm},
          vNorm = Norm[v];
          If[vNorm > 10^-8,
            v / vNorm
            ,
            pProj = puVec - (puVec . nVec) * nVec;
            pNorm = Norm[pProj];
            If[pNorm > 10^-8,
              pProj / pNorm
              ,
              pProj = pvVec - (pvVec . nVec) * nVec;
              pNorm = Norm[pProj];
              If[pNorm > 10^-8,
                pProj / pNorm
                ,
                pProj = {1.0, 0.0, 0.0} - nVec[[1]] * nVec;
                pNorm = Norm[pProj];
                If[pNorm > 10^-8, pProj / pNorm, {1.0, 0.0, 0.0}]
              ]
            ]
          ]
        ]&
        ,
        {v1Raw, nx, pu, pv}
        ,
        2
      ];

    e2x = nx[[All, All, 2]] * e1[[All, All, 3]] - nx[[All, All, 3]] * e1[[All, All, 2]];
    e2y = nx[[All, All, 3]] * e1[[All, All, 1]] - nx[[All, All, 1]] * e1[[All, All, 3]];
    e2z = nx[[All, All, 1]] * e1[[All, All, 2]] - nx[[All, All, 2]] * e1[[All, All, 1]];
    e2 = MapThread[List, {e2x, e2y, e2z}, 2];

    mask3D = MapThread[ConstantArray[#1, 3]&, {mask}, 2];
    k1Vec = e1 * mask3D;
    k2Vec = e2 * mask3D;

    logCurvedness = Log[1.0 + curvedness] * mask;
    parabolic = UnitStep[0.05 - Abs[k]] * mask;
    slant = Clip[nx[[All, All, 3]], {-1.0, 1.0}] * mask;

    tiltNorm = Sqrt[nx[[All, All, 1]]^2 + nx[[All, All, 2]]^2 + 10^-8];
    tiltX = (nx[[All, All, 1]] / tiltNorm) * mask;
    tiltY = (nx[[All, All, 2]] / tiltNorm) * mask;
    tilt = MapThread[List, {tiltX, tiltY}, 2];

    <|"k1" -> k1, "k2" -> k2, "H" -> h, "K" -> k, "ShapeIndex" -> shapeIndex,
       "Curvedness" -> curvedness, "LogCurvedness" -> logCurvedness,
       "Parabolic" -> parabolic, "Slant" -> slant, "Tilt" -> tilt,
       "ContrastOfCurvature" -> contrast,
       "k1Vector" -> k1Vec, "k2Vector" -> k2Vec|>
  ]

ComputeCurvature[pImg_Image, nImg_Image] :=
  ComputeCurvature[ImageData[pImg], ImageData[nImg]]

ComputeCurvature[p_List, n_List, mask_:None] :=
  Module[{m},
    m =
      If[mask === None,
        ConstantArray[1.0, Dimensions[p[[All, All, 1]]]]
        ,
        mask
      ];
    computeCurvatureCore[p, n, None, None, m]
  ]

ComputeCurvature[p_List, n_List, dpdu_List, dpdv_List, mask_:None] :=
  Module[{m},
    m =
      If[mask === None,
        ConstantArray[1.0, Dimensions[p[[All, All, 1]]]]
        ,
        mask
      ];
    computeCurvatureCore[p, n, dpdu, dpdv, m]
  ]

ComputeCurvature[inputs_List, opts : OptionsPattern[]] :=
  Map[ComputeCurvature[#, opts] &, inputs];

ComputeCurvature[f_File, opts : OptionsPattern[]] :=
  ComputeCurvature[First[f], opts];

ComputeCurvature[file_String, opts : OptionsPattern[]] :=
  Module[{exr = Quiet @ Check[EXRIO`ReadEXR[file, opts], $Failed]},
    If[exr =!= $Failed,
      ComputeCurvature[exr, opts],
      $Failed
    ]
  ];

ComputeCurvature[img_Image, opts : OptionsPattern[]] :=
  Module[{exr = Quiet @ Check[EXRIO`ReadEXR[img, opts], $Failed]},
    If[exr =!= $Failed,
      ComputeCurvature[exr, opts],
      $Failed
    ]
  ];

ComputeCurvature[exr : _Association | _?isEXRDataQ, opts : OptionsPattern[]] :=
  Module[{passes, pImg, nImg, dpduImg, dpdvImg, mask, curv, newPasses, assoc
    },
    assoc = unwrapEXR[exr];
    passes = assoc["Passes"];
    If[!SubsetQ[Keys[passes], {"P", "Nn"}],
      Return[exr]
    ];
    pImg = Normal[passes["P"]["Data"]];
    nImg = Normal[passes["Nn"]["Data"]];
    dpduImg =
      If[KeyExistsQ[passes, "dPdu"],
        Normal[passes["dPdu"]["Data"]]
        ,
        None
      ];
    dpdvImg =
      If[KeyExistsQ[passes, "dPdv"],
        Normal[passes["dPdv"]["Data"]]
        ,
        None
      ];
    mask = objectMaskFromLabels[passes];
    If[mask === $Failed,
      mask =
        If[KeyExistsQ[passes, "A"],
          Normal[passes["A"]["Data"]]
          ,
          ConstantArray[1.0, Dimensions[pImg[[All, All, 1]]]]
        ]
    ];
    curv = computeCurvatureCore[pImg, nImg, dpduImg, dpdvImg, mask];
    newPasses = AssociationMap[
      <|
        "Type" -> Which[
          ArrayDepth[curv[#]] === 3 && Dimensions[curv[#]][[3]] == 3, "Vector3",
          ArrayDepth[curv[#]] === 3 && Dimensions[curv[#]][[3]] == 2, "Vector2",
          True, "Scalar"
        ],
        "Data" -> NumericArray[curv[#], "Real32"]
      |>&
      ,
      Keys[curv]
    ];
    wrapEXR[exr, Join[assoc, <|"Computed" -> Join[Lookup[assoc, "Computed", <||>], newPasses]|>]]
  ]

(* ::Section:: *)
(* Decomp v3 ecological cues (derived from camera-space P / Nn; no re-render) *)

(* Shift a HxW or HxWxC array by {dy, dx} pixels with edge replication:
   result[[i, j]] = a[[i + dy, j + dx]] (clamped). *)
ecoShift[a_, {dy_Integer, dx_Integer}] := Module[{h, w, rows, cols},
  {h, w} = Take[Dimensions[a], 2];
  rows = Clip[Range[h] + dy, {1, h}];
  cols = Clip[Range[w] + dx, {1, w}];
  a[[rows, cols]]
];

ecoDot[a_, b_] := Total[a * b, {3}];

(* Vectorised element-wise max of two equally shaped arrays *)
ecoMax[a_, b_] := 0.5 * (a + b + Abs[a - b]);

ecoDilate[m_, r_Integer] := If[r <= 0, m,
  Fold[ecoMax, Flatten[Table[ecoShift[m, {i, j}], {i, -r, r}, {j, -r, r}], 1]]
];

Options[ComputeEcologicalCues] = {
  "DepthStepThreshold" -> 0.02,   (* relative depth jump that counts as an occluding step *)
  "StepNormalThreshold" -> 0.5,    (* |N . d| needed on either side (rejects grazing smooth surfaces) *)
  "AORadiusFraction" -> 0.06,      (* AO world radius as a fraction of camera depth (scale-relative) *)
  "AODirections" -> 8,
  "AOSteps" -> {2, 4, 8, 16, 32},  (* pixel offsets along each direction *)
  "AOBias" -> 0.1,
  "RimCurvatureEpsilon" -> 0.02,   (* |kappa| below this on a contour = inflection (RimSign 0) *)
  "FarDepth" -> 10000.
};

ComputeEcologicalCues[inputs_List, opts : OptionsPattern[]] :=
  Map[ComputeEcologicalCues[#, opts] &, inputs];

ComputeEcologicalCues[f_File, opts : OptionsPattern[]] :=
  ComputeEcologicalCues[First[f], opts];

ComputeEcologicalCues[file_String, opts : OptionsPattern[]] :=
  Module[{exr = Quiet @ Check[EXRIO`ReadEXR[file, opts], $Failed]},
    If[exr =!= $Failed,
      ComputeEcologicalCues[exr, opts],
      $Failed
    ]
  ];

ComputeEcologicalCues[img_Image, opts : OptionsPattern[]] :=
  Module[{exr = Quiet @ Check[EXRIO`ReadEXR[img, opts], $Failed]},
    If[exr =!= $Failed,
      ComputeEcologicalCues[exr, opts],
      $Failed
    ]
  ];

ComputeEcologicalCues[exr : _Association | _?isEXRDataQ, opts : OptionsPattern[]] :=
  Module[{assoc, passes, cues, newPasses},
    assoc = unwrapEXR[exr];
    passes = assoc["Passes"];
    If[!SubsetQ[Keys[passes], {"P", "Nn"}], Return[exr]];
    cues = ecologicalCuesCore[
      N @ Normal[passes["P"]["Data"]],
      N @ Normal[passes["Nn"]["Data"]],
      opts
    ];
    newPasses = AssociationMap[
      <|
        "Type" -> If[ArrayDepth[cues[#]] === 3, "Vector2", "Scalar"],
        "Data" -> NumericArray[cues[#], "Real32"]
      |> &,
      Keys[cues]
    ];
    wrapEXR[exr, Join[assoc, <|"Computed" -> Join[Lookup[assoc, "Computed", <||>], newPasses]|>]]
  ];

ecologicalCuesCore[pIn_, nIn_, opts : OptionsPattern[ComputeEcologicalCues]] :=
  Module[{tau = N @ OptionValue[ComputeEcologicalCues, {opts}, "DepthStepThreshold"],
      nThr = N @ OptionValue[ComputeEcologicalCues, {opts}, "StepNormalThreshold"],
      aoFrac = N @ OptionValue[ComputeEcologicalCues, {opts}, "AORadiusFraction"],
      nDirs = OptionValue[ComputeEcologicalCues, {opts}, "AODirections"],
      steps = OptionValue[ComputeEcologicalCues, {opts}, "AOSteps"],
      aoBias = N @ OptionValue[ComputeEcologicalCues, {opts}, "AOBias"],
      kEps = N @ OptionValue[ComputeEcologicalCues, {opts}, "RimCurvatureEpsilon"],
      far = N @ OptionValue[ComputeEcologicalCues, {opts}, "FarDepth"],
      p, n, z, valid, h, w, nbrs, own, edgeAny, contour, texValid,
      pS, border, px, py, area, lnA, gx, gy, gmag, comp, texGrad,
      viewDir, facing, aoR, dirs, occ, ao,
      phi, phx, phy, phn, nxF, nyF, kappa, rimSign, logDepth},

    {h, w} = Take[Dimensions[pIn], 2];
    z = pIn[[All, All, 3]];
    valid = UnitStep[z - 10^-4] * (1. - UnitStep[z - far]);
    (* Non-hits / invalid samples are treated as infinitely far: ordinally correct for sky/void *)
    z = valid * z + (1. - valid) * far;
    p = pIn;
    p[[All, All, 3]] = z;

    (* Normals: renormalise and orient toward the camera (camera at origin, view ray = P) *)
    n = nIn / Sqrt[Total[nIn^2, {3}] + 10^-12];
    viewDir = p / Sqrt[Total[p^2, {3}] + 10^-12];
    facing = Sign[ecoDot[n, -viewDir] + 10^-12];
    n = n * facing;

    logDepth = Clip[Log[z], {-10., Log[far]}];

    (* ---- Occluding contours with figure/ground ownership ----
       A pixel owns a contour if a 4-neighbour is farther by > tau (relative) and the
       displacement is not tangent to either surface (rejects grazing ground planes). *)
    nbrs = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
    {own, edgeAny} = Transpose @ Map[
      Function[off, Module[{zq, pq, nq, d, dn, step, notTangent, ownQ},
        zq = ecoShift[z, off]; pq = ecoShift[p, off]; nq = ecoShift[n, off];
        d = pq - p;
        dn = Sqrt[Total[d^2, {3}] + 10^-12];
        notTangent = UnitStep[ecoMax[Abs[ecoDot[n, d]], Abs[ecoDot[nq, d]]] / dn - nThr];
        step = UnitStep[Abs[zq - z] - tau * Min[z, zq]] * notTangent;
        ownQ = UnitStep[zq - z] * step;
        {ownQ, step}
      ]],
      nbrs
    ];
    contour = Clip[Total[own], {0., 1.}] * valid;
    edgeAny = Clip[Total[edgeAny], {0., 1.}];
    texValid = (1. - ecoDilate[edgeAny, 2]) * valid;

    (* ---- Optical texture-density gradient (Gibson 1950) ----
       Surface area imaged by one pixel A = |P_x x P_y|; optical texture density ~ A, so
       grad ln A points along the direction of recession / compression. Expressed per image
       width (resolution invariant) and compressed to the unit disk: g / (1 + |g|). *)
    (* Smooth P first: the cue is the macro gradient, not micro-displacement detail *)
    pS = GaussianFilter[p, {{6, 6, 0}, {2.5, 2.5, 0.}}];
    px = 0.5 * (ecoShift[pS, {0, 1}] - ecoShift[pS, {0, -1}]);
    py = 0.5 * (ecoShift[pS, {1, 0}] - ecoShift[pS, {-1, 0}]);
    area = Sqrt[
      (px[[All, All, 2]] py[[All, All, 3]] - px[[All, All, 3]] py[[All, All, 2]])^2 +
      (px[[All, All, 3]] py[[All, All, 1]] - px[[All, All, 1]] py[[All, All, 3]])^2 +
      (px[[All, All, 1]] py[[All, All, 2]] - px[[All, All, 2]] py[[All, All, 1]])^2 + 10^-20];
    lnA = GaussianFilter[Log[area], 3.];
    gx = 0.5 * w * (ecoShift[lnA, {0, 1}] - ecoShift[lnA, {0, -1}]);
    gy = 0.5 * w * (ecoShift[lnA, {1, 0}] - ecoShift[lnA, {-1, 0}]);
    gmag = Sqrt[gx^2 + gy^2];
    border = ConstantArray[0., {h, w}];
    border[[9 ;; h - 8, 9 ;; w - 8]] = 1.;
    texValid = texValid * border;
    comp = texValid / (1. + gmag);
    texGrad = Transpose[{gx * comp, gy * comp}, {3, 1, 2}];

    (* ---- Screen-space horizon ambient occlusion (cheap AO; accurate render deferred to v4) ----
       Radius scales with depth so the cue is scale-relative rather than metric. *)
    aoR = aoFrac * z;
    dirs = Table[{Sin[2. Pi k / nDirs], Cos[2. Pi k / nDirs]}, {k, 0, nDirs - 1}];
    occ = Map[
      Function[dir, Fold[ecoMax,
        Map[
          Function[s, Module[{off, d, dn, c, fall},
            off = Round[s * dir];
            d = ecoShift[p, off] - p;
            dn = Sqrt[Total[d^2, {3}] + 10^-12];
            c = Ramp[ecoDot[n, d] / dn - aoBias];
            fall = Ramp[1. - dn / aoR];
            c * fall
          ]],
          steps
        ]
      ]],
      dirs
    ];
    ao = Clip[1. - Mean[occ], {0., 1.}] * valid + (1. - valid);

    (* ---- Koenderink rim sign ----
       Level-set curvature of smoothed log depth; its normal points from figure to ground,
       so div(n) > 0 on convex occluding contours (elliptic rim, K > 0) and < 0 on concave
       ones (hyperbolic, K < 0). Inflections (|kappa| < eps) mark parabolic rim points. *)
    phi = GaussianFilter[logDepth, 2.];
    phx = 0.5 * (ecoShift[phi, {0, 1}] - ecoShift[phi, {0, -1}]);
    phy = 0.5 * (ecoShift[phi, {1, 0}] - ecoShift[phi, {-1, 0}]);
    phn = Sqrt[phx^2 + phy^2 + 10^-12];
    nxF = phx / phn; nyF = phy / phn;
    kappa = 0.5 * (ecoShift[nxF, {0, 1}] - ecoShift[nxF, {0, -1}]) +
            0.5 * (ecoShift[nyF, {1, 0}] - ecoShift[nyF, {-1, 0}]);
    rimSign = contour * (UnitStep[kappa - kEps] - UnitStep[-kappa - kEps]);

    <|
      "LogDepth" -> logDepth,
      "Contour" -> contour,
      "TextureGradient" -> texGrad,
      "TexValid" -> texValid,
      "Occlusion" -> ao,
      "RimSign" -> rimSign
    |>
  ];

Options[SampleEnvironment] = {"FlipX" -> True, "FlipY" -> False, "InvertDirection"
   -> True, "Return" -> "Luminance"};

SampleEnvironment[nImg_, envMap_, opts : OptionsPattern[]] :=
  Module[{env, nData, envData, h, w, c, dims, normals, nx, ny, nz, xDir,
     yDir, zDir, sign, u, v, row, col, r0, r1, c0, c1, dr, dc, idx00, idx10,
     idx01, idx11, val00, val10, val01, val11, sampled, lum},
    env =
      Which[
        ImageQ[envMap],
          envMap
        ,
        StringQ[envMap] || MatchQ[envMap, _File],
          Import[envMap, "TIFF"]
        ,
        True,
          envMap
      ];
    env =
      If[ListQ[env],
        First[env]
        ,
        env
      ];
    If[!ImageQ[env],
      Return[Failure["InvalidEnvironmentMap", <|"Input" -> envMap|>]]
        
    ];
    nData =
      If[ImageQ[nImg],
        ImageData[nImg]
        ,
        nImg
      ];
    nData =
      If[Head[nData] === NumericArray,
        Normal[nData]
        ,
        nData
      ];
    envData = ImageData[env];
    envData =
      If[Head[envData] === NumericArray,
        Normal[envData]
        ,
        envData
      ];
    {h, w, c} = Dimensions[envData];
    dims = Dimensions[nData[[All, All, 1]]];
    normals = Map[safeNormalize, N[nData], {2}];
    nx = normals[[All, All, 1]];
    ny = normals[[All, All, 2]];
    nz = normals[[All, All, 3]];
    sign =
      If[TrueQ[OptionValue[SampleEnvironment, {opts}, "InvertDirection"
        ]],
        -1.
        ,
        1.
      ];
    xDir = sign nx;
    yDir = sign ny;
    zDir = sign nz;
    If[TrueQ[OptionValue[SampleEnvironment, {opts}, "FlipX"]],
      xDir = -xDir
    ];
    If[TrueQ[OptionValue[SampleEnvironment, {opts}, "FlipY"]],
      yDir = -yDir
    ];
    u = Mod[0.5 + ArcTan[-xDir, zDir] / (2. Pi), 1.];
    v = Clip[ArcCos[Clip[yDir, {-1., 1.}]] / Pi, {0., 1.}];
    row = 1. + (h - 1.) v;
    col = 1. + (w - 1.) u;
    r0 = Clip[Floor[row], {1, h}];
    r1 = Clip[r0 + 1, {1, h}];
    c0 = 1 + Mod[Floor[col] - 1, w];
    c1 = 1 + Mod[c0, w];
    dr = row - r0;
    dc = col - c0;
    idx00 = Transpose[{Flatten[r0], Flatten[c0]}];
    idx10 = Transpose[{Flatten[r1], Flatten[c0]}];
    idx01 = Transpose[{Flatten[r0], Flatten[c1]}];
    idx11 = Transpose[{Flatten[r1], Flatten[c1]}];
    val00 = ArrayReshape[Extract[envData, idx00], Join[dims, {c}]];
    val10 = ArrayReshape[Extract[envData, idx10], Join[dims, {c}]];
    val01 = ArrayReshape[Extract[envData, idx01], Join[dims, {c}]];
    val11 = ArrayReshape[Extract[envData, idx11], Join[dims, {c}]];
    sampled = (1. - dr) (1. - dc) val00 + dr (1. - dc) val10 + (1. - 
      dr) dc val01 + dr dc val11;
    Which[
      OptionValue[SampleEnvironment, {opts}, "Return"] === "RGB",
        Image[sampled, "Real32"]
      ,
      True,
        lum =
          If[c == 1,
            sampled
            ,
            0.2126 sampled[[All, All, 1]] + 0.7152 sampled[[All, All,
               2]] + 0.0722 sampled[[All, All, 3]]
          ];
        Image[lum, "Real32", ColorSpace -> "Grayscale"]
    ]
  ]

(* ::Section:: *)
(* Koenderink Scale-Space / Local-Jet Curvature (J^2_sigma & Weingarten Jet) *)

$KoenderinkShapeClasses = {
  <|"Class" -> 1, "Name" -> "Spherical Cup",     "Range" -> {-1.000, -0.875}, "Center" -> -0.9375, "Color" -> {0.00, 0.55, 0.25}|>,
  <|"Class" -> 2, "Name" -> "Trough",            "Range" -> {-0.875, -0.625}, "Center" -> -0.7500, "Color" -> {0.10, 0.75, 0.55}|>,
  <|"Class" -> 3, "Name" -> "Cylindrical Rut",   "Range" -> {-0.625, -0.375}, "Center" -> -0.5000, "Color" -> {0.20, 0.85, 0.85}|>,
  <|"Class" -> 4, "Name" -> "Saddle Rut",        "Range" -> {-0.375, -0.125}, "Center" -> -0.2500, "Color" -> {0.55, 0.90, 0.60}|>,
  <|"Class" -> 5, "Name" -> "Symmetric Saddle",  "Range" -> {-0.125,  0.125}, "Center" ->  0.0000, "Color" -> {0.95, 0.95, 0.25}|>,
  <|"Class" -> 6, "Name" -> "Saddle Ridge",      "Range" -> { 0.125,  0.375}, "Center" ->  0.2500, "Color" -> {0.98, 0.75, 0.20}|>,
  <|"Class" -> 7, "Name" -> "Cylindrical Ridge", "Range" -> { 0.375,  0.625}, "Center" ->  0.5000, "Color" -> {0.95, 0.50, 0.15}|>,
  <|"Class" -> 8, "Name" -> "Dome",              "Range" -> { 0.625,  0.875}, "Center" ->  0.7500, "Color" -> {0.90, 0.25, 0.15}|>,
  <|"Class" -> 9, "Name" -> "Spherical Cap",     "Range" -> { 0.875,  1.000}, "Center" ->  0.9375, "Color" -> {0.80, 0.05, 0.15}|>
};

KoenderinkShapeCategory[s_List, mask_: None] := Module[{m, cats},
  m = If[mask === None, ConstantArray[1, Dimensions[s]], UnitStep[N[mask] - 0.5]];
  cats = 1 + UnitStep[s + 0.875] + UnitStep[s + 0.625] + UnitStep[s + 0.375] +
         UnitStep[s + 0.125] + UnitStep[s - 0.125] + UnitStep[s - 0.375] +
         UnitStep[s - 0.625] + UnitStep[s - 0.875];
  Clip[cats, {1, 9}] * m
];

Options[ColorizeShapeIndex] = {
  "Discrete" -> True,
  "CurvednessModulation" -> None
};

ColorizeShapeIndex[s_List, mask_: None, opts : OptionsPattern[]] := Module[{
  discreteQ = TrueQ[OptionValue["Discrete"]],
  curvMod = OptionValue["CurvednessModulation"],
  m, cats, lut, rgb, mod
},
  m = If[mask === None, ConstantArray[1.0, Dimensions[s]], Clip[N[mask], {0.0, 1.0}]];
  If[discreteQ,
    cats = KoenderinkShapeCategory[s, m];
    lut = Prepend[Lookup[$KoenderinkShapeClasses, "Color"], {0.08, 0.09, 0.11}];
    rgb = Map[lut[[# + 1]] &, cats, {2}],
    (* Continuous interpolation across the 9 Koenderink palette anchors *)
    Module[{centers = Lookup[$KoenderinkShapeClasses, "Center"], colors = Lookup[$KoenderinkShapeClasses, "Color"], rFn, gFn, bFn},
      rFn = Interpolation[Transpose[{centers, colors[[All, 1]]}], InterpolationOrder -> 1];
      gFn = Interpolation[Transpose[{centers, colors[[All, 2]]}], InterpolationOrder -> 1];
      bFn = Interpolation[Transpose[{centers, colors[[All, 3]]}], InterpolationOrder -> 1];
      rgb = Transpose[{Map[rFn, Clip[s, {-0.9375, 0.9375}], {2}], Map[gFn, Clip[s, {-0.9375, 0.9375}], {2}], Map[bFn, Clip[s, {-0.9375, 0.9375}], {2}]}, {3, 1, 2}];
      rgb = rgb * Map[ConstantArray[#, 3] &, m, {2}] + (1.0 - Map[ConstantArray[#, 3] &, m, {2}]) * 0.09;
    ]
  ];
  If[ListQ[curvMod],
    mod = Clip[N[curvMod], {0.15, 1.0}];
    rgb = rgb * Map[ConstantArray[#, 3] &, mod, {2}];
  ];
  Image[rgb, "Real32"]
];

(* Mask-normalized Gaussian derivative filter: prevents background/void values from bleeding
   across occluding contours when the scale aperture sigma grows large. *)
maskedGaussianSmoothScalar[field_List, mask_List, sigma_?NumericQ] := Module[{rad, wMask, wField},
  If[sigma <= 0.25, Return[field * mask]];
  rad = Max[2, Ceiling[3.5 * sigma]];
  wMask = GaussianFilter[N[mask], {rad, N[sigma]}];
  wField = GaussianFilter[N[field * mask], {rad, N[sigma]}];
  (wField / (wMask + 10^-6)) * mask
];

maskedGaussianSmoothVec3[vecField_List, mask_List, sigma_?NumericQ] := Module[{c1, c2, c3},
  c1 = maskedGaussianSmoothScalar[vecField[[All, All, 1]], mask, sigma];
  c2 = maskedGaussianSmoothScalar[vecField[[All, All, 2]], mask, sigma];
  c3 = maskedGaussianSmoothScalar[vecField[[All, All, 3]], mask, sigma];
  Transpose[{c1, c2, c3}, {3, 1, 2}]
];

(* Gaussian derivative operator D_{dy, dx}(sigma) applied to a 2D scalar field *)
gaussianJetDerivScalar[field_List, sigma_?NumericQ, {dy_Integer, dx_Integer}] := Module[{s = Max[0.5, N[sigma]], rad},
  rad = Max[2, Ceiling[3.5 * s]];
  GaussianFilter[N[field], {rad, s}, {dy, dx}]
];

gaussianJetDerivVec3[vecField_List, sigma_?NumericQ, {dy_Integer, dx_Integer}] := Module[{d1, d2, d3},
  d1 = gaussianJetDerivScalar[vecField[[All, All, 1]], sigma, {dy, dx}];
  d2 = gaussianJetDerivScalar[vecField[[All, All, 2]], sigma, {dy, dx}];
  d3 = gaussianJetDerivScalar[vecField[[All, All, 3]], sigma, {dy, dx}];
  Transpose[{d1, d2, d3}, {3, 1, 2}]
];

Options[ComputeScaleSpaceCurvature] = {
  "Method" -> "WeingartenJet", (* "WeingartenJet" (3D P & Nn) or "MongeJet" (2D 2-Jet on Z) *)
  "Mask" -> Automatic,
  "VisualAngleDegrees" -> None (* Optional: specify scale in degrees of visual angle instead of pixels *)
};

ComputeScaleSpaceCurvature[inputs_List, scales_List, opts : OptionsPattern[]] :=
  Map[ComputeScaleSpaceCurvature[#, scales, opts] &, inputs];

ComputeScaleSpaceCurvature[inputs_List, sigma_?NumericQ, opts : OptionsPattern[]] :=
  Map[ComputeScaleSpaceCurvature[#, sigma, opts] &, inputs];

ComputeScaleSpaceCurvature[f_File, args___] :=
  ComputeScaleSpaceCurvature[First[f], args];

ComputeScaleSpaceCurvature[file_String, args___] :=
  Module[{exr = Quiet @ Check[EXRIO`ReadEXR[file], $Failed]},
    If[exr =!= $Failed,
      ComputeScaleSpaceCurvature[exr, args],
      $Failed
    ]
  ];

ComputeScaleSpaceCurvature[img_Image, args___] :=
  Module[{exr = Quiet @ Check[EXRIO`ReadEXR[img], $Failed]},
    If[exr =!= $Failed,
      ComputeScaleSpaceCurvature[exr, args],
      $Failed
    ]
  ];

ComputeScaleSpaceCurvature[exr : _Association | _?isEXRDataQ, opts : OptionsPattern[]] :=
  ComputeScaleSpaceCurvature[exr, 1.0, opts];

ComputeScaleSpaceCurvature[exr_, scales_List, opts : OptionsPattern[]] :=
  AssociationMap[ComputeScaleSpaceCurvature[exr, #, opts] &, scales];

ComputeScaleSpaceCurvature[exr : _Association | _?isEXRDataQ, sigmaIn_?NumericQ, opts : OptionsPattern[]] := Module[{
  assoc = unwrapEXR[exr],
  method = OptionValue["Method"],
  maskOpt = OptionValue["Mask"],
  visDeg = OptionValue["VisualAngleDegrees"],
  passes, pImg, nImg, zImg, mask, h, w, fov, sigmaPx,
  pSmooth, nSmooth, nNorm, pu, pv, nu, nv,
  e, f, g, l, mCoeff, nCoeff, detI, w11, w12, w21, w22,
  trW, detW, disc, sqrtDisc, k1, k2, meanH, gaussK,
  shapeIndex, curvedness, scaleNormCurv, logCurv, categories,
  zSmooth, zx, zy, zxx, zxy, zyy, denom
},
  passes = assoc["Passes"];
  pImg = N @ Normal[passes["P"]["Data"]];
  nImg = N @ Normal[passes["Nn"]["Data"]];
  {h, w} = Take[Dimensions[pImg], 2];

  (* Resolve foreground mask (prefer Foreground ObjectMask or non-ground depth mask) *)
  mask = Which[
    ListQ[maskOpt], N[maskOpt],
    KeyExistsQ[passes, "ObjectMask"], N @ Normal[passes["ObjectMask"]["Data"]],
    KeyExistsQ[ Lookup[assoc, "Computed", <||>], "ObjectMask"], N @ Normal[assoc["Computed", "ObjectMask", "Data"]],
    KeyExistsQ[passes, "A"], N @ Normal[passes["A"]["Data"]],
    True, UnitStep[pImg[[All, All, 3]] - 10^-4] * UnitStep[20.0 - pImg[[All, All, 3]]]
  ];
  mask = UnitStep[mask - 0.5] * UnitStep[pImg[[All, All, 3]] - 10^-4];

  (* Convert visual angle (degrees) to pixel aperture sigma if requested *)
  fov = Lookup[Lookup[Lookup[assoc, "Metadata", <||>], "Camera", <||>], "FOV", 45.0];
  If[!NumericQ[fov], fov = 45.0];
  sigmaPx = If[NumericQ[visDeg] && visDeg > 0,
    (w / (2.0 * Tan[ fov * Degree / 2.0 ])) * Tan[visDeg * Degree],
    Max[0.5, N[sigmaIn]]
  ];

  If[method === "MongeJet",
    (* ---- Koenderink 2D Local 2-Jet J^2_sigma[z] on Depth Relief ---- *)
    (* Note: camera Z increases away from the eye, so relief height toward observer is z = -P_z *)
    zImg = -pImg[[All, All, 3]];
    zSmooth = maskedGaussianSmoothScalar[zImg, mask, sigmaPx];
    zx  = gaussianJetDerivScalar[zSmooth, sigmaPx, {0, 1}] * mask;
    zy  = gaussianJetDerivScalar[zSmooth, sigmaPx, {1, 0}] * mask;
    zxx = gaussianJetDerivScalar[zSmooth, sigmaPx, {0, 2}] * mask;
    zxy = gaussianJetDerivScalar[zSmooth, sigmaPx, {1, 1}] * mask;
    zyy = gaussianJetDerivScalar[zSmooth, sigmaPx, {2, 0}] * mask;

    (* First and Second Fundamental Forms of Monge patch (x, y, z(x,y)) at scale sigma *)
    e = 1.0 + zx^2;
    f = zx * zy;
    g = 1.0 + zy^2;
    denom = Sqrt[1.0 + zx^2 + zy^2];
    l = zxx / denom;
    mCoeff = zxy / denom;
    nCoeff = zyy / denom;
    detI = e * g - f^2;
    w11 = safeDivide[g * l - f * mCoeff, detI];
    w12 = safeDivide[g * mCoeff - f * nCoeff, detI];
    w21 = safeDivide[-f * l + e * mCoeff, detI];
    w22 = safeDivide[-f * mCoeff + e * nCoeff, detI];
    ,
    (* ---- 3D Manifold Weingarten Scale-Space Jet J^1_sigma[P, Nn] ---- *)
    pSmooth = maskedGaussianSmoothVec3[pImg, mask, sigmaPx];
    nSmooth = maskedGaussianSmoothVec3[nImg, mask, sigmaPx];
    nNorm = nSmooth / Sqrt[Total[nSmooth^2, {3}] + 10^-12];

    pu = gaussianJetDerivVec3[pSmooth, sigmaPx, {0, 1}];
    pv = gaussianJetDerivVec3[pSmooth, sigmaPx, {1, 0}];
    nu = gaussianJetDerivVec3[nNorm, sigmaPx, {0, 1}];
    nv = gaussianJetDerivVec3[nNorm, sigmaPx, {1, 0}];

    e = Total[pu * pu, {3}];
    f = Total[pu * pv, {3}];
    g = Total[pv * pv, {3}];
    l = -Total[pu * nu, {3}];
    mCoeff = -0.5 * (Total[pu * nv, {3}] + Total[pv * nu, {3}]);
    nCoeff = -Total[pv * nv, {3}];
    detI = e * g - f^2;
    w11 = safeDivide[g * l - f * mCoeff, detI];
    w12 = safeDivide[g * mCoeff - f * nCoeff, detI];
    w21 = safeDivide[-f * l + e * mCoeff, detI];
    w22 = safeDivide[-f * mCoeff + e * nCoeff, detI];
  ];

  trW = (w11 + w22) * mask;
  detW = (w11 * w22 - w12 * w21) * mask;
  disc = Ramp[trW^2 - 4.0 * detW];
  sqrtDisc = Sqrt[disc];
  k1 = 0.5 * (trW + sqrtDisc) * mask;
  k2 = 0.5 * (trW - sqrtDisc) * mask;
  meanH = 0.5 * trW;
  gaussK = detW;

  shapeIndex = MapThread[
    If[#1 > 10^-7, -(2.0 / Pi) * ArcTan[#2 / #1], 0.0] &,
    {k1 - k2, k1 + k2},
    2
  ] * mask;

  curvedness = Sqrt[0.5 * (k1^2 + k2^2)] * mask;
  (* Dimensionless Scale-Normalized Curvedness: sigma * C(sigma) *)
  scaleNormCurv = (sigmaPx * curvedness) * mask;
  logCurv = Log[1.0 + curvedness] * mask;
  categories = KoenderinkShapeCategory[shapeIndex, mask];

  <|
    "SigmaPixels" -> sigmaPx,
    "VisualAngleDegrees" -> (2.0 * ArcTan[sigmaPx / (w / (2.0 * Tan[fov * Degree / 2.0]))] / Degree),
    "Method" -> method,
    "ShapeIndex" -> shapeIndex,
    "Curvedness" -> curvedness,
    "ScaleNormalizedCurvedness" -> scaleNormCurv,
    "LogCurvedness" -> logCurv,
    "KoenderinkCategory" -> categories,
    "k1" -> k1,
    "k2" -> k2,
    "H" -> meanH,
    "K" -> gaussK,
    "Mask" -> mask
  |>
];

End[]

EndPackage[]
