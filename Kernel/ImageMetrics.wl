(* ::Package:: *)

(* ::Package:: *)

BeginPackage["ImageMetrics`"]

ToGrayArray::usage="ToGrayArray[img] converts an image to a normalized grayscale array.";
ToRGBImage::usage="ToRGBImage[img, size] converts an image to RGB and resizes it.";
ConformedPair::usage="ConformedPair[img1, img2] returns same-size grayscale arrays for two images.";
L1Distance::usage="L1Distance[img1, img2] gives the mean absolute difference between two images.";
MAE::usage="MAE[img1, img2] is an alias for L1Distance.";
MSE::usage="MSE[img1, img2] gives the mean squared error between two images.";
RMSE::usage="RMSE[img1, img2] gives the root mean squared error between two images.";
PSNR::usage="PSNR[img1, img2] gives the peak signal-to-noise ratio between two images.";
GlobalSSIM::usage="GlobalSSIM[img1, img2] gives a global structural similarity measure.";
LocalSSIM::usage="LocalSSIM[img1, img2] gives a local-window SSIM approximation using Gaussian filtering.";
MeanMetric::usage="MeanMetric[metric, imagePairs] averages a metric over image pairs.";
BuiltInImageDistance::usage="BuiltInImageDistance[img1, img2, distanceFunction] wraps ImageDistance on resized images.";
InceptionScoreFromProbabilities::usage="InceptionScoreFromProbabilities[probs] computes an inception-style score from class probabilities.";
FeatureStatistics::usage="FeatureStatistics[features] returns mean and covariance of a feature matrix.";
RegularizedCovariance::usage="RegularizedCovariance[matrix, eps] adds eps to the diagonal of a covariance matrix.";
FIDFromFeatures::usage="FIDFromFeatures[realFeatures, generatedFeatures] computes a Fr\[EAcute]chet-style distance from feature vectors.";
ImageFeatureDataset::usage="ImageFeatureDataset[images] computes simple feature vectors for images.";
ImageProbabilityDataset::usage="ImageProbabilityDataset[images, classCount] computes demo class probabilities from image brightness.";

Begin["`Private`"]

imagePairQ[img1_,img2_]:=MatchQ[img1,_Image]&&MatchQ[img2,_Image]

ToGrayArray::img="Expected an Image, got `1`.";
ToRGBImage::img="Expected an Image, got `1`.";
ConformedPair::img="Expected two Image arguments.";
L1Distance::img="Expected two Image arguments.";
MSE::img="Expected two Image arguments.";
RMSE::img="Expected two Image arguments.";
PSNR::img="Expected two Image arguments.";
GlobalSSIM::img="Expected two Image arguments.";
LocalSSIM::img="Expected two Image arguments.";
BuiltInImageDistance::img="Expected two Image arguments.";
MeanMetric::pairs="Expected a list of image pairs.";
InceptionScoreFromProbabilities::arg="Expected a nonempty numeric matrix of probabilities.";
FeatureStatistics::arg="Expected a nonempty numeric matrix of feature vectors.";
RegularizedCovariance::arg="Expected a matrix.";
FIDFromFeatures::arg="Expected two nonempty numeric matrices of feature vectors with compatible dimensions.";
ImageFeatureDataset::arg="Expected a nonempty list of images.";
ImageProbabilityDataset::arg="Expected a nonempty list of images and a positive integer class count.";

ToGrayArray[img_Image]:=N @ ImageData @ ColorConvert[RemoveAlphaChannel[img],"Grayscale"]
ToGrayArray[arg_]:=(Message[ToGrayArray::img,HoldForm[arg]]; $Failed)

ToRGBImage[img_Image,size_ : {299,299}]:=ImageResize[ColorConvert[RemoveAlphaChannel[img],"RGB"],size]
ToRGBImage[arg_,size_ : {299,299}]:=(Message[ToRGBImage::img,HoldForm[arg]]; $Failed)

ConformedPair[img1_Image,img2_Image]:=Module[{size,a,b},
  size=Min /@ Transpose[{ImageDimensions[img1],ImageDimensions[img2]}];
  a=ToGrayArray @ ImageResize[img1,size];
  b=ToGrayArray @ ImageResize[img2,size];
  {a,b}
]
ConformedPair[img1_,img2_]:=(Message[ConformedPair::img]; $Failed)

getConformedMasking[img1_,img2_,size_,maskingOption_]:=Switch[maskingOption,
  All | None,
    All,
  Automatic,
    Module[{alpha1,alpha2,m},
      alpha1=If[ImageChannels[img1]==4||ImageChannels[img1]==2,AlphaChannel[img1],None];
      alpha2=If[ImageChannels[img2]==4||ImageChannels[img2]==2,AlphaChannel[img2],None];
      Which[
        Head[alpha1]===Image&&Head[alpha2]===Image,
          m=ImageMultiply[ImageResize[alpha1,size],ImageResize[alpha2,size]];
          Binarize[m,0],
        Head[alpha1]===Image,
          Binarize[ImageResize[alpha1,size],0],
        Head[alpha2]===Image,
          Binarize[ImageResize[alpha2,size],0],
        True,
          All
      ]
    ],
  _Image,
    Binarize[ImageResize[maskingOption,size],0],
  _,
    All
]

Options[L1Distance]={Masking->None};
L1Distance[img1_Image,img2_Image,opts : OptionsPattern[]]:=Module[{a,b,size,mask,diff,picked},
  size=Min /@ Transpose[{ImageDimensions[img1],ImageDimensions[img2]}];
  a=ToGrayArray @ ImageResize[img1,size];
  b=ToGrayArray @ ImageResize[img2,size];
  diff=Abs[a-b];
  mask=getConformedMasking[img1,img2,size,OptionValue[Masking]];
  If[mask===All,
    Mean[Flatten[diff]],
    picked=Pick[Flatten[diff],Flatten[ImageData[RemoveAlphaChannel[mask]]],_?Positive];
    If[Length[picked]==0,0.,Mean[picked]]
  ]
]
L1Distance[img1_,img2_,opts : OptionsPattern[]]:=(Message[L1Distance::img]; $Failed)

MAE[args___]:=L1Distance[args]

Options[MSE]={Masking->None};
MSE[img1_Image,img2_Image,opts : OptionsPattern[]]:=Module[{a,b,size,mask,diff,picked},
  size=Min /@ Transpose[{ImageDimensions[img1],ImageDimensions[img2]}];
  a=ToGrayArray @ ImageResize[img1,size];
  b=ToGrayArray @ ImageResize[img2,size];
  diff=(a-b)^2;
  mask=getConformedMasking[img1,img2,size,OptionValue[Masking]];
  If[mask===All,
    Mean[Flatten[diff]],
    picked=Pick[Flatten[diff],Flatten[ImageData[RemoveAlphaChannel[mask]]],_?Positive];
    If[Length[picked]==0,0.,Mean[picked]]
  ]
]
MSE[img1_,img2_,opts : OptionsPattern[]]:=(Message[MSE::img]; $Failed)

Options[RMSE]=Options[MSE];
RMSE[img1_Image,img2_Image,opts : OptionsPattern[]]:=With[{mse=MSE[img1,img2,opts]},
  If[mse===$Failed,$Failed,Sqrt[mse]]
]
RMSE[img1_,img2_,opts : OptionsPattern[]]:=(Message[RMSE::img]; $Failed)

Options[PSNR]=Join[Options[MSE],{DynamicRange->1.0}];
PSNR[img1_Image,img2_Image,opts : OptionsPattern[]]:=Module[{mse,dynamicRange},
  mse=MSE[img1,img2,FilterRules[{opts},Options[MSE]]];
  dynamicRange=N @ OptionValue[DynamicRange];
  If[mse===$Failed,
    $Failed,
    If[mse==0.,Infinity,10. Log10[(dynamicRange^2)/mse]]
  ]
]
PSNR[img1_,img2_,opts : OptionsPattern[]]:=(Message[PSNR::img]; $Failed)

Options[GlobalSSIM]={Masking->None};
GlobalSSIM[img1_Image,img2_Image,k1_ : 0.01,k2_ : 0.03,luminanceRange_ : 1.,opts : OptionsPattern[]]:=Module[
  {a,b,size,mask,x,y,muX,muY,sigmaX2,sigmaY2,sigmaXY,c1,c2},
  size=Min /@ Transpose[{ImageDimensions[img1],ImageDimensions[img2]}];
  a=ToGrayArray @ ImageResize[img1,size];
  b=ToGrayArray @ ImageResize[img2,size];
  mask=getConformedMasking[img1,img2,size,OptionValue[Masking]];
  If[mask===All,
    x=Flatten[a];
    y=Flatten[b],
    x=Pick[Flatten[a],Flatten[ImageData[RemoveAlphaChannel[mask]]],_?Positive];
    y=Pick[Flatten[b],Flatten[ImageData[RemoveAlphaChannel[mask]]],_?Positive]
  ];
  If[Length[x]==0||Length[y]==0,
    0.,
    muX=Mean[x];
    muY=Mean[y];
    sigmaX2=Variance[x];
    sigmaY2=Variance[y];
    sigmaXY=Covariance[x,y];
    c1=(k1 luminanceRange)^2;
    c2=(k2 luminanceRange)^2;
    ((2 muX muY+c1) (2 sigmaXY+c2))/((muX^2+muY^2+c1) (sigmaX2+sigmaY2+c2))
  ]
]
GlobalSSIM[img1_,img2_,k1_ : 0.01,k2_ : 0.03,luminanceRange_ : 1.,opts : OptionsPattern[]]:=(Message[GlobalSSIM::img]; $Failed)

Options[LocalSSIM]={Masking->None};
LocalSSIM[img1_Image,img2_Image,sigma_ : 1.5,k1_ : 0.01,k2_ : 0.03,luminanceRange_ : 1.,opts : OptionsPattern[]]:=Module[
  {a,b,mask,size,muX,muY,muX2,muY2,muXY,sigmaX2,sigmaY2,sigmaXY,c1,c2,ssimMap,picked},
  size=Min /@ Transpose[{ImageDimensions[img1],ImageDimensions[img2]}];
  a=ToGrayArray @ ImageResize[img1,size];
  b=ToGrayArray @ ImageResize[img2,size];
  muX=GaussianFilter[a,sigma];
  muY=GaussianFilter[b,sigma];
  muX2=muX^2;
  muY2=muY^2;
  muXY=muX muY;
  sigmaX2=GaussianFilter[a^2,sigma]-muX2;
  sigmaY2=GaussianFilter[b^2,sigma]-muY2;
  sigmaXY=GaussianFilter[a b,sigma]-muXY;
  c1=(k1 luminanceRange)^2;
  c2=(k2 luminanceRange)^2;
  ssimMap=((2 muXY+c1) (2 sigmaXY+c2))/((muX2+muY2+c1) (sigmaX2+sigmaY2+c2));
  mask=getConformedMasking[img1,img2,size,OptionValue[Masking]];
  If[mask===All,
    Mean[Flatten[ssimMap]],
    picked=Pick[Flatten[ssimMap],Flatten[ImageData[RemoveAlphaChannel[mask]]],_?Positive];
    If[Length[picked]==0,0.,Mean[picked]]
  ]
]
LocalSSIM[img1_,img2_,sigma_ : 1.5,k1_ : 0.01,k2_ : 0.03,luminanceRange_ : 1.,opts : OptionsPattern[]]:=(Message[LocalSSIM::img]; $Failed)

MeanMetric[metric_,imagePairs_List] /; VectorQ[imagePairs,MatchQ[#,{_Image,_Image}] &]:=Mean[metric @@@ imagePairs]
MeanMetric[metric_,imagePairs_]:=(Message[MeanMetric::pairs]; $Failed)

normalizeImageDistanceFunction[Automatic]:=Automatic;
normalizeImageDistanceFunction["Euclidean"]:=EuclideanDistance;
normalizeImageDistanceFunction["SquaredEuclidean"]:=SquaredEuclideanDistance;
normalizeImageDistanceFunction["NormalizedSquaredEuclidean"]:=NormalizedSquaredEuclideanDistance;
normalizeImageDistanceFunction["Manhattan"]:=ManhattanDistance;
normalizeImageDistanceFunction["Cosine"]:=CosineDistance;
normalizeImageDistanceFunction["Correlation"]:=CorrelationDistance;
normalizeImageDistanceFunction[other_]:=other;

Options[BuiltInImageDistance]=Options[ImageDistance];
BuiltInImageDistance[img1_Image,img2_Image,distanceFunction_ : Automatic,opts : OptionsPattern[]]:=Module[{size,a,b,f},
  size=Min /@ Transpose[{ImageDimensions[img1],ImageDimensions[img2]}];
  a=ImageResize[img1,size];
  b=ImageResize[img2,size];
  f=normalizeImageDistanceFunction[distanceFunction];
  If[
    f===Automatic,
    ImageDistance[a,b,opts],
    ImageDistance[a,b,DistanceFunction->f,opts]
  ]
]
BuiltInImageDistance[img1_,img2_,distanceFunction_ : Automatic,opts : OptionsPattern[]]:=(Message[BuiltInImageDistance::img]; $Failed)

InceptionScoreFromProbabilities[probabilities : {{__?NumericQ} ..}]:=Module[{probs,marginal,klValues,eps=10.^-20},
  probs=N[probabilities];
  marginal=Mean[probs];
  klValues=Total[# Log[Clip[#,{eps,Infinity}]/Clip[marginal,{eps,Infinity}]]] & /@ probs;
  Exp[Mean[klValues]]
]
InceptionScoreFromProbabilities[arg_]:=(Message[InceptionScoreFromProbabilities::arg]; $Failed)

FeatureStatistics[features : {{__?NumericQ} ..}]:=<|
  "Mean"->Mean[features],
  "Covariance"->Covariance[features]
|>
FeatureStatistics[arg_]:=(Message[FeatureStatistics::arg]; $Failed)

RegularizedCovariance[matrix_?MatrixQ,eps_ : 10.^-6]:=N[matrix]+eps IdentityMatrix[Length[matrix]]
RegularizedCovariance[arg_,eps_ : 10.^-6]:=(Message[RegularizedCovariance::arg]; $Failed)

coerceFeatureMatrix[features_]:=Module[{m=N @ Normal[features]},
  If[MatrixQ[m,NumericQ]&&Length[m]>0,m,$Failed]
]

FIDFromFeatures[realFeatures_,generatedFeatures_,eps_ : 10.^-6]:=Module[
  {real,gen,muR,muG,sigmaR,sigmaG,sqrtTerm},
  real=coerceFeatureMatrix[realFeatures];
  gen=coerceFeatureMatrix[generatedFeatures];
  If[
    real===$Failed||gen===$Failed||
    Length[real]<2||Length[gen]<2||
    Length[First[real]]=!=Length[First[gen]],
    Message[FIDFromFeatures::arg];
    Return[$Failed]
  ];
  muR=Mean[real];
  muG=Mean[gen];
  sigmaR=RegularizedCovariance[Covariance[real],eps];
  sigmaG=RegularizedCovariance[Covariance[gen],eps];
  sqrtTerm=Re @ Quiet @ Chop @ MatrixPower[sigmaR . sigmaG,1/2];
  Chop[Norm[muR-muG]^2+Tr[sigmaR+sigmaG-2 sqrtTerm]]
]

ImageFeatureDataset[images : {__Image}]:=Module[{gray},
  gray=ToGrayArray /@ images;
  N @ Map[
    Function[array,
      With[{flat=Flatten[array]},
        {
          Mean[flat],
          StandardDeviation[flat],
          Skewness[flat],
          Kurtosis[flat],
          Mean[Flatten[Abs[Differences[array,{1}]]]],
          Mean[Flatten[Abs[Differences[array,{2}]]]]
        }
      ]
    ],
    gray
  ]
]
ImageFeatureDataset[arg_]:=(Message[ImageFeatureDataset::arg]; $Failed)

ImageProbabilityDataset[images : {__Image},classCount_Integer : 5] /; classCount>0:=Module[{means,raw},
  means=Mean[Flatten[ToGrayArray[#]]] & /@ images;
  raw=Table[
    Normalize[
      Exp[-(Range[classCount]-Rescale[means[[i]],{0,1},{1,classCount}])^2],
      Total
    ],
    {i,Length[means]}
  ];
  raw
]
ImageProbabilityDataset[arg_,classCount_Integer : 5]:=(Message[ImageProbabilityDataset::arg]; $Failed)

End[]

EndPackage[]
