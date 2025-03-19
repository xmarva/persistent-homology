ComputeDistanceMatrix[points_] := DistanceMatrix[points];

BuildVietorisRipsComplex[points_, distanceMatrix_, epsilon_, maxDimension_: 3] := Module[
  {n, edges, simplices, dim},
  
  n = Length[points];
  
  edges = Select[
    Subsets[Range[n], {2}],
    distanceMatrix[[#[[1]], #[[2]]]] <= epsilon &
  ];
  
  simplices = {Range[n]}; 
  AppendTo[simplices, edges]; 
  
  Do[
    dim = k + 1;
    simplices = AppendTo[
      simplices,
      Select[
        Subsets[Range[n], {dim + 1}],
        AllTrue[Subsets[#, {dim}], MemberQ[simplices[[dim]], #] &] &
      ]
    ],
    {k, 1, maxDimension - 1}
  ];
  
  <|
    "Dimension" -> Min[maxDimension, Length[simplices] - 1],
    "Epsilon" -> epsilon,
    "Simplices" -> simplices,
    "NumSimplices" -> Total[Length /@ simplices]
  |>
]

BuildFilteredComplexes[data_, maxEpsilon_, stepSize_, maxDimension_: 3] := Module[
  {points, distanceMatrix, epsilons, complexes},
  
  points = data["Points"];
  distanceMatrix = ComputeDistanceMatrix[points];
  epsilons = Range[0, maxEpsilon, stepSize];
  
  complexes = Table[
    BuildVietorisRipsComplex[points, distanceMatrix, eps, maxDimension],
    {eps, epsilons}
  ];
  
  <|
    "Filtration" -> complexes,
    "EpsilonValues" -> epsilons,
    "DistanceMatrix" -> distanceMatrix
  |>
]

ComputeBoundaryOperator[complex_, dimension_] := Module[
  {simplices, lowerSimplices, boundaryMatrix, i, j, face, sign, k},
  
  If[dimension <= 0 || dimension > complex["Dimension"],
    Return[{}]
  ];
  
  simplices = complex["Simplices"][[dimension + 1]];
  lowerSimplices = complex["Simplices"][[dimension]];
  
  boundaryMatrix = SparseArray[{}, {Length[lowerSimplices], Length[simplices]}];
  
  Do[
    Do[
      face = Delete[simplices[[j]], i];
      sign = (-1)^(i-1);
      
      k = Position[lowerSimplices, face];
      If[Length[k] > 0,
        boundaryMatrix[[k[[1, 1]], j]] = sign
      ],
      {i, 1, dimension + 1}
    ],
    {j, 1, Length[simplices]}
  ];
  
  boundaryMatrix
]

ComputeBettiNumber[complex_, dimension_] := Module[
  {boundaryK, boundaryKPlus1, kernelDim, imageDim},
  
  boundaryK = ComputeBoundaryOperator[complex, dimension];
  boundaryKPlus1 = ComputeBoundaryOperator[complex, dimension + 1];
  
  kernelDim = NullSpace[boundaryK] // Length;
  imageDim = MatrixRank[boundaryKPlus1];
  
  kernelDim - imageDim
]

ComputeAllBettiNumbers[complex_] := Module[
  {dimension, bettiNumbers},
  
  dimension = complex["Dimension"];
  bettiNumbers = Table[
    ComputeBettiNumber[complex, d],
    {d, 0, dimension}
  ];
  
  bettiNumbers
]

VisualizeComplex[complex_, points_] := Module[
  {vertices, edges, triangles, tetrahedra, graphics},
  
  vertices = Point[points];
  
  If[Length[complex["Simplices"]] >= 2,
    edges = Line[points[[#]]] & /@ complex["Simplices"][[2]]
  ];
  
  If[Length[complex["Simplices"]] >= 3,
    triangles = Polygon[points[[#]]] & /@ complex["Simplices"][[3]]
  ];
  
  If[Length[complex["Simplices"]] >= 4,
    tetrahedra = Tetrahedron[points[[#]]] & /@ complex["Simplices"][[4]]
  ];
  
  graphics = {
    {PointSize[0.02], Red, vertices},
    {Thickness[0.002], Blue, edges},
    {EdgeForm[{Thin, Black}], FaceForm[{Blue, Opacity[0.2]}], triangles},
    {EdgeForm[{Thin, Black}], FaceForm[{Green, Opacity[0.1]}], tetrahedra}
  };
  
  Graphics3D[graphics,
    PlotLabel -> "Simplicial Complex (ε = " <> ToString[complex["Epsilon"]] <> ")",
    BoxRatios -> {1, 1, 1},
    Axes -> True
  ]
]