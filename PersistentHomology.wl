ReduceMatrix[matrix_] := Module[
  {m, n, R, low, pivot, i, j, col},
  
  {m, n} = Dimensions[matrix];
  R = matrix;
  
  low = Table[0, {n}];
  
  For[j = 1, j <= n, j++,
    col = R[[All, j]];
    nonZero = Position[col, _?(# != 0 &)];
    
    If[Length[nonZero] > 0,
      pivot = nonZero[[-1, 1]];
      low[[j]] = pivot;
      
      For[k = j + 1, k <= n, k++,
        If[R[[pivot, k]] != 0,
          R[[All, k]] = Mod[R[[All, k]] + R[[All, j]], 2];
        ]
      ];
    ];
  ];
  
  {R, low}
]

ComputePersistentHomology[filteredComplexes_, dimension_] := Module[
  {complexes, epsilons, birthDeathPairs, i, j, boundary, reduced, low, 
   birth, death, persistenceDiagram, bettiNumbers},
  
  complexes = filteredComplexes["Filtration"];
  epsilons = filteredComplexes["EpsilonValues"];
  
  birthDeathPairs = {};
  bettiNumbers = Table[0, {Length[complexes]}];
  
  For[i = 1, i <= Length[complexes], i++,
    complex = complexes[[i]];
    boundary = ComputeBoundaryOperator[complex, dimension];
    
    {reduced, low} = ReduceMatrix[boundary];
    
    For[j = 1, j <= Length[low], j++,
      If[low[[j]] != 0,
        birth = epsilons[[Position[low, low[[j]]][[1, 1]]]];
        death = epsilons[[i]];
        
        If[birth < death,
          AppendTo[birthDeathPairs, {birth, death}];
        ];
      ];
    ];
    
    bettiNumbers[[i]] = ComputeBettiNumber[complex, dimension];
  ];
  
  persistenceDiagram = Join[birthDeathPairs, Table[{epsilons[[i]], Infinity}, {i, Length[epsilons]}]];
  
  <|
    "Dimension" -> dimension,
    "BirthDeathPairs" -> birthDeathPairs,
    "PersistenceDiagram" -> persistenceDiagram,
    "EpsilonValues" -> epsilons,
    "BettiNumbers" -> bettiNumbers
  |>
]

ComputeAllPersistentHomology[filteredComplexes_, maxDimension_: 2] := Module[
  {homologies},
  
  homologies = Table[
    ComputePersistentHomology[filteredComplexes, dim],
    {dim, 0, maxDimension}
  ];
  
  <|
    "Homologies" -> homologies,
    "MaxDimension" -> maxDimension
  |>
]

ComputePersistence[birthDeathPairs_] := Module[
  {persistence},
  
  persistence = Table[
    If[pair[[2]] == Infinity, 
      Infinity, 
      pair[[2]] - pair[[1]]
    ],
    {pair, birthDeathPairs}
  ];
  
  persistence
]