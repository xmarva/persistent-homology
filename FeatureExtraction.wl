ExtractTopologicalFeatures[homology_] := Module[
  {birthDeathPairs, persistence, features},
  
  birthDeathPairs = homology["BirthDeathPairs"];
  
  persistence = Table[
    If[pair[[2]] == Infinity, 
      10.0, 
      pair[[2]] - pair[[1]]
    ],
    {pair, birthDeathPairs}
  ];
  
  features = <|
    "PersistenceCount" -> Length[persistence],
    "MeanPersistence" -> If[Length[persistence] > 0, Mean[persistence], 0],
    "MaxPersistence" -> If[Length[persistence] > 0, Max[persistence], 0],
    "PersistenceSum" -> Total[persistence],
    "PersistenceVariance" -> If[Length[persistence] > 0, Variance[persistence], 0],
    "PersistenceEntropy" -> If[Length[persistence] > 0, 
                             -Total[Map[# * Log[#] &, persistence/Total[persistence]]], 0],
    "PersistenceDistribution" -> If[Length[persistence] > 0,
                                   BinCounts[persistence, {0, 1, 0.1}], 
                                   ConstantArray[0, 10]],
    "BettiCurve" -> homology["BettiNumbers"],
    "Dimension" -> homology["Dimension"]
  |>;
  
  features
]

CreatePersistenceImage[homology_, resolution_: 20, sigma_: 0.1] := Module[
  {birthDeathPairs, births, deaths, maxBirth, maxDeath, xRange, yRange, img},
  
  birthDeathPairs = Cases[homology["BirthDeathPairs"], {_, x_} /; x != Infinity];
  
  If[Length[birthDeathPairs] == 0,
    Return[ConstantArray[0, {resolution, resolution}]]
  ];
  
  births = birthDeathPairs[[All, 1]];
  deaths = birthDeathPairs[[All, 2]];
  
  maxBirth = Max[births];
  maxDeath = Max[deaths];
  
  xRange = {0, maxBirth};
  yRange = {0, maxDeath};
  
  img = Table[
    Sum[
      (deaths[[i]] - births[[i]]) * Exp[
        -((x - births[[i]])^2 + (y - deaths[[i]])^2)/(2*sigma^2)
      ],
      {i, Length[birthDeathPairs]}
    ],
    {y, Subdivide[yRange[[1]], yRange[[2]], resolution-1]},
    {x, Subdivide[xRange[[1]], xRange[[2]], resolution-1]}
  ];
  
  img
]

ComputeBettiCurve[homology_] := Module[
  {epsilons, bettiNumbers},
  
  epsilons = homology["EpsilonValues"];
  bettiNumbers = homology["BettiNumbers"];
  
  Transpose[{epsilons, bettiNumbers}]
]

ExtractTDAVectorRepresentation[homology_, numFeatures_: 20] := Module[
  {bettiCurve, persistenceImage, vectorized},
  
  bettiCurve = ComputeBettiCurve[homology];
  persistenceImage = CreatePersistenceImage[homology, Ceiling[Sqrt[numFeatures]]];
  
  vectorized = Join[
    Flatten[persistenceImage],
    {
      Length[homology["BirthDeathPairs"]],
      Mean[Table[pair[[2]]-pair[[1]], {pair, Cases[homology["BirthDeathPairs"], {_, x_} /; x != Infinity]}]],
      Max[homology["BettiNumbers"]]
    }
  ];
  
  Take[vectorized, Min[numFeatures, Length[vectorized]]]
]