RunMLWithTopologicalFeatures[data_, topologicalFeatures_, method_] := Module[
  {points, labels, baseFeatures, combinedFeatures, splitRatio, 
   trainData, testData, baseModel, enhancedModel, 
   baseAccuracy, enhancedAccuracy},
  
  points = data["Points"];
  
  If[data["Type"] == "CirclesCluster",
    labels = Table[
      Which[
        EuclideanDistance[point, {0, 0, 0}] < 2, "Cluster1",
        EuclideanDistance[point, {5, 0, 0}] < 2, "Cluster2",
        EuclideanDistance[point, {0, 5, 0}] < 2, "Cluster3",
        True, "Cluster4"
      ],
      {point, points}
    ],
    
    labels = Table[
      If[point[[3]] > 0, "Class1", "Class2"],
      {point, points}
    ]
  ];
  
  baseFeatures = Map[
    {#[[1]], #[[2]], #[[3]], 
     EuclideanDistance[#, Mean[points]],
     Total[#^2]} &,
    points
  ];
  
  topologicalVector = ExtractTDAVectorRepresentation[topologicalFeatures];
  
  combinedFeatures = Map[
    Join[#, topologicalVector] &,
    baseFeatures
  ];
  
  splitRatio = 0.7;
  {trainData, testData} = TakeDrop[
    RandomSample[Transpose[{baseFeatures, labels}]],
    Floor[splitRatio*Length[points]]
  ];
  
  baseModel = Classify[trainData, Method -> method];
  baseAccuracy = ClassifierMeasurements[baseModel, testData, "Accuracy"];
  
  {trainDataEnhanced, testDataEnhanced} = TakeDrop[
    RandomSample[Transpose[{combinedFeatures, labels}]],
    Floor[splitRatio*Length[points]]
  ];
  
  enhancedModel = Classify[trainDataEnhanced, Method -> method];
  enhancedAccuracy = ClassifierMeasurements[enhancedModel, testDataEnhanced, "Accuracy"];
  
  <|
    "BaseModel" -> baseModel,
    "EnhancedModel" -> enhancedModel,
    "BaseAccuracy" -> baseAccuracy,
    "EnhancedAccuracy" -> enhancedAccuracy,
    "AccuracyImprovement" -> enhancedAccuracy - baseAccuracy,
    "AccuracyComparison" -> BarChart[
      {baseAccuracy, enhancedAccuracy},
      ChartLabels -> {"Without TDA", "With TDA"},
      PlotLabel -> "Classification Accuracy",
      AxesLabel -> {"Model", "Accuracy"},
      PlotRange -> {0, 1}
    ]
  |>
]

ComputeFeatureImportance[data_, topologicalFeatures_, method_: "RandomForest"] := Module[
  {points, labels, baseFeatures, combinedFeatures, model, importance},
  
  points = data["Points"];
  
  labels = Table[
    If[point[[3]] > 0, "Class1", "Class2"],
    {point, points}
  ];
  
  baseFeatures = Map[
    {#[[1]], #[[2]], #[[3]], 
     EuclideanDistance[#, Mean[points]],
     Total[#^2]} &,
    points
  ];
  
  topologicalVector = ExtractTDAVectorRepresentation[topologicalFeatures];
  
  combinedFeatures = Map[
    Join[#, topologicalVector] &,
    baseFeatures
  ];
  
  model = Classify[Transpose[{combinedFeatures, labels}], Method -> method];
  
  importance = ClassifierInformation[model, "FeatureImportances"];
  
  <|
    "FeatureImportance" -> importance,
    "TopologicalFeatureImportance" -> Total[importance[[6 ;;]]],
    "GeometricFeatureImportance" -> Total[importance[[;; 5]]],
    "Visualization" -> BarChart[
      importance,
      ChartLabels -> Join[
        {"X", "Y", "Z", "Distance", "NormSquared"},
        Table["Topo" <> ToString[i], {i, Length[topologicalVector]}]
      ],
      PlotLabel -> "Feature Importance",
      AxesLabel -> {"Feature", "Importance"}
    ]
  |>
]