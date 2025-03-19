Needs["IGraphM`"]

SetDirectory[NotebookDirectory[]];

Get["DataGeneration.wl"]
Get["SimplicialComplex.wl"]
Get["PersistentHomology.wl"]
Get["FeatureExtraction.wl"]
Get["MachineLearning.wl"]
Get["Visualization.wl"]

RunPersistentHomologyAnalysis[dataType_String, numPoints_Integer, 
                              maxEpsilon_Real, stepSize_Real, 
                              homologyDimension_Integer, 
                              mlMethod_String] := Module[
  {data, complexes, homology, features, mlResults},
  
  Print["Step 1: Generating data..."];
  data = GenerateData[dataType, numPoints];
  
  Print["Step 2: Building simplicial complexes..."];
  complexes = BuildFilteredComplexes[data, maxEpsilon, stepSize];
  
  Print["Step 3: Computing persistent homology..."];
  homology = ComputePersistentHomology[complexes, homologyDimension];
  
  Print["Step 4: Extracting topological features..."];
  features = ExtractTopologicalFeatures[homology];
  
  Print["Step 5: Integrating with ML model..."];
  mlResults = RunMLWithTopologicalFeatures[data, features, mlMethod];
  
  Print["Step 6: Visualizing results..."];
  VisualizeResults[data, complexes, homology, mlResults];
  
  <|
    "Data" -> data,
    "Complexes" -> complexes,
    "PersistentHomology" -> homology,
    "TopologicalFeatures" -> features,
    "MLResults" -> mlResults,
    "PersistenceDiagram" -> VisualizePersistenceDiagram[homology],
    "PersistenceBarcode" -> VisualizePersistenceBarcode[homology]
  |>
]

CompareTopologies[dataTypes_List, numPoints_Integer] := Module[
  {results, homologies, comparison},
  
  results = Table[
    data = GenerateData[dataType, numPoints];
    complexes = BuildFilteredComplexes[data, 1.0, 0.05];
    homology = ComputePersistentHomology[complexes, 1];
    <| "DataType" -> dataType, "Homology" -> homology |>,
    {dataType, dataTypes}
  ];
  
  homologies = Map[#["Homology"] &, results];
  
  comparison = Grid[{
    {"Data Type", "Betti_0", "Betti_1", "Betti_2", "Persistence Diagram"},
    Sequence @@ Table[
      {
        results[[i, "DataType"]],
        results[[i, "Homology", "BettiNumbers", 1]],
        results[[i, "Homology", "BettiNumbers", 2]],
        results[[i, "Homology", "BettiNumbers", 3]],
        VisualizePersistenceDiagram[results[[i, "Homology"]]]
      },
      {i, Length[results]}
    ]
  }];
  
  comparison
]