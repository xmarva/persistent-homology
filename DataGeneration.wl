GenerateData[dataType_String, numPoints_Integer] := Module[
  {data},
  
  data = Switch[dataType,
    "Sphere", RandomPoint[Sphere[], numPoints],
    
    "Torus", Module[
      {r1 = 3, r2 = 1, theta, phi},
      Table[
        theta = RandomReal[{0, 2 Pi}];
        phi = RandomReal[{0, 2 Pi}];
        {(r1 + r2 Cos[phi]) Cos[theta], 
         (r1 + r2 Cos[phi]) Sin[theta], 
         r2 Sin[phi]},
        {numPoints}
      ]
    ],
    
    "MobiusBand", Module[
      {width = 1, theta, t},
      Table[
        theta = RandomReal[{0, 2 Pi}];
        t = RandomReal[{-width/2, width/2}];
        {(1 + t Cos[theta/2]) Cos[theta], 
         (1 + t Cos[theta/2]) Sin[theta], 
         t Sin[theta/2]},
        {numPoints}
      ]
    ],
    
    "KleinBottle", Module[
      {a = 3, theta, phi},
      Table[
        theta = RandomReal[{0, 2 Pi}];
        phi = RandomReal[{0, 2 Pi}];
        If[theta <= Pi,
          {(a + Cos[theta/2] Sin[phi] - Sin[theta/2] Sin[2 phi]) Cos[theta],
           (a + Cos[theta/2] Sin[phi] - Sin[theta/2] Sin[2 phi]) Sin[theta],
           Sin[theta/2] Sin[phi] + Cos[theta/2] Sin[2 phi]},
          {(a + Cos[theta/2] Sin[phi] + Sin[theta/2] Sin[2 phi]) Cos[theta],
           (a + Cos[theta/2] Sin[phi] + Sin[theta/2] Sin[2 phi]) Sin[theta],
           Sin[theta/2] Sin[phi] - Cos[theta/2] Sin[2 phi]}
        ],
        {numPoints}
      ]
    ],
    
    "CirclesCluster", Module[
      {centers, radii, pointsPerCluster, clusterID},
      centers = {{0, 0, 0}, {5, 0, 0}, {0, 5, 0}, {5, 5, 0}};
      radii = {1, 0.8, 1.2, 0.7};
      pointsPerCluster = Floor[numPoints/Length[centers]];
      
      Flatten[
        Table[
          clusterID = i;
          Table[
            centers[[clusterID]] + radii[[clusterID]] * RandomPoint[Sphere[{0, 0, 0}, 1], 1][[1]],
            {pointsPerCluster}
          ],
          {i, 1, Length[centers]}
        ], 1
      ]
    ],
    
    "ProteinStructure", RandomVariate[
      MultinormalDistribution[{0, 0, 0}, 
                            {{1, 0.7, 0.3}, {0.7, 1, 0.5}, {0.3, 0.5, 1}}],
      numPoints
    ],
    
    _, RandomPoint[Sphere[], numPoints]
  ];
  
  <|
    "Points" -> data,
    "Type" -> dataType,
    "Count" -> numPoints
  |>
]

AddNoiseToData[data_, noiseLevel_] := Module[
  {points},
  points = data["Points"];
  data["Points"] = points + RandomReal[{-noiseLevel, noiseLevel}, Dimensions[points]];
  data
]

NormalizeData[data_] := Module[
  {points, normalized},
  points = data["Points"];
  normalized = Rescale[points, {Min[points], Max[points]}, {-1, 1}];
  data["Points"] = normalized;
  data
]

SplitData[data_, trainRatio_: 0.8] := Module[
  {points, numPoints, trainSize, trainIndices, testIndices},
  points = data["Points"];
  numPoints = Length[points];
  trainSize = Floor[numPoints * trainRatio];
  
  trainIndices = RandomSample[Range[numPoints], trainSize];
  testIndices = Complement[Range[numPoints], trainIndices];
  
  {
    <|
      "Points" -> points[[trainIndices]],
      "Type" -> data["Type"],
      "Count" -> trainSize
    |>,
    <|
      "Points" -> points[[testIndices]],
      "Type" -> data["Type"],
      "Count" -> numPoints - trainSize
    |>
  }
]

VisualizeData[data_] := Module[
  {points, title},
  points = data["Points"];
  title = StringJoin["Data Visualization: ", data["Type"], " (", ToString[data["Count"]], " points)"];
  
  ListPointPlot3D[points, 
    PlotStyle -> {PointSize[0.01], Opacity[0.7]},
    BoxRatios -> {1, 1, 1},
    PlotLabel -> title,
    AxesLabel -> {"X", "Y", "Z"}
  ]
]