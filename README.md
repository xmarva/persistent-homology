# Persistent Homology

Persistent homology algorithms for topological data analysis (TDA) and extracted topological features integration with ML models. The code analyzes the "persistent" topological features (connectivity, cycles, cavities) in data across different scales.

## Project Overview

1. Generate synthetic data with known topological properties
2. Build simplicial complexes using the Vietoris-Rips algorithm
3. Compute persistent homology
4. Extract topological features
5. Integrate features with machine learning models

## Requirements

- Wolfram Mathematica 13.0 or later
- IGraphM package (for advanced graph operations)

## Installation

1. Clone this repository to your local machine
2. Install the IGraphM package by running:
   ```
   ResourceFunction["IGInstaller"][]
   ```

## Usage

Open Mathematica and run the following commands:

```wolfram
SetDirectory[NotebookDirectory[]];
Get["Main.wl"]

(* Run a complete analysis *)
results = RunPersistentHomologyAnalysis[
  "Torus",   (* data type *)
  500,       (* number of points *)
  1.0,       (* maximum epsilon *)
  0.05,      (* step size *)
  1,         (* homology dimension *)
  "RandomForest"  (* ML method *)
];

(* Examine results *)
results["PersistenceBarcode"]
results["PersistenceDiagram"]
results["MLResults"]["AccuracyComparison"]
```

## Examples

The code supports several data types:
- "Sphere"
- "Torus"
- "MobiusBand"
- "KleinBottle"
- "CirclesCluster"
- "ProteinStructure"

```wolfram
(* Generate and visualize torus data *)
torusData = GenerateData["Torus", 500];
VisualizeData[torusData]

(* Build simplicial complexes *)
complexes = BuildFilteredComplexes[torusData, 0.5, 0.05];

(* Compute persistent homology *)
homology = ComputePersistentHomology[complexes, 1];

(* Visualize persistence diagram *)
VisualizePersistenceDiagram[homology]
```