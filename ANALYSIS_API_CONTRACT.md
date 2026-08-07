# Analysis API Contract

The analysis workflow is coordinated by the frontend. GSWB and LiGER each
produce one layer of a derived sequence; neither service owns the complete
`XlePlusGlueDocument`.

## Ownership

```text
Frontend coordinator
  owns XlePlusGlueDocument, source sentences, derived sequences, selections

LiGER
  owns syntactic analyses, linguistic structures, syntax graphs, syntax merge

GSWB
  owns semantic analyses, DRS graphs, semantic alternatives, semantic merge
```

## Sentence Analysis

The frontend first creates a `Sentence` containing all LiGER syntax analyses.
It then calls GSWB deduction for that sentence. GSWB returns solution wrappers
for compatibility and also exposes `semanticAnalyses` plus `synSemMapping` in
the `GswbOutput` analysis projection.

Discriminants are sentence-level state. The frontend retains every semantic
alternative and stores the active subset as sentence `selectedSemanticIds`.
The selected IDs, not the filtered UI list alone, are the input to the next
sequence operation.

## Sequence Merge

`POST /merge_sequence_semantics` with `parts` is the canonical semantic merge
request. Each ordered `GswbSemanticMergePart` contains a semantic graph, its
semantic ID, its syntactic origin, and provenance. It does not contain a LiGER
syntax structure.

The request produces one semantic merge result. Its semantic ID is the ordered
composite of parent semantic IDs:

```text
sem-1 + sem-7 -> sem-1+sem-7
```

The frontend separately sends the parent syntax structures to LiGER. It then
combines the semantic result and syntax result into a `Sequence` and stores
that sequence as a derived document element.

The `graphs` and `semantics` parallel arrays remain only for legacy consumers.
New analysis code must use `parts`.

## Graph Layers

GSWB `graph` is the canonical LFGxDRT semantic graph. It is not a LiGER
linguistic structure. The semantic-analysis `structure` field is reserved for
future coordinator-supplied linguistic structure and is not populated by GSWB.

## Persistence

The frontend owns the volatile analysis document and persists it through the
Redis `/analysis_document/<session>` API. Analysis sessions use generated IDs.
Batch, regression, and inference session namespaces are separate workflows.
