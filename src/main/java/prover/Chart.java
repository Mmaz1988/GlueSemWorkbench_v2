package prover;

import prover.categoryGraph.History;

import java.util.HashMap;
import java.util.Set;

public class Chart {

    HashMap<String, Set<History>> atomicChart = new HashMap<>();
    HashMap<String, Set<History>> nonAtomicChart = new HashMap<>();

    HashMap<String, Set<History>> modifierChart = new HashMap<>();

    public Chart(){}
}
