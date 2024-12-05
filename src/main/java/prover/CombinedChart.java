package prover;

import prover.categoryGraph.History;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class CombinedChart {

    HashMap<String, Set<History>> atomicChart = new HashMap<>();
    HashMap<String, Set<History>> nonAtomicChart = new HashMap<>();

    HashMap<String, Set<History>> chart = new HashMap<>();

    public CombinedChart(){}

    public CombinedChart(List<History> histories){
        crateCombinedChart(histories);
    }


    public void crateCombinedChart(List<History> histories)
    {
        HashMap<String,Set<History>> atomicChart = new HashMap<>();
        HashMap<String,Set<History>> nonAtomicChart = new HashMap<>();

        for (History h : histories)
        {
            if (h.category.left != null)
            {
                if (!nonAtomicChart.containsKey(h.category.left.toString()))
                {
                    nonAtomicChart.put(h.category.left.toString(),new HashSet<>());
                }
                nonAtomicChart.get(h.category.left.toString()).add(h);
            } else {
                if (!atomicChart.containsKey(h.category.toString()))
                {
                    atomicChart.put(h.category.toString(),new HashSet<>());
                }
                atomicChart.get(h.category.toString()).add(h);
            }
        }
        this.atomicChart = atomicChart;
        this.nonAtomicChart = nonAtomicChart;
    }
}
