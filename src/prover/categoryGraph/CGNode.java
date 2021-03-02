package prover.categoryGraph;

import glueSemantics.linearLogic.LLTerm;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class CGNode {

    public enum type {
        CATEGORY,
        CONNECTOR,
        SCC
    }
    public String category;
    public type nodeType;
    public Set<History> histories = new HashSet<>();

    public CGNode(String category, type nodeType)
    {
        this.category = category;
        this.nodeType = nodeType;
    }

    @Override
    public String toString()
    {
        return this.category;
    }


}
