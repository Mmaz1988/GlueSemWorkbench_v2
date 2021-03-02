package prover.categoryGraph;

import glueSemantics.linearLogic.Premise;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class History {

    public String category;
    public Set<Integer> indexSet;
    public Set<Integer> requirements = new HashSet<>();
    //0 func //1 argument
    public HashMap<Integer,History> parents;
    public Set<Integer> discharges = new HashSet<>();
    public Premise premise;


    public History(String category, Set<Integer> indexSet, HashMap<Integer,History> parents, Premise p)
    {
        this.category = category;
        this.indexSet = indexSet;
        this.parents = parents;
        this.premise = p;
    }



}
