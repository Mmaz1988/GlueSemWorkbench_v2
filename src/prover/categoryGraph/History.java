package prover.categoryGraph;

import glueSemantics.linearLogic.Category;
import glueSemantics.linearLogic.Premise;
import prover.LLProver1;
import prover.ProverException;
import prover.VariableBindingException;

import java.util.*;

public class History {

    public Category category;
    public Set<Integer> indexSet;
    public Set<Integer> requirements = new HashSet<>();
    public Set<Integer> discharges = new HashSet<>();
    //0 func //1 argument
    public Set<HashMap<Integer,History>> parents;
    public Premise p;


    public History(Category category, Set<Integer> indexSet, Set<HashMap<Integer,History>> parents, Premise p)
    {
        this.category = category;
        this.indexSet = indexSet;
        this.parents = parents;
        this.p = p;

    }

    public History(Category category, Set<Integer> indexSet, Set<HashMap<Integer,History>> parents)
    {
        this.category = category;
        this.indexSet = indexSet;
        this.parents = parents;
    }

    @Override
    public String toString() {


        return category.toString() + indexSet + "(" + parents + ")";
    }

    public List<Premise> calculateSolutions(StringBuilder resultBuilder) throws VariableBindingException, ProverException {
        Set<Premise> func = new HashSet<>();
        Set<Premise> arg = new HashSet<>();
        List<Premise> results = new ArrayList<>();

        for (HashMap<Integer,History> parentLinks : parents)
        {
            if (parentLinks.get(0).p != null)
            {
                func.add(parentLinks.get(0).p);
            } else
            {
                func.addAll(parentLinks.get(0).calculateSolutions(resultBuilder));
            }

            if (parentLinks.get(1).p != null)
            {
                arg.add(parentLinks.get(1).p);
            } else
            {
                arg.addAll(parentLinks.get(1).calculateSolutions(resultBuilder));
            }


        }
        for (Premise p : func)
        {
            for (Premise q : arg)
            {
                Premise r = LLProver1.combinePremises(p,q,resultBuilder);
                if (r != null)
                {
                    results.add(r);
                }
            }
        }
        return results;



    }


}
