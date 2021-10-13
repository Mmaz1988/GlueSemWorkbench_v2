package prover.categoryGraph;

import glueSemantics.linearLogic.Category;
import glueSemantics.linearLogic.Premise;
import prover.LLProver;
import prover.ProverException;
import prover.VariableBindingException;

import java.util.*;

public class History {

    public LLProver prover;
    public Category category;
    public Set<Integer> indexSet;
    public Integer mainIndex;
    public Integer lastModifierMainIndex;
    public Set<Integer> requirements = new HashSet<>();
    public Set<Integer> discharges = new HashSet<>();
    //0 func //1 argument
    public Set<HashMap<Integer,History>> parents;
    public Premise p;
    public QuantForce quantifierForce;
    public enum QuantForce {
        UNIVERSAL,
        EXISTENTIAL,
        OTHER
    }


    public History(Category category, Set<Integer> indexSet, Set<HashMap<Integer,History>> parents, Premise p, LLProver prover)
    {
        this.prover = prover;
        this.category = category;
        this.indexSet = indexSet;
        this.parents = parents;
        this.p = p;
        calculateMainIndex();

        /*

        if (p.getSemTerm() instanceof SemanticExpression)
        {
            if (p.getSemTerm() instanceof SemQuantEx) {
                if (((SemQuantEx) p.getSemTerm()).getQuantifier() == SemQuantEx.SemQuant.UNI) {
                    this.quantifierForce = QuantForce.UNIVERSAL;
                } else if (((SemQuantEx) p.getSemTerm()).getQuantifier() == SemQuantEx.SemQuant.EX) {
                    this.quantifierForce = QuantForce.EXISTENTIAL;
                }
            }
            else{
                this.quantifierForce = QuantForce.OTHER;
                }
            }

*/

    }

    public History(Category category, Set<Integer> indexSet, Set<HashMap<Integer,History>> parents, LLProver prover)
    {
        this.prover = prover;
        this.category = category;
        this.indexSet = indexSet;
        this.parents = parents;
        calculateMainIndex();

        this.quantifierForce = this.parents.stream().findAny().get().get(0).quantifierForce;
    }

    @Override
    public String toString() {


        return category.toString() + " " + indexSet + " (" + parents + ")";
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
                Premise r = prover.combinePremises(p,q,resultBuilder);
                if (r != null)
                {
                    prover.db.combinations++;
                    results.add(r);
                }
            }
        }
        return results;



    }

    public void calculateMainIndex()
    {
        if (indexSet.size() == 1)
        {
            this.mainIndex = indexSet.stream().findAny().get();
        } else
        {
            this.mainIndex = parents.stream().findAny().get().get(0).mainIndex;
        }

    }

}
