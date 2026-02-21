package prover.categoryGraph;

import glueSemantics.linearLogic.Category;
import glueSemantics.linearLogic.Premise;
import prover.LLProver;
import prover.ProverException;
import prover.VariableBindingException;

import java.util.*;
import java.util.stream.Collectors;

public class History {

    public LLProver prover;
    public Category category;
    public LinkedHashSet<Integer> indexSet;
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

    public HashSet<String> scopeDiscriminants = new HashSet<>();

    public String stage;

    public List<Premise> results;
    public History(Category category, LinkedHashSet<Integer> indexSet, Set<HashMap<Integer,History>> parents, Premise p, LLProver prover)
    {
        this.prover = prover;
        this.category = category;
        this.indexSet = indexSet;
        this.parents = parents;
        this.p = p;
        if (p != null && p.stage != null) {
            this.stage = p.stage;
        }
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

    public History(Category category, LinkedHashSet<Integer> indexSet, Set<HashMap<Integer,History>> parents, LLProver prover)
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

        if (results != null)
        {
            return results;
        }

        List<Premise> results = new ArrayList<>();

        for (HashMap<Integer,History> parentLinks : parents)
        {
            Set<Premise> func = new HashSet<>();
            Set<Premise> arg = new HashSet<>();


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

            for (Premise p : func)
            {


                for (Premise q : arg)
                {

                    prover.db.attemptedCombination++;
                    Premise r = prover.combinePremises(p,q);
                    if (r != null ) {
                        if (r.getPremiseIDs().equals(this.indexSet)) {
                            resultBuilder.append("Combining function " + p.toString() + " with argument " + q.toString() + " to get " + r.toString() + "\n");
                            prover.db.combinations++;
                            results.add(r);
                        }
                    }
                }
            }

        }


        if (!results.isEmpty())
        {
            this.results = results;
        }

        return results;



    }

    public List<Premise> calculateSolutions() throws VariableBindingException, ProverException {

        List<Premise> results = new ArrayList<>();

        for (HashMap<Integer,History> parentLinks : parents)
        {
            Set<Premise> func = new HashSet<>();
            Set<Premise> arg = new HashSet<>();

            if (parentLinks.get(0).p != null)
            {
                func.add(parentLinks.get(0).p);
            } else
            {
                func.addAll(parentLinks.get(0).calculateSolutions());
            }
            if (parentLinks.get(1).p != null)
            {
                arg.add(parentLinks.get(1).p);
            } else
            {
                arg.addAll(parentLinks.get(1).calculateSolutions());
            }
            for (Premise p : func)
            {
                for (Premise q : arg)
                {
                    /* TODO this optimization only applies inside an SCC. To do this, we need to calculate solutions of histories that go into an SCC?
                    Premise r = null;
                    if (this.prover.getSettings().isParseSemantics())
                    {
                        if (SemanticExpression.nonScopingQuantifiers((SemanticExpression) p.getSemTerm(), (SemanticExpression) q.getSemTerm()))
                        {
                            if (parentLinks.get(0).mainIndex < parentLinks.get(1).mainIndex)
                            {
                                r = prover.combinePremises(p,q);
                            }
                        } else
                        {
                            r = prover.combinePremises(p,q);
                        }
                    } else {
                        r = prover.combinePremises(p, q);
                    }

                     */
                    prover.db.attemptedCombination++;
                    Premise r = prover.combinePremises(p,q);
                    if (r != null ) {
                        if (r.getPremiseIDs().equals(this.indexSet)) {
                            prover.db.combinations++;
                            results.add(r);
                        }
                    }
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

    public static List<History> categorySort(List<History> histories){
        List<History> output = new ArrayList<>();
        HashMap<Integer,List<History>> sortByDepth = new HashMap<>();

        for (History h : histories)
        {
            Integer depthCounter = 0;
            Category current = h.category;

            while(current.left != null)
            {
                depthCounter++;
                current = current.right;
            }

            sortByDepth.computeIfAbsent(depthCounter, k -> new ArrayList<>());
            sortByDepth.get(depthCounter).add(h);
        }

        //sort keys of sortByDepth inascending order
        List<Integer> sortedKeys = new ArrayList<>(sortByDepth.keySet());
        Collections.sort(sortedKeys);

        for (Integer i : sortedKeys)
        {
            //sort histories in sortByDepth.get(i) by mainIndex
            output.addAll(sortByDepth.get(i).stream().sorted(Comparator.comparingInt(o -> o.mainIndex)).collect(Collectors.toList()));
        }

        return output;

    }


    public String printParentGraph() {

return null;
    }

}
