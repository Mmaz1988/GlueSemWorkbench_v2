package gluePaP.linearLogic;


import Prover.Equality;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LLUniversalQuant extends LLTerm{
    private final String symbol = "\u2200";
    private LLAtom variable;
    private LLFormula term;
    private HashMap<LLAtom,List<LLAtom>> boundVariables;

    public LLAtom getVariable() {
        return variable;
    }

    public LLFormula getTerm() {
        return term;
    }

    public LLUniversalQuant(LLAtom variable, LLFormula term) {
        this.variable = variable;
        this.term = term;
        this.boundVariables = new HashMap<LLAtom,List<LLAtom>>();

        this.boundVariables.put(variable,findBoundOccurrences(this.term));

        System.out.println(boundVariables);

    }

    public String toString() {

        return symbol + variable + "." + term;
    }

    @Override
    // TODO is this required?
    public boolean checkEquivalence(LLTerm term) {
/*        if (term instanceof LLUniversalQuant)
            return true;
        return false;*/
        return true;
    }
/*
    public void findBoundOccurences(){

        if (term.getLhs() instanceof LLAtom) {
            if (((LLAtom) term.getLhs()).getLLtype() == LLAtom.LLType.VAR){

                if (this.variable.checkEquivalence(term.getLhs()))
                {
                    this.boundVariables.get(this.variable).add((LLAtom) term.getLhs());
                }
            }

        } else if ( term.getLhs() instanceof LLFormula)

    }
*/


    public List<LLAtom> findBoundOccurrences(LLTerm term){

        if (term instanceof LLAtom) {
            if (((LLAtom) term).getLLtype() == LLAtom.LLType.VAR){

                if (this.variable.checkEquivalence(term))
                {
                    List <LLAtom> var = new ArrayList<>();
                    var.add((LLAtom) term);
                    return var;
                }

            }


        } else if ( term instanceof LLFormula)
        {
            List <LLAtom> right = findBoundOccurrences(((LLFormula) term).getLhs());
            List <LLAtom> left = findBoundOccurrences(((LLFormula) term).getRhs());

            return Stream.concat(right.stream(), left.stream()).collect(Collectors.toList());
        }

        List<LLAtom> emptyList = Collections.emptyList();
        return emptyList;
    }


    public boolean isInstance(LLAtom var){

        Set keys = boundVariables.keySet();
        Iterator it = keys.iterator();

        while (it.hasNext()){
            Object o = it.next();
            if (boundVariables.get(o).contains(var))
            {
                return true;
            }
        }
        return false;
    }


    @Override
    public List<Equality> checkCompatibility(LLTerm term) {


        List<Equality> emptyList = Collections.emptyList();

        return emptyList;

    }
}
