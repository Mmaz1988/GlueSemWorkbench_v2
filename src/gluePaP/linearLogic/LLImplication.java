package gluePaP.linearLogic;

import Prover.Equality;

import java.util.List;

public class LLImplication extends LLTerm implements LLOperator {
    private final String symbol = "\u22B8";


    public String toString() {
        return symbol;
    }

    @Override
    public boolean checkEquivalence(LLTerm term) {
        if (term instanceof LLImplication)
            return true;
        return false;
    }

    @Override
    public List<Equality> checkCompatibility(LLTerm term) {
        return null;
    }
}
