package glueSemantics.linearLogic;

import prover.Equality;

import java.util.ArrayList;
import java.util.LinkedHashSet;

public class LLQuantEx extends LLTerm {


    private LLAtom var;
    private LLTerm scope;

    public LLQuantEx(LLAtom var, LLTerm scope)
    {
        this.var = var;
        this.scope = scope;
        updateVariables(var);
        setType(scope.getType());
    }

    public LLQuantEx(LLQuantEx q)
    {
        this.var = (LLAtom) q.getVar().clone();
        this.scope = q.getScope().clone();
        setType(q.getType().clone());
    }

    public void updateVariables(LLAtom var) {
        if (scope instanceof LLQuantEx) {
            ((LLQuantEx) scope).updateVariables(var);
        } else
        {
            if (scope.getVariable() == null)
            {
                scope.setVariable(new ArrayList<>());
            }
            if (!scope.getVariable().contains(var))
            {
                scope.getVariable().add(var);
            }
        }
    }


    @Override
    public boolean checkEquivalence(LLTerm term) {
        return checkEquivalence(scope);
    }

    @Override
    public LinkedHashSet<Equality> checkCompatibility(LLTerm term) {
        return scope.checkCompatibility(scope);
    }

    @Override
    public String category() {
        return scope.category();
    }

    @Override
    public boolean isModifier() {
        return scope.isModifier();
    }

    @Override
    public LLTerm clone() {
        return new LLQuantEx(this);
    }

    @Override
    public String toString()
    {
        return "A" + var.toString() + "." + scope.toString();
    }


    public void updateBoundVariables(LLAtom var)
    {
        scope.updateBoundVariables(var);
    }



    public LLAtom getVar() {
        return var;
    }

    public void setVar(LLAtom var) {
        this.var = var;
    }

    public LLTerm getScope() {
        return scope;
    }

    public void setScope(LLTerm scope) {
        this.scope = scope;
    }
}
