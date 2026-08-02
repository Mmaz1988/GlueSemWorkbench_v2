package Discriminants;

import java.util.HashSet;
import java.util.LinkedHashSet;

public class ScopeDiscriminant {

    public String discriminantID;
    public String scopeConstraint;
    public HashSet<String> solutionIds;
    public LinkedHashSet<String> instantiations;
    public String surfaceLabel;
   // public List<String> associatedSolutions = new ArrayList<>();

    public ScopeDiscriminant(String discriminantID, String scopeConstraint, HashSet<String> solutionIds, LinkedHashSet<String> instantiations) {
        this.discriminantID = discriminantID;
        this.scopeConstraint = scopeConstraint;
        this.solutionIds = solutionIds;
        this.instantiations = instantiations;
    }
}
