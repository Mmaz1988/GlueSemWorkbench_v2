package prover;

import glueSemantics.linearLogic.Premise;

import java.util.ArrayList;
import java.util.List;

public class SolutionObject {
    Premise solution;
    List<String> discriminants;


    public SolutionObject(Premise solution, List<String> discriminants) {
        this.solution = solution;
        this.discriminants = discriminants;
    }

}
