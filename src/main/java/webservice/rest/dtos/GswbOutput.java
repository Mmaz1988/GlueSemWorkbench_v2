package webservice.rest.dtos;

import java.util.List;

public class GswbOutput {
    public List<String> solutions;
    public String derivation;

    public GswbOutput(List<String> solutions, String derivation)
    {
        this.solutions = solutions;
        this.derivation = derivation;
    }
}
