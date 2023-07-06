package webservice.rest.dtos;

import java.util.List;

public class GswbOutput {
    public List<String> solutions;
    public String log;
    public Object derivation;

    public GswbOutput(List<String> solutions, String log, Object derivation)
    {
        this.solutions = solutions;
        this.log = log;
        this.derivation = derivation;
    }
}
