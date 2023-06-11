package webservice.rest.dtos;

import java.util.List;

public class GswbSolution {

    public List<String> solutions;
    public String explanation;

    public GswbSolution(List<String> solutions, String explanation)
    {
        this.solutions = solutions;
        this.explanation = explanation;
    }
}
