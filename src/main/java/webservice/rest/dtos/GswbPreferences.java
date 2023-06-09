package webservice.rest.dtos;

public class GswbPreferences {
    public int prover;
    public boolean debugging;
    public int outputstyle;
    public boolean parseSem;
    public boolean noreduce;
    public boolean glueOnly;
    public boolean solutionOnly;
    public boolean meaningOnly;
    public boolean explainFail;


    public GswbPreferences(){
        this.prover = 1;
        this.outputstyle = 1;
        this.parseSem = false;
        this.noreduce = false;
        this.glueOnly = false;
        this.meaningOnly = false;
        this.solutionOnly = false;
        this.debugging = false;
        this.explainFail = false;

    }

    public GswbPreferences(int prover, int outputstyle, boolean explain, boolean parseSem, boolean debugging)
    {
        this.prover = prover;
        this.outputstyle = outputstyle;
        this.parseSem = parseSem;
        this.noreduce = false;
        this.glueOnly = false;
        this.meaningOnly = false;
        this.explainFail = explain;
        this.debugging = debugging;
      }

}
