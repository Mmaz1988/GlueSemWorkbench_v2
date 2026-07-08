package webservice.rest.dtos;

public class GswbPreferences {
    public int prover;
    public boolean debugging;
    public int outputstyle;
    public boolean parseSem;
    public boolean betaReduce;
    public boolean resolveDrs;
    public boolean glueOnly;
    public boolean solutionOnly;
    public boolean meaningOnly;
    public boolean explainFail;

    public int naturalDeductionStyle;


    public GswbPreferences(){
        this.prover = 1;
        this.outputstyle = 1;
        this.parseSem = false;
        this.betaReduce = false;
        this.resolveDrs = false;
        this.glueOnly = false;
        this.meaningOnly = false;
        this.solutionOnly = false;
        this.debugging = false;
        this.explainFail = false;
        this.naturalDeductionStyle = 0;

    }

    public GswbPreferences(int prover, int outputstyle, boolean explain, boolean parseSem, boolean noreduce, boolean resolveDrs, boolean debugging, Integer naturalDeductionStyle)
    {
        this.prover = prover;
        this.outputstyle = outputstyle;
        this.parseSem = parseSem;
        this.betaReduce = noreduce;
        this.resolveDrs = resolveDrs;
        this.glueOnly = false;
        this.meaningOnly = false;
        this.explainFail = explain;
        this.debugging = debugging;
        this.naturalDeductionStyle = naturalDeductionStyle;
      }

}
