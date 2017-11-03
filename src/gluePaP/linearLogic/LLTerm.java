package gluePaP.linearLogic;

public abstract class LLTerm {
    private String termId;
    private boolean polarity;

    //Default constructor
    public LLTerm(){ }

    public String getTermId() {
        return termId;
    }

    void setTermId(String termId) { this.termId = termId;}

    public boolean isPolarity() { return polarity; }

    public void setPolarity(boolean pol) { this.polarity = pol; }

    public abstract boolean checkEquivalence(LLTerm term);


}
