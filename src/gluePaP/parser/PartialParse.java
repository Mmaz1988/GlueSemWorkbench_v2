package gluePaP.parser;

import gluePaP.linearLogic.LLTerm;

public class PartialParse {

    private LLTerm partialParseResult;
    private Integer initialPosition;
    private Integer finalPosition;



    // Constructur
    public PartialParse(Integer initialPosition)
    {
        this.initialPosition = initialPosition;
    }


    public LLTerm getPartialParseResult() {
        return partialParseResult;
    }

    public void setPartialParseResult(LLTerm partialParseResult) {
        this.partialParseResult = partialParseResult;
    }

    public Integer getInitialPosition() {
        return initialPosition;
    }

    public void setInitialPosition(Integer initialPosition) {
        this.initialPosition = initialPosition;
    }

    public Integer getFinalPosition() {
        return finalPosition;
    }

    public void setFinalPosition(Integer finalPosition) {
        this.finalPosition = finalPosition;
    }
}
