package webservice.rest.dtos;

public class GswbTptpBatchResult {
    public String tptp;
    public String semantic;

    public GswbTptpBatchResult() {
    }

    public GswbTptpBatchResult(String tptp, String semantic) {
        this.tptp = tptp;
        this.semantic = semantic;
    }
}
