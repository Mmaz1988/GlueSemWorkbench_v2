package webservice.rest.dtos;

import java.util.LinkedHashMap;

public class GswbReasoningCheckAstsOutput {
    public LinkedHashMap<String, GswbReasoningCheckAst> checks;

    public GswbReasoningCheckAstsOutput() {}

    public GswbReasoningCheckAstsOutput(LinkedHashMap<String, GswbReasoningCheckAst> checks) {
        this.checks = checks;
    }
}
