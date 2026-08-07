package webservice.rest.dtos;

import java.util.Map;

public class GswbReasoningCheckAst {
    public String semantic;
    public Map<String, Object> ast;

    public GswbReasoningCheckAst() {}

    public GswbReasoningCheckAst(String semantic, Map<String, Object> ast) {
        this.semantic = semantic;
        this.ast = ast;
    }
}
