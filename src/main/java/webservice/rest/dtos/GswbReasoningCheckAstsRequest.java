package webservice.rest.dtos;

import java.util.LinkedHashMap;
import java.util.List;

public class GswbReasoningCheckAstsRequest {
    public List<LinkedHashMap<String, Object>> premiseAsts;
    public List<LinkedHashMap<String, Object>> hypothesisAsts;
    public boolean typed;

    public GswbReasoningCheckAstsRequest() {}
}
