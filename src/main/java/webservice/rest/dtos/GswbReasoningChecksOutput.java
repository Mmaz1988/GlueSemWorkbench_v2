package webservice.rest.dtos;

import java.util.LinkedHashMap;
import java.util.Map;

public class GswbReasoningChecksOutput {
    public Map<String, GswbReasoningCheck> checks = new LinkedHashMap<>();

    public GswbReasoningChecksOutput() {}

    public GswbReasoningChecksOutput(Map<String, GswbReasoningCheck> checks) {
        this.checks.putAll(checks);
    }
}
