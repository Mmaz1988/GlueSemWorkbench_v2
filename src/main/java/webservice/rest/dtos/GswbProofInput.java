package webservice.rest.dtos;

import java.util.LinkedHashMap;

/** One syntactic origin and its MC input within an aggregate deduction. */
public class GswbProofInput {
    public String id;
    public String sentenceId;
    public String proofId;
    public String solutionKey;
    public String mcSetId;
    public String meaningConstructors;
    public LinkedHashMap<String, Object> structure;
    public LinkedHashMap<String, Object> provenance = new LinkedHashMap<>();

    public GswbProofInput() {}
}
