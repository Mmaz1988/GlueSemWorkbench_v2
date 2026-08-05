package webservice.rest.dtos;

import java.util.LinkedHashMap;

/** One ordered unresolved semantic reading and its exact proof/syntax origin. */
public class GswbSequencePart {
    public String id;
    public String sentenceId;
    public String solutionId;
    public String proofId;
    public String solutionKey;
    public String mcSetId;
    public String semantic;
    public LinkedHashMap<String, Object> graph;
    public LinkedHashMap<String, Object> syntax;
    public LinkedHashMap<String, Object> provenance = new LinkedHashMap<>();

    public GswbSequencePart() {
    }
}
