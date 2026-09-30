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
    /** The sentence this proof's MCs belong to ({@code id}, {@code text}); inside a sequence,
     *  that sentence's entry of {@link #sequenceAnalysis}. Used for logging. */
    public LinkedHashMap<String, Object> sentenceAnalysis;
    /** The whole sequence the proof's sentence is part of, when it is one; null standalone. */
    public LinkedHashMap<String, Object> sequenceAnalysis;

    public GswbProofInput() {}
}
