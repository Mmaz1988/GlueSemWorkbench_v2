package webservice.rest.dtos;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class GswbDiscriminant {
    public String id;
    public String type;
    public String identifier;
    public HashSet<String> associatedSolutions;
    public LinkedHashSet<String> instantiations;
    public String surfaceLabel;
    public Set<String> originIds;
    public Map<String, String> surfaceLabelsByOrigin;

    public GswbDiscriminant() {}

    public GswbDiscriminant(String id, String type, String identifier,  HashSet<String> associatedSolutions) {
        this.id = id;
        this.type = type;
        this.identifier = identifier;
        this.associatedSolutions = associatedSolutions;
    }

    public GswbDiscriminant(String id, String type, String identifier,  HashSet<String> associatedSolutions, LinkedHashSet<String> instantiations) {
        this.id = id;
        this.type = type;
        this.identifier = identifier;
        this.associatedSolutions = associatedSolutions;
        this.instantiations = instantiations;
    }

    public GswbDiscriminant(String id, String type, String identifier, HashSet<String> associatedSolutions,
                            LinkedHashSet<String> instantiations, String surfaceLabel) {
        this(id, type, identifier, associatedSolutions, instantiations);
        this.surfaceLabel = surfaceLabel;
    }
}
