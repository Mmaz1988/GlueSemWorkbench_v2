package webservice.rest.dtos;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
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
    /** Scope discriminants only: the first SYN-ID of the outscoping and of the outscoped element,
     *  null where an element has none. */
    public List<Integer> surfaceKey;
    /** Scope discriminants only: whether the outscoping element comes first in the sentence,
     *  i.e. the scoping follows the surface order. Null when that cannot be told. */
    public Boolean surfaceAligned;

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
