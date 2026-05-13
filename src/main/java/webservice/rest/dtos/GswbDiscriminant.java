package webservice.rest.dtos;

import java.util.HashSet;
import java.util.LinkedHashSet;

public class GswbDiscriminant {
    public String id;
    public String type;
    public String identifier;
    public HashSet<String> associatedSolutions;
    public LinkedHashSet<String> instantiations;

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
}
