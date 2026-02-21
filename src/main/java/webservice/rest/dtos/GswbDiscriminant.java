package webservice.rest.dtos;

import java.util.HashSet;

public class GswbDiscriminant {
    public String id;
    public String type;
    public String identifier;
    public HashSet<String> associatedSolutions;

    public GswbDiscriminant(String id, String type, String identifier,  HashSet<String> associatedSolutions) {
        this.id = id;
        this.type = type;
        this.identifier = identifier;
        this.associatedSolutions = associatedSolutions;
    }

}
