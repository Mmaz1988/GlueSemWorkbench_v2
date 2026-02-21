package Discriminants;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class McDiscriminant {
    public String discriminantID;
    public String meaningConstructor;
    public HashSet<Integer> mcSetIds;
    public HashSet<String> associatedSolutions = new HashSet<>();


    public McDiscriminant(String discriminantID, String meaningConstructor, HashSet<Integer> mcSetIds) {
        this.discriminantID = discriminantID;
        this.meaningConstructor = meaningConstructor;
        this.mcSetIds = mcSetIds;

    }

}
