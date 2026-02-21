package Discriminants;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class McDiscriminantValues {
    String meaningConstructor;
    public HashSet<Integer> mcSetIds;
    public List<String> associatedSolutions = new ArrayList<>();


    public McDiscriminantValues(String meaningConstructor, HashSet<Integer> mcSetIds, Integer counter) {
        this.meaningConstructor = meaningConstructor;
        this.mcSetIds = mcSetIds;

    }

}
