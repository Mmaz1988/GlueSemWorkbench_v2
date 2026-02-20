package glueSemantics.parser;

import java.util.HashSet;

public class McDiscriminantValues {
    String meaningConstructor;
    public HashSet<Integer> mcSetIds;


    public McDiscriminantValues(String meaningConstructor, HashSet<Integer> mcSetIds, Integer counter) {
        this.meaningConstructor = meaningConstructor;
        this.mcSetIds = mcSetIds;

    }

}
