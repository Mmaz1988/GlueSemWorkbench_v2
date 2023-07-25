package webservice.rest.dtos;

import glueSemantics.linearLogic.Premise;
import main.Settings;

import java.util.HashMap;
import java.util.List;
import java.util.SplittableRandom;

public class GswbBatchRequest {

    public HashMap<String, String> premises;
    public GswbPreferences gswbPreferences;

    public GswbBatchRequest(HashMap<String, String> premises, GswbPreferences settings) {
        this.premises = premises;
        this.gswbPreferences = settings;
    }
}
