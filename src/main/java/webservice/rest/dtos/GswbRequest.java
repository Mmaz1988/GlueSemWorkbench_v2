package webservice.rest.dtos;

import java.util.LinkedHashMap;

public class GswbRequest {

    public String premises;
    public GswbPreferences gswbPreferences;
    public String sessionKey;
    public LinkedHashMap<String, Object> structure;

    public GswbRequest() {}

    public GswbRequest(String premises, GswbPreferences gswbprefs)
    {
        this.premises = premises;
        this. gswbPreferences = gswbprefs;
    }

    public GswbRequest(String premises, GswbPreferences gswbprefs, String sessionKey)
    {
        this.premises = premises;
        this.gswbPreferences = gswbprefs;
        this.sessionKey = sessionKey;
    }
}
