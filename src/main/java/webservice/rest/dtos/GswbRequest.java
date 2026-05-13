package webservice.rest.dtos;

public class GswbRequest {

    public String premises;
    public GswbPreferences gswbPreferences;
    public String sessionKey;

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
