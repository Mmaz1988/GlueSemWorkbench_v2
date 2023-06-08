package webservice.rest.dtos;

public class GswbRequest {

    public String premises;
    public GswbPreferences gswbPreferences;

    public GswbRequest(String premises, GswbPreferences gswbprefs)
    {
        this.premises = premises;
    this. gswbPreferences = gswbprefs;
    }
}
