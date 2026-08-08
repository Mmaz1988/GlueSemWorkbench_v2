package webservice.rest.dtos;

/** JSON-friendly mirror of LFGxDRT's AnaphoraRelation, for clients that need a structured
 *  pronoun-to-antecedent mapping instead of AnaphoraMapping.toString(). */
public class AnaphoraRelationDto {

    public String pronounReferentId;
    public String pronounDisplay;
    public String antecedent;
    public String stateLabel;

    public AnaphoraRelationDto() {
    }

    public AnaphoraRelationDto(String pronounReferentId, String pronounDisplay, String antecedent, String stateLabel) {
        this.pronounReferentId = pronounReferentId;
        this.pronounDisplay = pronounDisplay;
        this.antecedent = antecedent;
        this.stateLabel = stateLabel;
    }
}
