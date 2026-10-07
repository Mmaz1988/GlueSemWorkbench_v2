package webservice.rest.dtos;

/**
 * One accommodation of a PCDRS branch, anchored on referents: the presupposition introducing
 * {@code presupposition} goes into the box introducing {@code site} ({@code s0}: the outermost
 * box). The branch shows the same relation between states (GswbSolution.presuppositionMapping);
 * this form is what the prior and the reasoning checks are collapsed against, since wrapping a
 * sentence into a check shifts its state labels but not its referents.
 */
public class PresuppositionRelationDto {
    public String presupposition;
    public String site;

    public PresuppositionRelationDto() {
    }

    public PresuppositionRelationDto(String presupposition, String site) {
        this.presupposition = presupposition;
        this.site = site;
    }
}
