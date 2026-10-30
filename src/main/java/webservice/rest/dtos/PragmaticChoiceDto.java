package webservice.rest.dtos;

/**
 * One alternative a PCDRS reading holds at one choice point (D1-D4 in xleplusglue's
 * docs/plans/PRAGMATIC_DISCRIMINANTS_PLAN.md). Keys are character spans of referents' heads
 * ("17-24", several joined by ","), an identity within one discourse update only (D2); labels
 * are display, never identity.
 */
public class PragmaticChoiceDto {
    /** ANAPHORA (a pronoun) or PRESUPPOSITION (a p-box): the anaphor decides (D3). */
    public String kind;
    /** The choice point: the pronoun's key, or the key of the p-box's bind referents. */
    public String point;
    public String pointLabel;
    /** ANTECEDENT or UNBOUND for a pronoun, BOUND or ACCOMMODATED for a p-box. */
    public String resolution;
    /** A referent's key, "/", or a site: GLOBAL, LOCAL, INTERMEDIATE:<key of the site box's
     *  referents>. A bound p-box with several bind referents: <bind referent's key>><key>. */
    public String target;
    public String targetLabel;
    /** The alternative's key: kind, point and target, unique within an update. */
    public String id;
    /** Whether the reading's own link names the target, rather than its coreference chain (D4). */
    public boolean direct;

    public PragmaticChoiceDto() {
    }

    public PragmaticChoiceDto(String kind, String point, String pointLabel, String resolution,
                              String target, String targetLabel, boolean direct) {
        this.kind = kind;
        this.point = point;
        this.pointLabel = pointLabel;
        this.resolution = resolution;
        this.target = target;
        this.targetLabel = targetLabel;
        this.direct = direct;
        this.id = kind + "@" + point + "=" + target;
    }
}
