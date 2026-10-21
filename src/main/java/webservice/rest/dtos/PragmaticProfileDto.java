package webservice.rest.dtos;

/**
 * A PCDRS reading's pragmatic profile (N4 in xleplusglue's
 * docs/plans/PRESUPPOSITION_PRONOUN_PIPELINE_PLAN.md): how many of its choices fall in each
 * cell. Rows in priority order, columns best to worst; readings compare row by row, a row
 * column by column, more in an earlier column being better.
 */
public class PragmaticProfileDto {
    /** Pronouns: bound (in one complex condition with the antecedent: the rules' BOUND-ANT),
     *  referential (any other antecedent), unbound (a(x,/)). */
    public int[] pronouns = new int[3];
    /** Bound p-boxes by the depth of their site: global, intermediate, local. */
    public int[] binding = new int[3];
    /** Accommodated p-boxes by the depth of their site: global, intermediate, local. */
    public int[] accommodation = new int[3];
}
