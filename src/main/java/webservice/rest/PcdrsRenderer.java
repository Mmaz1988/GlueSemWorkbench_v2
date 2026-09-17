package webservice.rest;

import de.ukon.lfgxdrt.DrsParser;
import de.ukon.lfgxdrt.DrsSvgRenderer;
import de.ukon.lfgxdrt.SemanticExpression;
import de.ukon.lfgxdrt.drs_elements.DRS;
import webservice.rest.dtos.AnaphoraRelationDto;

import java.util.List;

/** Renders a PCDRS branch again -- a DRS plus its anaphora mapping -- so a client that stores
 *  only the mapping need not keep the SVG /generate_pcdrs rendered for it.
 *
 *  To reproduce /generate_pcdrs exactly, pass the semantic string it was given (for a sequence,
 *  the unresolved `(...) + (...)`, i.e. the semantic origin's semString) with the branch's
 *  relations. Rendering the branch's stored PCDRS string instead was not identical on stored
 *  documents (sentence dividers missing). */
public final class PcdrsRenderer {

    private PcdrsRenderer() {
    }

    /** `semantic` as a DRS (merges resolved), with `relations`, when given, applied as its
     *  mapping -- replacing one the string already carries, as /collapse_anaphora does. */
    public static DRS withMapping(String semantic, List<AnaphoraRelationDto> relations) throws Exception {
        if (semantic == null || semantic.isBlank()) {
            throw new IllegalArgumentException("A DRS semantic is required.");
        }
        SemanticExpression expression = new DrsParser().parse(semantic).expression;
        if (!(expression instanceof DRS)) {
            expression = expression.resolveMerges();
        }
        if (!(expression instanceof DRS drs)) {
            throw new IllegalArgumentException("Rendering a PCDRS requires a DRS semantic expression.");
        }
        return relations == null || relations.isEmpty()
                ? drs
                : drs.withAnaphoraMapping(AnaphoraMappingConverter.fromDto(relations));
    }

    public static String toSvg(String semantic, List<AnaphoraRelationDto> relations) throws Exception {
        return new DrsSvgRenderer().toSvg(withMapping(semantic, relations));
    }
}
