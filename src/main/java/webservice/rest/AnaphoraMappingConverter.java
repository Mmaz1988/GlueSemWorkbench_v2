package webservice.rest;

import de.ukon.lfgxdrt.drs_elements.AnaphoraMapping;
import de.ukon.lfgxdrt.drs_elements.AnaphoraRelation;
import de.ukon.lfgxdrt.drs_elements.DiscourseReferent;
import webservice.rest.dtos.AnaphoraRelationDto;

import java.util.ArrayList;
import java.util.List;

/** Converts between LFGxDRT's AnaphoraMapping/AnaphoraRelation and the JSON-friendly DTO. */
final class AnaphoraMappingConverter {

    private AnaphoraMappingConverter() {
    }

    static List<AnaphoraRelationDto> toDto(AnaphoraMapping mapping) {
        List<AnaphoraRelationDto> result = new ArrayList<>();
        if (mapping == null || mapping.relations == null) {
            return result;
        }
        for (AnaphoraRelation relation : mapping.relations) {
            if (relation == null || relation.pronoun == null || relation.antecedent == null) {
                continue;
            }
            DiscourseReferent pronoun = relation.pronoun;
            String referentId = pronoun.id != null ? pronoun.id : pronoun.name;
            result.add(new AnaphoraRelationDto(
                    referentId,
                    pronoun.toSimpleString(),
                    relation.antecedent,
                    relation.stateLabel));
        }
        return result;
    }

    static AnaphoraMapping fromDto(List<AnaphoraRelationDto> dtos) {
        AnaphoraMapping result = new AnaphoraMapping();
        if (dtos == null) {
            return result;
        }
        for (AnaphoraRelationDto dto : dtos) {
            if (dto == null || dto.pronounReferentId == null || dto.antecedent == null) {
                continue;
            }
            // The pronoun this relation is reapplied to is always a *freshly re-parsed* referent
            // (semantic is re-parsed from scratch server-side, see collapseAnaphora() below) --
            // its .name/.id are whatever DrsParser/DrsGraphParser derive from the text, which only
            // ever matches pronoun.toSimpleString() (the plain display form, e.g. "x3"), not the
            // original in-memory pronounReferentId (which can be a richer internal id/name that no
            // longer exists once the DRS has been serialized and reparsed). Use pronounDisplay as
            // the name so DRS.collapseAnaphoraUnchecked()'s .name/.toSimpleString() lookups match;
            // keep pronounReferentId as .id too, for any caller still working with the original,
            // non-round-tripped in-memory object.
            DiscourseReferent pronoun = new DiscourseReferent(
                    dto.pronounDisplay != null ? dto.pronounDisplay : dto.pronounReferentId);
            pronoun.id = dto.pronounReferentId;
            result.addRelation(new AnaphoraRelation(pronoun, dto.antecedent, dto.stateLabel));
        }
        return result;
    }
}
