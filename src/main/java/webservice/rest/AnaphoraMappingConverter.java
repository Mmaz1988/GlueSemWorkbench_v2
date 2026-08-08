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
            result.addRelation(new AnaphoraRelation(
                    new DiscourseReferent(dto.pronounReferentId),
                    dto.antecedent,
                    dto.stateLabel));
        }
        return result;
    }
}
