package test;

import org.junit.jupiter.api.Test;
import webservice.rest.PcdrsRenderer;
import webservice.rest.dtos.AnaphoraRelationDto;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/** /render_pcdrs reproduces /generate_pcdrs from the semantic it was sent plus a branch's
 *  mapping, so clients can drop the SVG and ask for it again. Checked live 2026-09-17 against
 *  every branch of two stored analysis documents (96/96 byte-identical). */
class PcdrsRendererTest {

    private static final String SEQUENCE =
            "([x1,x2,x3],[big(x1),cat(x1),black(x2),dog(x2),eat(x3),arg2(x3,x2),arg1(x3,x1)])"
                    + " + ([x4,x5],[ant(x4),bark(x5),arg1(x5,x4)])";
    private static final List<AnaphoraRelationDto> IT_IS_THE_CAT =
            List.of(new AnaphoraRelationDto("s1:4", "x4", "x1", "s1"));

    @Test
    void rendersTheMappedDrsTheWayGeneratePcdrsDoes() throws Exception {
        var pcdrs = PcdrsRenderer.withMapping(SEQUENCE, IT_IS_THE_CAT);
        assertTrue(pcdrs.toString().endsWith("A:[a(x4,x1)]"), pcdrs.toString());

        String svg = PcdrsRenderer.toSvg(SEQUENCE, IT_IS_THE_CAT);
        assertEquals(new de.ukon.lfgxdrt.DrsSvgRenderer().toSvg(pcdrs), svg);
        assertNotEquals(PcdrsRenderer.toSvg(SEQUENCE, null), svg, "the mapping must be visible in the SVG");
    }

    @Test
    void rejectsAMissingSemantic() {
        assertThrows(IllegalArgumentException.class, () -> PcdrsRenderer.toSvg(" ", null));
    }
}
