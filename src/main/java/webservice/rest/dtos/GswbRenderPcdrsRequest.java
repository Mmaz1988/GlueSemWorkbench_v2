package webservice.rest.dtos;

import java.util.List;

/** PCDRS branches to render to SVG again (see /render_pcdrs). */
public class GswbRenderPcdrsRequest {
    public List<Item> items;

    public GswbRenderPcdrsRequest() {
    }

    public static class Item {
        /** Echoed back on the rendered solution, e.g. the discourse branch id. */
        public String id;
        /** The DRS the branch was generated from -- for an exact match with /generate_pcdrs,
         *  the semantic string it was sent (the semantic origin's `semString`). */
        public String semantic;
        /** The branch's mapping, applied to the DRS. */
        public List<AnaphoraRelationDto> anaphoraRelations;

        public Item() {
        }
    }
}
