package webservice.rest.dtos;

import java.util.ArrayList;
import java.util.List;

/**
 * What collapsing a PCDRS reading did (I7.6), as data: one resolution per p-box in the order
 * they were resolved, then the names the collapse's own numbering gave the referents. The
 * reading's {@code collapseReport} says the same as text.
 */
public class CollapseStepsDto {

    /** {@code from} became {@code to}: a bind referent and its binder, or a referent's name
     *  before and after the collapse. */
    public static class Renaming {
        public String from;
        public String to;

        public Renaming() {
        }

        public Renaming(String from, String to) {
            this.from = from;
            this.to = to;
        }
    }

    /** One p-box ({@code s3}), bound or accommodated, at a site ({@code s0}), at a depth
     *  (GLOBAL, INTERMEDIATE, LOCAL); a bound one with its bind referents' binders. */
    public static class Resolution {
        public String presupposition;
        public boolean bound;
        public String site;
        public String depth;
        public List<Renaming> bindings = new ArrayList<>();
    }

    public List<Resolution> resolutions = new ArrayList<>();
    public List<Renaming> renamed = new ArrayList<>();
}
