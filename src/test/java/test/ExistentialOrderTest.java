package test;

import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import main.Settings;
import org.junit.jupiter.api.Test;
import prover.LLProver1;
import prover.SolutionObject;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

// Existentials that take scope directly over one another keep the surface order: their relative
// scope changes only the order of their states, which decides accessibility in a PCDRS (I4).
// Another scoping element between them is genuine scope. The meaning constructors are the
// lfgxdrt inference grammar's for "A man gave a dog a cat.", "A man gave every dog a cat." and
// "Every man that liked a dog saw a cat." (the parse with "a dog" scoping in the relative clause).
class ExistentialOrderTest {

    private static List<String> premises(String dogDeterminer) {
        return List.of(
                "{",
                "[1] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f15_e -o f15_t) -o ((f15_e -o f17_t) -o f17_t))",
                "[2] (\\x.([],[man(x)])) : (f15_e -o f15_t)",
                "[3] (\\V.(\\x.(\\e.(V@e+([],[arg3(e,x)]))))) : ((g18_v -o g18_t) -o (f10_e -o (g18_v -o g18_t))) || noscope",
                "[4] (\\V.(\\x.(\\e.(V@e+([],[arg2(e,x)]))))) : ((g18_v -o g18_t) -o (f5_e -o (g18_v -o g18_t))) || noscope",
                "[5] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g18_v -o g18_t) -o (f15_e -o (g18_v -o g18_t))) || noscope",
                "[6] (\\V.([e],[])+V@e) : ((g18_v -o g18_t) -o f17_t)",
                "[7] (\\v.([],[give(v)])) : (g18_v -o g18_t)",
                "[8] (\\P.P) : (f17_t -o g19_t)",
                "[9] " + dogDeterminer + " : ((f5_e -o f5_t) -o ((f5_e -o f17_t) -o f17_t))",
                "[10] (\\x.([],[dog(x)])) : (f5_e -o f5_t)",
                "[11] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f10_e -o f10_t) -o ((f10_e -o f17_t) -o f17_t))",
                "[12] (\\x.([],[cat(x)])) : (f10_e -o f10_t)",
                "}");
    }

    private static final List<String> RELATIVE_CLAUSE = List.of(
            "{",
            "[1] (\\P.(\\Q.([],[([x],[])+P@x->Q@x]))) : ((f23_e -o f23_t) -o ((f23_e -o f25_t) -o f25_t))",
            "[2] (\\x.([],[man(x)])) : (f23_e -o f23_t)",
            "[3] (\\P.(\\Q.(\\x.(P@x+Q@x)))) : ((f23_e -o f23_t) -o ((f15_e -o g19_t) -o (f23_e -o f23_t))) || noscope",
            "[4] (\\V.(\\x.(\\e.(V@e+([],[arg2(e,x)]))))) : ((g18_v -o g18_t) -o (f13_e -o (g18_v -o g18_t))) || noscope",
            "[5] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g18_v -o g18_t) -o (f15_e -o (g18_v -o g18_t))) || noscope",
            "[6] (\\V.([e],[])+V@e) : ((g18_v -o g18_t) -o f17_t)",
            "[7] (\\v.([],[like(v)])) : (g18_v -o g18_t)",
            "[8] (\\P.P) : (f17_t -o g19_t)",
            "[9] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f13_e -o f13_t) -o ((f13_e -o f17_t) -o f17_t))",
            "[10] (\\x.([],[dog(x)])) : (f13_e -o f13_t)",
            "[11] (\\V.(\\x.(\\e.(V@e+([],[arg2(e,x)]))))) : ((g26_v -o g26_t) -o (f5_e -o (g26_v -o g26_t))) || noscope",
            "[12] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g26_v -o g26_t) -o (f23_e -o (g26_v -o g26_t))) || noscope",
            "[13] (\\V.([e],[])+V@e) : ((g26_v -o g26_t) -o f25_t)",
            "[14] (\\v.([],[see(v)])) : (g26_v -o g26_t)",
            "[15] (\\P.P) : (f25_t -o g27_t)",
            "[16] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f5_e -o f5_t) -o ((f5_e -o f25_t) -o f25_t))",
            "[17] (\\x.([],[cat(x)])) : (f5_e -o f5_t)",
            "}");

    private static List<SolutionObject> prove(List<String> premises) throws Exception {
        Settings settings = new Settings();
        LexicalEntries lexicalEntries = new GlueParser(settings).parseMeaningConstructorList(premises);
        LLProver1 prover = new LLProver1(settings);
        List<SolutionObject> solutions = new ArrayList<>();
        for (Integer key : lexicalEntries.lexicalEntries.keySet()) {
            solutions.addAll(prover.searchProof(key, lexicalEntries, true));
        }
        return solutions;
    }

    /** The nouns in the order the reading prints them: outer scope first. */
    private static String order(SolutionObject solution) {
        String semantics = solution.solution.getSemTerm().toString();
        return Stream.of("man", "dog", "cat")
                .sorted(Comparator.comparingInt(noun -> semantics.indexOf(noun + "(")))
                .map(noun -> noun.substring(0, 1))
                .collect(Collectors.joining());
    }

    private static Set<String> orders(List<SolutionObject> solutions) {
        return solutions.stream().map(ExistentialOrderTest::order).collect(Collectors.toCollection(TreeSet::new));
    }

    private static String readings(List<SolutionObject> solutions) {
        return solutions.stream().map(solution -> solution.solution.getSemTerm().toString()).toList().toString();
    }

    @Test
    void existentialsInARowKeepTheSurfaceOrder() throws Exception {
        // Six orders without the filter; 1 over 3 over 2 fails at 3 over 2, and so on.
        List<SolutionObject> solutions = prove(premises("(\\P.(\\Q.([x],[])+P@x+Q@x))"));
        assertEquals(Set.of("mdc"), orders(solutions), readings(solutions));
        assertEquals(1, solutions.size());
    }

    @Test
    void aQuantifierBetweenExistentialsIsGenuineScope() throws Exception {
        // "every dog" between them: "a cat" over "every dog" over "a man" stays. Out are "a cat"
        // directly over "a man", above or below "every dog".
        List<SolutionObject> solutions = prove(premises("(\\P.(\\Q.([],[([x],[])+P@x->Q@x])))"));
        assertEquals(Set.of("mdc", "mcd", "dmc", "cdm"), orders(solutions), readings(solutions));
        assertEquals(4, solutions.size());
    }

    @Test
    void anExistentialInARestrictorIsNotDirectlyUnderTheOneOverItsHost() throws Exception {
        // "a cat" over "every man that liked a dog": "every man" stands between "a cat" and "a dog".
        List<SolutionObject> solutions = prove(RELATIVE_CLAUSE);
        assertEquals(Set.of("mdc", "cmd"), orders(solutions), readings(solutions));
    }
}
