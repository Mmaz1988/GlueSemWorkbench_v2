package test;

import glueSemantics.parser.GlueParser;
import glueSemantics.parser.LexicalEntries;
import main.Settings;
import org.junit.jupiter.api.Test;
import prover.LLProver1;
import prover.SolutionObject;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

// An insitu element violates insitu by its own scope, not by travelling in a host's restrictor.
// "Every man saw a dog that liked Kim." with "Kim" scoping in the relative clause (the lfgxdrt
// inference grammar's meaning constructors): "a dog that liked Kim" over "every man" is a
// strict reading, Kim outscopes nothing.
class InsituCarriedTest {

    private static final List<String> PREMISES = List.of(
            "{",
            "[1] (\\P.(\\Q.([],[([x],[])+P@x->Q@x]))) : ((f23_e -o f23_t) -o ((f23_e -o f25_t) -o f25_t))",
            "[2] (\\x.([],[man(x)])) : (f23_e -o f23_t)",
            "[3] (\\V.(\\x.(\\e.(V@e+([],[arg2(e,x)]))))) : ((g26_v -o g26_t) -o (f18_e -o (g26_v -o g26_t))) || noscope",
            "[4] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g26_v -o g26_t) -o (f23_e -o (g26_v -o g26_t))) || noscope",
            "[5] (\\V.([e],[])+V@e) : ((g26_v -o g26_t) -o f25_t)",
            "[6] (\\v.([],[see(v)])) : (g26_v -o g26_t)",
            "[7] (\\P.P) : (f25_t -o g27_t)",
            "[8] (\\P.(\\Q.([x],[])+P@x+Q@x)) : ((f18_e -o f18_t) -o ((f18_e -o f25_t) -o f25_t))",
            "[9] (\\x.([],[dog(x)])) : (f18_e -o f18_t)",
            "[10] (\\P.(\\Q.(\\x.(P@x+Q@x)))) : ((f18_e -o f18_t) -o ((f10_e -o g14_t) -o (f18_e -o f18_t))) || noscope",
            "[11] (\\V.(\\x.(\\e.(V@e+([],[arg2(e,x)]))))) : ((g13_v -o g13_t) -o (f8_e -o (g13_v -o g13_t))) || noscope",
            "[12] (\\V.(\\x.(\\e.(V@e+([],[arg1(e,x)]))))) : ((g13_v -o g13_t) -o (f10_e -o (g13_v -o g13_t))) || noscope",
            "[13] (\\V.([e],[])+V@e) : ((g13_v -o g13_t) -o f12_t)",
            "[14] (\\v.([],[like(v)])) : (g13_v -o g13_t)",
            "[15] (\\P.P) : (f12_t -o g14_t)",
            "[16] (\\x.([],[x='kim'])) : (f8_e -o f8_t)",
            "[17] (\\P.(\\Q.p(([x],[bind(x)])+P@x)+Q@x)) : ((f8_e -o f8_t) -o ((f8_e -o f12_t) -o f12_t)) || insitu",
            "}");

    @Test
    void aWideHostCarriesItsInsituElement() throws Exception {
        Settings settings = new Settings();
        LexicalEntries lexicalEntries = new GlueParser(settings).parseMeaningConstructorList(PREMISES);
        LLProver1 prover = new LLProver1(settings);
        List<SolutionObject> solutions = new ArrayList<>();
        for (Integer key : lexicalEntries.lexicalEntries.keySet()) {
            solutions.addAll(prover.searchProof(key, lexicalEntries, true));
        }
        List<String> readings = solutions.stream().map(solution -> solution.solution.getSemTerm().toString()).toList();
        // "every man" over "a dog", and "a dog" (with Kim) over "every man".
        assertEquals(2, solutions.size(), readings.toString());
        assertTrue(readings.stream().anyMatch(reading -> reading.indexOf("dog(") < reading.indexOf("man(")), readings.toString());
        solutions.forEach(solution -> assertEquals(0, solution.insituViolations, "a strict proof"));
    }
}
