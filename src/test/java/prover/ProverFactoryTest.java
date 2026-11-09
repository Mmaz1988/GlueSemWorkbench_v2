package prover;

import main.Settings;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * GSWB has two provers, Hepple and Lev; staged input is proved by Lev since the multistage merge
 * (2026-11-07). Before {@link LLProver#forSettings}, the CLI ran Hepple for any other index without
 * a word ({@code xlerc}'s {@code prover 3} got Hepple, which ignores stages) and the web service
 * built no prover at all.
 */
class ProverFactoryTest {

    private static LLProver prover(int type) {
        Settings settings = new Settings();
        settings.setProverType(type);
        return LLProver.forSettings(settings, new StringBuilder());
    }

    @Test
    void heppleAndLev() {
        assertInstanceOf(LLProver2.class, prover(Settings.HEPPLE));
        assertInstanceOf(LLProver1.class, prover(Settings.LEV));
    }

    @Test
    void theFormerMultistageIndexRunsLevUntilTheClientDropsIt() {
        assertInstanceOf(LLProver1.class, prover(Settings.FORMER_MULTISTAGE));
    }

    @Test
    void anyOtherIndexIsRefused() {
        assertThrows(IllegalArgumentException.class, () -> prover(3));
        assertThrows(IllegalArgumentException.class, () -> prover(-1));
    }
}
