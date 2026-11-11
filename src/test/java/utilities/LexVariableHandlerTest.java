package utilities;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import utilities.LexVariableHandler.VariablePoolExhaustedException;
import utilities.LexVariableHandler.variableType;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

// The fresh-variable pool is one per thread (one per deduction on the server): two deductions at
// the same time neither hand out each other's names nor empty each other's pool, and a pool that
// runs out fails instead of naming a variable null (which lost readings without an error).
class LexVariableHandlerTest {

    @AfterEach
    void emptyThePool() {
        LexVariableHandler.resetVars();
    }

    @Test
    void namesAreFreshWithinADeductionAndStartOverAfterAReset() {
        LexVariableHandler.resetVars();
        assertEquals("x", LexVariableHandler.returnNewVar(variableType.SemVarE));
        assertEquals("y", LexVariableHandler.returnNewVar(variableType.SemVarE));
        LexVariableHandler.resetVars();
        assertEquals("x", LexVariableHandler.returnNewVar(variableType.SemVarE));
    }

    @Test
    void aPoolThatRunsOutFailsInsteadOfReturningNull() {
        LexVariableHandler.resetVars();
        Set<String> names = new HashSet<>();
        // x, y, z and their indices 1..999
        for (int i = 0; i < 3000; i++) {
            names.add(LexVariableHandler.returnNewVar(variableType.SemVarE));
        }
        assertEquals(3000, names.size());
        assertThrows(VariablePoolExhaustedException.class,
                () -> LexVariableHandler.returnNewVar(variableType.SemVarE));
    }

    @Test
    void anotherThreadsResetLeavesThisPoolAlone() throws Exception {
        LexVariableHandler.resetVars();
        LexVariableHandler.returnNewVar(variableType.SemVarE);   // x, in this deduction
        CountDownLatch otherDone = new CountDownLatch(1);
        CompletableFuture<String> other = CompletableFuture.supplyAsync(() -> {
            LexVariableHandler.resetVars();
            String name = LexVariableHandler.returnNewVar(variableType.SemVarE);
            LexVariableHandler.resetVars();
            otherDone.countDown();
            return name;
        });
        otherDone.await();
        assertEquals("x", other.get(), "the other deduction has a pool of its own");
        assertEquals("y", LexVariableHandler.returnNewVar(variableType.SemVarE),
                "x is still taken here after the other deduction's reset");
    }
}
