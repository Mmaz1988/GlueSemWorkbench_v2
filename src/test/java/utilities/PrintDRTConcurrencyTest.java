package utilities;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

/**
 * Concurrent Prolog-DRT renderings each get their own files. All of them used to share
 * gswb_resources/tmp, which every call deleted and recreated, so one request could remove or
 * overwrite another's input mid-run (2026-11-09).
 */
class PrintDRTConcurrencyTest {

    private static final int CALLS = 16;

    @Test
    void concurrentRenderingsDoNotShareFiles() throws Exception {
        assumeTrue(swiplAvailable(), "swipl not on PATH");
        ExecutorService pool = Executors.newFixedThreadPool(CALLS);
        CountDownLatch start = new CountDownLatch(1);
        try {
            List<Future<String>> results = new ArrayList<>();
            for (int i = 0; i < CALLS; i++) {
                String symbol = "pred" + i;
                results.add(pool.submit(() -> {
                    start.await();
                    return String.join("", PrintDRT.printDRT(
                            List.of("solution(1,drs([X],[pred(" + symbol + ",X)])).",
                                    "solution(2,drs([X],[pred(" + symbol + "b,X)]))."), true));
                }));
            }
            start.countDown();
            for (int i = 0; i < CALLS; i++) {
                String out = results.get(i).get();
                assertTrue(out.contains("pred" + i + "(x1)") && out.contains("pred" + i + "b(x1)"),
                        "call " + i + " lost its own rendering: " + out);
                for (int j = 0; j < CALLS; j++) {
                    if (j != i) {
                        assertTrue(!out.contains("pred" + j + "("), "call " + i + " got call " + j + "'s DRS");
                    }
                }
            }
        } finally {
            pool.shutdownNow();
        }
    }

    private static boolean swiplAvailable() {
        try {
            return new ProcessBuilder("swipl", "--version").start().waitFor() == 0;
        } catch (Exception e) {
            return false;
        }
    }
}
