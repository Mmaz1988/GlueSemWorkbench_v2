package utilities;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

/**
 * The command-line workbench prints its log to the console in {@link MyFormatter}'s format. In
 * {@code -web} mode that must not happen: a logger with its own console handler bypasses logback,
 * so it ignores GSWB_LOG_LEVEL, carries no request id, and its lines interleave mid-line with
 * Spring's. There the loggers keep their parents, i.e. logback.
 */
public final class ConsoleLogging {

    private static volatile boolean webMode;

    private ConsoleLogging() {}

    /** Set by WorkbenchMain before anything else logs. */
    public static void setWebMode(boolean web) {
        webMode = web;
    }

    /** Gives {@code logger} its own console handler -- in command-line mode only. */
    public static void attachConsole(Logger logger) {
        if (webMode) {
            return;
        }
        logger.setUseParentHandlers(false);
        StreamHandler handler = new StreamHandler(System.out, new MyFormatter());
        handler.setLevel(Level.FINE);
        logger.addHandler(handler);
        logger.setLevel(Level.ALL);
    }
}
