package utilities;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

public class PrintDRT {

    private final static Logger LOGGER = Logger.getLogger(PrintDRT.class.getName());

    static {
        LOGGER.setUseParentHandlers(false);
        StreamHandler handler = new StreamHandler(System.out, new MyFormatter());
        //   handler.setFormatter(new MyFormatter());
        handler.setLevel(Level.FINE);
        LOGGER.addHandler(handler);

        LOGGER.setLevel(Level.ALL);
    }



    public static List<String> printDRT(List<String> solutions) {
        LOGGER.info("Pretty printing DRT structures ...");
        List<String> drtSolutions = new ArrayList<>();

        File tmpDir = new File("gswb_resources/tmp");

        // Clean up or recreate temporary directory
        if (tmpDir.exists()) {
            File[] files = tmpDir.listFiles();
            if (files != null) {
                for (File file : files) file.delete();
            }
            tmpDir.delete();
        }
        tmpDir.mkdir();
        LOGGER.fine("Created temporary directory: " + tmpDir.getAbsolutePath());

        File gswbFile = new File(tmpDir, "gswbFile.txt");
        File drtOutputFile = new File(tmpDir, "drtOutputFile.txt");

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(gswbFile))) {
            for (String solution : solutions) {
                writer.write(solution);
                writer.newLine();
            }
        } catch (IOException e) {
            LOGGER.warning("Failed to write DRT input file: " + e.getMessage());
            throw new RuntimeException(e);
        }

        //Create output file
        try {
            if (drtOutputFile.createNewFile()) {
                LOGGER.fine("DRT output file created successfully: " + drtOutputFile.getAbsolutePath());
            } else {
                LOGGER.warning("DRT output file already exists: " + drtOutputFile.getAbsolutePath());
            }
        } catch (IOException e) {
            LOGGER.warning("An error occurred while creating the DRT output file: " + e.getMessage());
            throw new RuntimeException(e);
        }

        String[] command = {
                "swipl",
                "-q",
                "-f", "gswb_resources/lambdaDRT.pl",
                "-t", "main.",
                "--",
                gswbFile.getAbsolutePath(),
                drtOutputFile.getAbsolutePath()
        };

        ProcessBuilder processBuilder = new ProcessBuilder(command);
        processBuilder.redirectErrorStream(false);

        StringBuilder prettyDRT = new StringBuilder();
        StringBuilder errorOutput = new StringBuilder();

        try {
            LOGGER.info("Executing Prolog to pretty print DRT...");
            Process process = processBuilder.start();

            Thread stdoutThread = new Thread(() -> {
                try (BufferedReader reader = new BufferedReader(
                        new InputStreamReader(process.getInputStream()))) {
                    String line;
                    while ((line = reader.readLine()) != null) {
                        prettyDRT.append(line).append(System.lineSeparator());
                    }
                } catch (IOException e) {
                    if (process.isAlive()) {
                        LOGGER.warning("Error reading stdout: " + e.getMessage());
                    } else {
                        LOGGER.fine("stdout stream closed after process termination.");
                    }
                }
            });

            Thread stderrThread = new Thread(() -> {
                try (BufferedReader reader = new BufferedReader(
                        new InputStreamReader(process.getErrorStream()))) {
                    String line;
                    while ((line = reader.readLine()) != null) {
                        errorOutput.append(line).append(System.lineSeparator());
                    }
                } catch (IOException e) {
                    if (process.isAlive()) {
                        LOGGER.warning("Error reading stderr: " + e.getMessage());
                    } else {
                        LOGGER.fine("stderr stream closed after process termination.");
                    }
                }
            });

            stdoutThread.start();
            stderrThread.start();

            ExecutorService executor = Executors.newSingleThreadExecutor();
            Future<Integer> task = executor.submit(() -> process.waitFor());

            try {
                int exitCode = task.get(5, TimeUnit.SECONDS); // Wait for Prolog to finish

                stdoutThread.join();
                stderrThread.join();

                if (exitCode != 0) {
                    LOGGER.warning("Prolog exited with code " + exitCode);
                    LOGGER.warning("stderr:\n" + errorOutput);
                    throw new RuntimeException("Prolog failed:\n" + errorOutput);
                }

            } catch (TimeoutException e) {
                LOGGER.warning("Prolog process timed out. Killing it...");
                process.destroyForcibly();
                process.waitFor(3, TimeUnit.SECONDS); // Give it time to terminate
            } catch (InterruptedException | ExecutionException e) {
                Thread.currentThread().interrupt();
                process.destroyForcibly();
                throw new RuntimeException("Process execution failed", e);
            } finally {
                executor.shutdownNow();
                try {
                    stdoutThread.join();
                    stderrThread.join();
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    LOGGER.warning("Interrupted while joining output threads.");
                }
            }


            if (!prettyDRT.toString().trim().isEmpty()) {
                LOGGER.fine("Using prettyDRT from stdout.");
                drtSolutions.add(prettyDRT.toString());
            } else {
                LOGGER.warning("prettyDRT is empty — attempting to read from output file.");
                if (drtOutputFile.exists() && drtOutputFile.length() > 0) {
                    try (BufferedReader reader = new BufferedReader(new FileReader(drtOutputFile))) {
                        String line;
                        StringBuilder sb = new StringBuilder();
                        while ((line = reader.readLine()) != null) {
                            sb.append(line).append(System.lineSeparator());
                        }
                        drtSolutions.add(sb.toString());
                        LOGGER.fine("Successfully read from output file.");
                    } catch (IOException e) {
                        LOGGER.warning("Failed to read fallback DRT file: " + e.getMessage());
                    }
                } else {
                    LOGGER.warning("DRT output file does not exist or is empty.");
                }
            }

            return drtSolutions;

        } catch (IOException | InterruptedException e) {
            LOGGER.warning("Failed to pretty print DRT structures");
            throw new RuntimeException(e);
        } finally {
            // Clean up temp files
            gswbFile.delete();
            drtOutputFile.delete();
            tmpDir.delete();
        }
    }


}
