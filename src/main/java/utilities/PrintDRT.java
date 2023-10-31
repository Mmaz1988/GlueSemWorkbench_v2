package utilities;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
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
        //create a file that includes all Strings in solutions line  by line
        //run swipl with the file as input

        LOGGER.fine("Creating temporary files...");
        //create temporary directory gswb_resources/tmp
        File tmpDir = new File("gswb_resources2/tmp");
        if (tmpDir.exists()) {
            //delete all files in tmpDir and tmpDir itself
            File[] files = tmpDir.listFiles();
            for (File file : files) {
                file.delete();
            }
            tmpDir.delete();
        }

        tmpDir.mkdir();

        LOGGER.fine("Created temporary directory: " + tmpDir.getAbsolutePath());

        File gswbFile = new File( tmpDir.getAbsolutePath() + "/gswbFile.txt");

        try {
            if (gswbFile.createNewFile()) {
                LOGGER.fine("File created successfully!");
            } else {
                LOGGER.warning("File already exists!");
            }
        } catch (
                IOException e) {
            LOGGER.warning("An error occurred while creating the file: " + e.getMessage());
            e.printStackTrace();
        }

        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(gswbFile));
            for (String solution : solutions) {
                writer.write(solution);
                writer.newLine();
            }
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        try {

            File drtOutputFile = new File(tmpDir.getAbsolutePath() + "/drtOutputFile.txt");
            LOGGER.info("Created DRT output file: " + drtOutputFile.getAbsolutePath());


            String[] command = {
                    "swipl",
                    "-q",
                    "-f",
                    "gswb_resources2/lambdaDRT.pl",
                    "-t",
                    "main.",
                    "--",
                    gswbFile.getAbsolutePath(),
                    drtOutputFile.getAbsolutePath()
            };

            ProcessBuilder processBuilder = new ProcessBuilder(command);

            // Java join command with white space


            LOGGER.info("Executing Prolog goal to pretty print DRT!");

            processBuilder.redirectErrorStream(true);

            Process process = processBuilder.start();

            StringBuilder prettyDRT = new StringBuilder();

            // Get the input stream to read the process output
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    prettyDRT.append(line);
                    prettyDRT.append(System.lineSeparator());
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

            LOGGER.fine("Pretty DRT: " + prettyDRT.toString());

            // Read and print the output of the external command

            // Wait for the process to complete
            // Create a separate thread to wait for the process to finish
            FutureTask<Integer> task = new FutureTask<>(process::waitFor);
            ExecutorService executor = Executors.newFixedThreadPool(1);
            executor.execute(task);

            // Wait for 5 seconds for the process to finish
            try {
                int exitCode = task.get(5, TimeUnit.SECONDS);
                if (exitCode != 0) {
                    LOGGER.warning("\nFailed to read output from lambdaDRT.pl!\n");
                }
            } catch (Exception e) {
                // If the process takes more than 5 seconds, destroy it
                process.destroyForcibly();
                LOGGER.warning("\nProcess timed out and was forcibly terminated.\n");
            }

            executor.shutdown();

            //Kill process if it takes longer than 10 seconds


            //create a new arraylist with prettyDRT as content

            //delete temporary files
            gswbFile.delete();
            drtOutputFile.delete();
            tmpDir.delete();


            drtSolutions.add(prettyDRT.toString());
            return drtSolutions;
        } catch (IOException e) {
            LOGGER.warning("Failed to pretty print DRT structures");
            throw new RuntimeException(e);
        }
    }

}
