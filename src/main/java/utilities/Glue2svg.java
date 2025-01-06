package utilities;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.UUID;

public class Glue2svg {
    public static void main(String[] args) {
        // Example usage
        if (args.length != 1) {
            System.out.println("Usage: java Glue2Svg <inputFile.tex>");
            return;
        }

        String texFilePath = args[0];

        try {
            String svgOutput = latexToSvg(texFilePath,true);
            System.out.println("SVG Output:");
            System.out.println(svgOutput);
        } catch (IOException | InterruptedException e) {
            System.err.println("Error while rendering TeX to SVG: " + e.getMessage());
            e.printStackTrace();
        }
    }


    public static String latexToSvg(String latexContent, boolean deleteIntermediateFiles) throws IOException, InterruptedException {
        // Ensure the temporary directory exists
        String tempDirPath = "gswb_resources/tmp/";
        File tempDir = new File(tempDirPath);
        if (!tempDir.exists()) {
            tempDir.mkdirs();
        }

        // Generate unique file names
        String uniqueId = UUID.randomUUID().toString();
        String texFilePath = tempDirPath + "temp_" + uniqueId + ".tex";
        String dviFilePath = tempDirPath + "temp_" + uniqueId + ".dvi";
        String svgFilePath = tempDirPath + "temp_" + uniqueId + ".svg";

        try {
            // Step 1: Generate the .tex file
            createTexFile(latexContent, texFilePath);

            // Step 2: Compile the .tex file to .dvi
            compileTexToDvi(texFilePath);

            // Step 3: Convert the .dvi file to SVG
            String svgOutput = convertDviToSvg(dviFilePath, svgFilePath);

            // Step 4: Clean up intermediate files if requested
            if (deleteIntermediateFiles) {
                deleteFile(texFilePath);
                deleteFile(dviFilePath);
                deleteFile(svgFilePath);
            }

            return svgOutput;
        } finally {
            // Ensure cleanup in case of failure
            if (deleteIntermediateFiles) {
                deleteFile(texFilePath);
                deleteFile(dviFilePath);
                deleteFile(svgFilePath);
            }
        }
    }

    private static void createTexFile(String latexContent, String texFilePath) throws IOException {
        String texTemplate =
                "\\documentclass[preview,border=2pt]{standalone}\n" +
                        "\\usepackage{amsmath}\n" +
                        "\\usepackage{stmaryrd}\n" +
                        "\\usepackage{amssymb}\n" +
                        "\\usepackage{amsfonts}\n" +
                        "\\usepackage{lmodern}\n" +
                        "\\usepackage[T1]{fontenc}\n" +
                        "\\begin{document}\n" +
                        "\\fontsize{18pt}{22pt}\\selectfont\n" +
                        "$\\boldsymbol{%s}$\n" +
                        "\\end{document}\n";
        String fullTexContent = String.format(texTemplate, latexContent);

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(texFilePath))) {
            writer.write(fullTexContent);
        }
    }

    private static void compileTexToDvi(String texFilePath) throws IOException, InterruptedException {
        // Extract directory and base filename
        File texFile = new File(texFilePath);
        String tempDirPath = texFile.getParent();
        String baseFileName = texFile.getName().replaceFirst("\\.tex$", "");

        // Build the LaTeX process
        ProcessBuilder processBuilder = new ProcessBuilder(
                "latex",
                "-output-format=dvi",
                "-output-directory=" + tempDirPath, // Redirect output to the tmp folder
                texFilePath
        );

        // Redirect error stream to capture output and errors
        processBuilder.redirectErrorStream(true);

        Process process = processBuilder.start();

        // Capture logs (optional)
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
        }

        int exitCode = process.waitFor();
        if (exitCode != 0) {
            throw new RuntimeException("LaTeX compilation failed with exit code: " + exitCode);
        }

        // Clean up unnecessary auxiliary files
        deleteFile(tempDirPath + "/" + baseFileName + ".aux");
        deleteFile(tempDirPath + "/" + baseFileName + ".log");
    }


    private static String convertDviToSvg(String dviFilePath, String svgFilePath) throws IOException, InterruptedException {
        String libGsPath = "/usr/local/bin/gs"; // Adjust this path if necessary
        ProcessBuilder processBuilder = new ProcessBuilder(
                "dvisvgm",
                "--libgs=" + libGsPath,
                dviFilePath,
                "-o", svgFilePath,
                "--no-fonts",
                "--exact",
                "--bbox=preview"
        );

        // Redirect error stream to capture output and errors
        processBuilder.redirectErrorStream(true);

        Process process = processBuilder.start();

        // Capture logs (optional)
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
        }

        int exitCode = process.waitFor();
        if (exitCode != 0) {
            throw new RuntimeException("dvisvgm command failed with exit code: " + exitCode);
        }

        // Read and return the SVG file content
        return readFileContent(svgFilePath);
    }

    private static String readFileContent(String filePath) throws IOException {
        StringBuilder content = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line).append("\n");
            }
        }
        return content.toString();
    }

    private static void deleteFile(String filePath) {
        try {
            Files.deleteIfExists(Paths.get(filePath));
        } catch (IOException e) {
            System.err.println("Failed to delete file: " + filePath);
        }
    }
}
