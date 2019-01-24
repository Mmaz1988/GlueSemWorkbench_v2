package test;

public class Debugging {
    /**
     * This class is designed to track performance.
     */

    public Long computationTime;
    public Integer allIterations = 0;
    public Integer combinations = 0;
    public Integer compilations = 0;


    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();

        sb.append("The following data was collected:");
        sb.append(System.lineSeparator());
        sb.append("computationTime: " + computationTime / 1000000 + "ms" );
        sb.append(System.lineSeparator());
        sb.append("Number of iterations through Sequent: " + allIterations);
        sb.append(System.lineSeparator());
        sb.append("Number of combination steps: " + combinations);
        sb.append(System.lineSeparator());
        sb.append("Number of proper compilation steps:" + compilations);
        sb.append(System.lineSeparator());


        return sb.toString();
    }

    public Debugging()
    {}





}
