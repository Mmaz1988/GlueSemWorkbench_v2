package utilities;

public class Debugging {
    /**
     * This class is designed to track performance.
     */

    public Long computationTime;
    public Integer allIterations = 0;
    public Integer combinations = 0;
    public Integer compilations = 0;
    public Integer discardedHistories = 0;

    public Integer noScopedHistories = 0;
    @Override
    public String toString() {

        StringBuilder sb = new StringBuilder();

        sb.append("The following data was collected:");
        sb.append(System.lineSeparator());
        sb.append("computationTime: " + computationTime / 1000000 + "ms" );
        sb.append(System.lineSeparator());
        if (allIterations > 0) {
            sb.append("Number of iterations through Sequent: " + allIterations);
            sb.append(System.lineSeparator());
        }
        sb.append("Number of combination steps: " + combinations);
        sb.append(System.lineSeparator());
        sb.append("Number of proper compilation steps: " + compilations);
        sb.append(System.lineSeparator());

        if (discardedHistories > 0)
        {
            sb.append("Number of histories discared during derivation: " + discardedHistories);
            sb.append(System.lineSeparator());
            sb.append("Histories deleted based on noScope flag: " + noScopedHistories);
            sb.append(System.lineSeparator());
        }


        return sb.toString();
    }

    public Debugging()
    {}





}
