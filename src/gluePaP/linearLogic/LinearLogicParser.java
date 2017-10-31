package gluePaP.linearLogic;

public class LinearLogicParser {
    public LinearLogicParser() {

    }

    public int parse(String str) {

        // Initialize new sequent
        Sequent input_seq = new Sequent();


        // Read input string characterwise
        for (int i = 0; i < str.length(); i++) {
            int c = (int) str.charAt(i);

            // character is a whitespace
            if (c == 32)
                continue;
            // character is a lower case letter
            else if(c >= 97 || c <= 122){

            }
            // character is an upper case letter
            else if (c >= 65 || c <= 90){

            }
            // character is a left parenthesis, set scope
            else if (c == 40) {

            }
            // character is a comma, delimits premises
            else if (c == 44) {

            }
            // character is an equal sign, might be first part of consequent separator (=>)
            else if (c == 61) {

            }
            // character is a minus, might be first part of linear implication
            else if (c == 45) {

            }
            else {
                // return exception?
            }



        }
    }
}
