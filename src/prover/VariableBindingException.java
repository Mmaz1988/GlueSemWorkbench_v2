package prover;

public class VariableBindingException extends Exception{
    public VariableBindingException() {
        super("BindingErroe: duplicate bindings detected! ");
    }

    public VariableBindingException(String message) {
        super(message);
    }
}
