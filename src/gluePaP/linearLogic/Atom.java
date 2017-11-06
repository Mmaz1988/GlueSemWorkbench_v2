package gluePaP.linearLogic;

public interface Atom {

    enum Type {
        E, T, COMPLEX
    }

    Type getType();

}
