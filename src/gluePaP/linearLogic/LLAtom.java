package gluePaP.linearLogic;

public interface LLAtom {

    enum Type {
        E, T, COMPLEX
    }

    Type getType();

}
