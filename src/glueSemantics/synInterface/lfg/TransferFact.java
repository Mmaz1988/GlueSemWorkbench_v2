/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.synInterface.lfg;

import java.util.List;

public class TransferFact {
    private String functor;
    private List<String> arguments;
    private boolean consumability;

    public TransferFact(String functor) {
        this.functor = functor;
    }

    public TransferFact(String functor, List<String> arguments) {
        this.functor = functor;
        this.arguments = arguments;
    }

    public String getFunctor() {
        return functor;
    }

    public void setFunctor(String functor) {
        this.functor = functor;
    }

    public List<String> getArguments() {
        return arguments;
    }

    public void setArguments(List<String> arguments) {
        this.arguments = arguments;
    }

    public boolean isConsumable() {
        return consumability;
    }

    public void setConsumability(boolean consumability) {
        this.consumability = consumability;
    }
}
