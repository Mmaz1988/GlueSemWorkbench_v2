/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package glueSemantics.semantics.lambda;

import glueSemantics.semantics.SemanticRepresentation;


public abstract class SemanticExpression implements SemanticRepresentation {
    private SemType type;


    public SemanticExpression() {
    }

    public SemType getType() {
        return type;
    }

    public void setType(SemType.AtomicType type) {
        this.type = new SemType(type);
    }

    public void setType(SemType type) {
        this.type = type;
    }

    //public abstract SemanticRepresentation betaReduce();

    //public abstract SemanticRepresentation applyTo(SemAtom var, SemanticExpression arg);

    // This is not a regular clone() method, it just calls the copy constructor
    // of the respective class.
    public abstract SemanticExpression clone();

    /**
     * This function checks whether the functor has a quantifier that scopes over the argument. It does not work for sets
     * or in the unlikely scenario that two conjuncts interact differently with the scope of the argument (.e.g Ex P(x) & Ay P(x) >> Ez....)
     * TODO SemPreds should be taken is scope taking operators
     * @param functor
     * @param argument
     * @return
     */
    public static boolean nonScopingQuantifiers(SemanticExpression functor, SemanticExpression argument)
    {
        SemanticRepresentation scopedExpression = null;
        SemAtom binder = null;
        if (functor instanceof SemFunction)
        {
            binder = ((SemFunction) functor).getBinder();
            scopedExpression = ((SemFunction) functor).getFuncBody();

            while (scopedExpression.containsQuantExpression() && scopedExpression.bindsVar(binder)) {
                if (scopedExpression instanceof SemQuantEx) {
                    break;
                }

                if (scopedExpression instanceof SemFunction) {
                    scopedExpression = ((SemFunction) scopedExpression).getFuncBody();
                }

                if (scopedExpression instanceof BinaryTerm) {
                    if (((BinaryTerm) scopedExpression).getLeft().containsQuantExpression() && ((BinaryTerm) scopedExpression).getLeft().bindsVar(binder)) {
                        scopedExpression = ((BinaryTerm) scopedExpression).getLeft();
                    } else if (((BinaryTerm) scopedExpression).getRight().containsQuantExpression() && ((BinaryTerm) scopedExpression).getRight().bindsVar(binder)) {
                        scopedExpression = ((BinaryTerm) scopedExpression).getRight();
                    }
                }

                if (scopedExpression instanceof FuncApp) {
                    if (((FuncApp) scopedExpression).getFunctor().containsQuantExpression() && ((FuncApp) scopedExpression).getFunctor().bindsVar(binder)) {
                        scopedExpression = ((FuncApp) scopedExpression).getFunctor();
                    } else if (((FuncApp) scopedExpression).getArgument().containsQuantExpression() && ((FuncApp) scopedExpression).getArgument().bindsVar(binder)) {
                        scopedExpression = ((FuncApp) scopedExpression).getArgument();
                    }
                }

                if (scopedExpression instanceof SemPred) {
                    for (SemanticRepresentation arg : ((SemPred) scopedExpression).getArgList()) {
                        if (arg.containsQuantExpression() && arg.bindsVar(binder)) {
                            scopedExpression = arg;
                        }
                    }
                }

                if (scopedExpression instanceof SemSet) {
                    for (SemanticRepresentation member : ((SemSet) scopedExpression).getMembers()) {
                        if (member.containsQuantExpression() && member.bindsVar(binder)) {
                            scopedExpression = member;
                        }
                    }
                }
            }

                if (scopedExpression instanceof SemQuantEx)
                {
                    SemanticRepresentation embeddedExpression = argument;

                    while (embeddedExpression.containsQuantExpression())
                    {
                        if (embeddedExpression instanceof SemQuantEx)
                        {
                            break;
                        }

                        if (embeddedExpression instanceof BinaryTerm)
                        {
                            if (((BinaryTerm) embeddedExpression).getLeft().containsQuantExpression())
                            {
                                embeddedExpression = ((BinaryTerm) embeddedExpression).getLeft();
                            } else
                            if (((BinaryTerm) embeddedExpression).getRight().containsQuantExpression())
                            {
                                embeddedExpression = ((BinaryTerm) embeddedExpression).getRight();
                            }
                        }

                        if (embeddedExpression instanceof FuncApp)
                        {
                            embeddedExpression = ((FuncApp) embeddedExpression).getFunctor();
                        }

                        if (embeddedExpression instanceof SemPred)
                        {
                            break;
                        }

                        if (embeddedExpression instanceof  SemSet)
                        {
                            for (SemanticRepresentation member : ((SemSet) embeddedExpression).getMembers())
                            {
                                if (embeddedExpression.containsQuantExpression())
                                {
                                    embeddedExpression = member;
                                }
                            }
                        }

                    }

                    if (embeddedExpression instanceof  SemQuantEx)
                    {
                        SemQuantEx.SemQuant funcType = ((SemQuantEx) scopedExpression).getQuantifier();
                        SemQuantEx.SemQuant argType = ((SemQuantEx) embeddedExpression).getQuantifier();

                        return funcType == SemQuantEx.SemQuant.EX && argType == SemQuantEx.SemQuant.EX;
                    }

                }
            }
        return false;
    }


    public abstract boolean bindsVar(SemAtom var);

    public abstract boolean containsQuantExpression();

}
