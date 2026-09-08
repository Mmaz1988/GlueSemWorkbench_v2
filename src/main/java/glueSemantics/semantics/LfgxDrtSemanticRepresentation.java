package glueSemantics.semantics;

import de.ukon.lfgxdrt.DrsParser;
import de.ukon.lfgxdrt.SemanticExpression;
import glueSemantics.semantics.lambda.SemAtom;
import glueSemantics.semantics.lambda.SemType;
import prover.ProverException;

import java.util.HashSet;
import java.util.Set;

public class LfgxDrtSemanticRepresentation extends glueSemantics.semantics.lambda.SemanticExpression implements de.ukon.lfgxdrt.SemanticExpression {

    private final SemanticExpression delegate;

    public LfgxDrtSemanticRepresentation(SemanticExpression delegate) {
        this.delegate = delegate;
        setType(new SemType(SemType.AtomicType.T));
        setSourceIndex(delegate == null ? null : delegate.getSourceIndex());
    }

    public SemanticExpression getDelegate() {
        return delegate;
    }

    public static LfgxDrtSemanticRepresentation fromString(String input, Integer sourceIndex) throws Exception {
        SemanticExpression parsed = new DrsParser().parse(input).expression;
        attachSourceIndex(parsed, sourceIndex);
        LfgxDrtSemanticRepresentation wrapped = new LfgxDrtSemanticRepresentation(parsed);
        wrapped.setSourceIndex(sourceIndex);
        return wrapped;
    }

    public static LfgxDrtSemanticRepresentation fromGswb(glueSemantics.semantics.lambda.SemanticExpression expression, Integer sourceIndex) throws Exception {
        if (expression instanceof LfgxDrtSemanticRepresentation wrapped) {
            return wrapped;
        }
        SemanticExpression converted = convertExpression(expression, sourceIndex);
        LfgxDrtSemanticRepresentation wrapped = new LfgxDrtSemanticRepresentation(converted);
        wrapped.setSourceIndex(sourceIndex);
        return wrapped;
    }

    private static SemanticExpression convertExpression(glueSemantics.semantics.SemanticRepresentation expression, Integer fallbackSourceIndex) throws Exception {
        if (expression == null) {
            return null;
        }
        if (expression instanceof LfgxDrtSemanticRepresentation wrapped) {
            return wrapped.getDelegate();
        }
        if (expression instanceof glueSemantics.semantics.lambda.SemAtom atom) {
            de.ukon.lfgxdrt.lambda_elements.LambdaVariable variable = new de.ukon.lfgxdrt.lambda_elements.LambdaVariable(atom.getName(), convertType(atom.getType()));
            variable.setSourceIndex(atom.getSourceIndex() != null ? atom.getSourceIndex() : fallbackSourceIndex);
            return variable;
        }
        if (expression instanceof glueSemantics.semantics.lambda.SemFunction function) {
            de.ukon.lfgxdrt.lambda_elements.LambdaVariable binder = (de.ukon.lfgxdrt.lambda_elements.LambdaVariable) convertExpression(function.getBinder(), fallbackSourceIndex);
            SemanticExpression body = convertExpression(function.getFuncBody(), fallbackSourceIndex);
            de.ukon.lfgxdrt.lambda_elements.LambdaFunction converted = new de.ukon.lfgxdrt.lambda_elements.LambdaFunction(binder, body);
            converted.setSourceIndex(function.getSourceIndex() != null ? function.getSourceIndex() : fallbackSourceIndex);
            return converted;
        }
        if (expression instanceof glueSemantics.semantics.lambda.FuncApp funcApp) {
            SemanticExpression functionPart = convertExpression(funcApp.getFunctor(), fallbackSourceIndex);
            SemanticExpression argumentPart = convertExpression(funcApp.getArgument(), fallbackSourceIndex);
            de.ukon.lfgxdrt.lambda_elements.FuncApp converted = new de.ukon.lfgxdrt.lambda_elements.FuncApp(functionPart, argumentPart);
            converted.setSourceIndex(funcApp.getSourceIndex() != null ? funcApp.getSourceIndex() : fallbackSourceIndex);
            return converted;
        }

        LfgxDrtSemanticRepresentation parsed = fromString(expression.toString(), fallbackSourceIndex);
        return parsed.getDelegate();
    }

    private static de.ukon.lfgxdrt.lambda_elements.SemType convertType(glueSemantics.semantics.lambda.SemType type) {
        if (type == null) {
            return null;
        }
        if (type.getLeft() == null) {
            return new de.ukon.lfgxdrt.lambda_elements.SemType(type.toString());
        }
        return new de.ukon.lfgxdrt.lambda_elements.SemType(convertType(type.getLeft()), convertType(type.getRight()));
    }

    @Override
    public String toString() {
        return delegate.toString();
    }

    @Override
    public String toString(boolean includeStateLabels) {
        return delegate.toString(includeStateLabels);
    }

    @Override
    public String toSimpleString() {
        return delegate.toSimpleString();
    }

    @Override
    public LfgxDrtSemanticRepresentation resolveMerges() {
        SemanticExpression resolved = delegate.resolveMerges();
        return wrapResult(resolved);
    }

    @Override
    public LfgxDrtSemanticRepresentation betaReduce() {
        SemanticExpression reduced = delegate.betaReduce();
        return wrapResult(reduced);
    }

    @Override
    public LfgxDrtSemanticRepresentation substitute(de.ukon.lfgxdrt.lambda_elements.LambdaVariable var, SemanticExpression replacement) {
        SemanticExpression substituted = delegate.substitute(var, replacement);
        return wrapResult(substituted);
    }

    @Override
    public LfgxDrtSemanticRepresentation collapseAnaphora(java.util.Map<String, de.ukon.lfgxdrt.drs_elements.DiscourseReferent> referentEnv,
                                              java.util.Map<String, de.ukon.lfgxdrt.drs_elements.DiscourseReferent> anaphoraEnv) {
        SemanticExpression collapsed = delegate.collapseAnaphora(referentEnv, anaphoraEnv);
        return wrapResult(collapsed);
    }

    @Override
    public int allocateStateLabels(int nextState) {
        return delegate.allocateStateLabels(nextState);
    }

    @Override
    public int allocateDisplayNames(int nextIndex) {
        return delegate.allocateDisplayNames(nextIndex);
    }

    @Override
    public String toTPTPString(boolean typed, java.util.Map<String, String> env) {
        return delegate.toTPTPString(typed, env);
    }

    @Override
    public de.ukon.lfgxdrt.liger_graph.LigerGraph toGraph() {
        return delegate.toGraph();
    }

    @Override
    public java.util.LinkedHashMap<String, Object> toJson() {
        return delegate.toJson();
    }

    @Override
    public java.util.Set<String> freeVariables() {
        return delegate.freeVariables();
    }

    @Override
    public java.util.Set<String> boundVariables() {
        return delegate.boundVariables();
    }

    @Override
    public LfgxDrtSemanticRepresentation alphaRename(String oldName, String newName) {
        SemanticExpression renamed = delegate.alphaRename(oldName, newName);
        return wrapResult(renamed);
    }

    @Override
    public void validateResolved() {
        delegate.validateResolved();
    }

    @Override
    public de.ukon.lfgxdrt.SvgMeasure measureSvg(de.ukon.lfgxdrt.SvgRenderContext ctx) {
        return delegate.measureSvg(ctx);
    }

    @Override
    public void renderSvg(de.ukon.lfgxdrt.SvgRenderContext ctx, double x, double y) {
        delegate.renderSvg(ctx, x, y);
    }

    @Override
    public void renderSvg(de.ukon.lfgxdrt.SvgRenderContext ctx, double x, de.ukon.lfgxdrt.SvgLayout layout) {
        delegate.renderSvg(ctx, x, layout);
    }

    @Override
    public String toProlog() {
        return delegate.toProlog();
    }

    @Override
    public void addSourceIndex(Integer sourceIndex) {
        if (sourceIndex == null) {
            return;
        }
        setSourceIndex(sourceIndex);
        delegate.setSourceIndex(sourceIndex);
    }

    @Override
    public glueSemantics.semantics.SemanticRepresentation applyTo(glueSemantics.semantics.SemanticRepresentation var,
                                                                    glueSemantics.semantics.SemanticRepresentation arg) throws ProverException {
        if (var instanceof LfgxDrtSemanticRepresentation wrappedVar && arg instanceof LfgxDrtSemanticRepresentation wrappedArg) {
            SemanticExpression applied = new de.ukon.lfgxdrt.lambda_elements.FuncApp(wrappedVar.delegate, wrappedArg.delegate).betaReduce();
            return wrapResult(applied);
        }
        return this;
    }

    @Override
    public LfgxDrtSemanticRepresentation clone() {
        return wrapResult(delegate);
    }

    @Override
    public Set<SemAtom> findBoundVariables() {
        return new HashSet<>();
    }

    @Override
    public boolean bindsVar(SemAtom var) {
        return delegate.boundVariables().contains(var.getName());
    }

    @Override
    public boolean containsQuantExpression() {
        return false;
    }

    private LfgxDrtSemanticRepresentation wrapResult(SemanticExpression expression) {
        LfgxDrtSemanticRepresentation wrapped = new LfgxDrtSemanticRepresentation(expression);
        wrapped.setType(getType());
        wrapped.setSourceIndex(getSourceIndex());
        return wrapped;
    }

    private static void attachSourceIndex(SemanticExpression expression, Integer sourceIndex) {
        if (expression == null || sourceIndex == null) {
            return;
        }

        expression.setSourceIndex(sourceIndex);

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.DRS drs) {
            if (drs.referents != null) {
                for (de.ukon.lfgxdrt.drs_elements.DiscourseReferent referent : drs.referents) {
                    attachSourceIndex(referent, sourceIndex);
                }
            }
            if (drs.conditions != null) {
                for (de.ukon.lfgxdrt.drs_elements.DrsCondition condition : drs.conditions) {
                    attachSourceIndex((SemanticExpression) condition, sourceIndex);
                }
            }
            if (drs.anaphoraMapping != null) {
                attachSourceIndex(drs.anaphoraMapping, sourceIndex);
            }
            if (drs.presuppositionMapping != null) {
                attachSourceIndex(drs.presuppositionMapping, sourceIndex);
            }
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.DrsMerge merge) {
            attachSourceIndex(merge.left, sourceIndex);
            attachSourceIndex(merge.right, sourceIndex);
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.UnaryOperator unary) {
            attachSourceIndex(unary.scope, sourceIndex);
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.BinaryOperator binary) {
            attachSourceIndex(binary.left, sourceIndex);
            attachSourceIndex(binary.right, sourceIndex);
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.ScopedCondition scopedCondition) {
            attachSourceIndex(scopedCondition.condition, sourceIndex);
            attachSourceIndex(scopedCondition.scope, sourceIndex);
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.SimpleCondition simpleCondition) {
            for (Object arg : simpleCondition.arguments) {
                if (arg instanceof SemanticExpression se) {
                    attachSourceIndex(se, sourceIndex);
                }
            }
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.EqualityCondition equalityCondition) {
            if (equalityCondition.left instanceof SemanticExpression seLeft) {
                attachSourceIndex(seLeft, sourceIndex);
            }
            if (equalityCondition.right instanceof SemanticExpression seRight) {
                attachSourceIndex(seRight, sourceIndex);
            }
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.ComparisonCondition comparisonCondition) {
            if (comparisonCondition.left instanceof SemanticExpression seLeft) {
                attachSourceIndex(seLeft, sourceIndex);
            }
            if (comparisonCondition.right instanceof SemanticExpression seRight) {
                attachSourceIndex(seRight, sourceIndex);
            }
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.InequalityCondition inequalityCondition) {
            if (inequalityCondition.left instanceof SemanticExpression seLeft) {
                attachSourceIndex(seLeft, sourceIndex);
            }
            if (inequalityCondition.right instanceof SemanticExpression seRight) {
                attachSourceIndex(seRight, sourceIndex);
            }
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.PresuppositionOperator presuppositionOperator) {
            attachSourceIndex(presuppositionOperator.scope, sourceIndex);
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.ScopeExpression scopeExpression) {
            attachSourceIndex(scopeExpression.scope, sourceIndex);
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.lambda_elements.FuncApp funcApp) {
            attachSourceIndex(funcApp.function, sourceIndex);
            attachSourceIndex(funcApp.argument, sourceIndex);
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.lambda_elements.LambdaFunction lambdaFunction) {
            attachSourceIndex(lambdaFunction.binder, sourceIndex);
            attachSourceIndex(lambdaFunction.body, sourceIndex);
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.AnaphoraMapping anaphoraMapping) {
            for (de.ukon.lfgxdrt.drs_elements.AnaphoraRelation relation : anaphoraMapping.relations) {
                attachSourceIndex(relation, sourceIndex);
            }
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.PresuppositionMapping presuppositionMapping) {
            for (de.ukon.lfgxdrt.drs_elements.PresuppositionRelation relation : presuppositionMapping.relations) {
                attachSourceIndex(relation, sourceIndex);
            }
            return;
        }

        if (expression instanceof de.ukon.lfgxdrt.drs_elements.AnaphoraRelation anaphoraRelation) {
            attachSourceIndex(anaphoraRelation.pronoun, sourceIndex);
            return;
        }
    }

}
