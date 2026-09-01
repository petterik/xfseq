package xfseq.bench;

import clojure.lang.Namespace;
import clojure.lang.PersistentHashMap;
import clojure.lang.RT;
import clojure.lang.Symbol;
import clojure.lang.Var;

import xfseq.bench.calls$_candidateFilter;
import xfseq.bench.calls$_candidateMap;
import xfseq.bench.calls$_candidateRemove;
import xfseq.bench.calls$_candidateTake;
import xfseq.bench.calls$_checksum;
import xfseq.bench.calls$_coreFilter;
import xfseq.bench.calls$_coreMap;
import xfseq.bench.calls$_coreRemove;
import xfseq.bench.calls$_coreTake;
import xfseq.bench.calls$_firstValue;
import xfseq.bench.calls$_phase3Function;
import xfseq.bench.calls$_phase3Xform;
import xfseq.bench.calls$_publicEduction;
import xfseq.bench.calls$_publicSequence;
import xfseq.bench.calls$_publicXfSeq;
import xfseq.bench.calls$_prefixChecksum;
import xfseq.bench.calls$_source;
import xfseq.bench.calls$_transduceChecksum;
import xfseq.bench.calls$_transduceFirst;
import xfseq.bench.calls$_transducePrefixChecksum;
import xfseq.bench.calls$_transduceVector;
import xfseq.bench.calls__init;

/** Non-timed setup and call selection for the Phase 3 direct-unary harness. */
public final class Phase3BenchmarkSupport {

    static {
        // Load the AOT caller namespace before benchmark state is created.
        // Every timed public call below enters a generated invokeStatic method;
        // this setup is deliberately outside the JMH benchmark methods.
        Var warnOnReflection = RT.var("clojure.core", "*warn-on-reflection*");
        Var currentNamespace = RT.var("clojure.core", "*ns*");
        Var.pushThreadBindings(PersistentHashMap.create(
                new Object[]{warnOnReflection, Boolean.TRUE,
                        currentNamespace,
                        Namespace.findOrCreate(Symbol.intern("xfseq.bench.calls"))}));
        try {
            calls__init.load();
        } finally {
            Var.popThreadBindings();
        }
    }

    static final String[] SOURCE_KINDS = {
            "list", "vector", "subvector", "range", "set", "array",
            "iterable", "iterator"
    };
    static final String[] IMPLEMENTATIONS = {
            "core-direct", "candidate-direct", "xfseq-generic", "sequence",
            "eduction", "transduce"
    };
    static final String[] OPERATIONS = {"map", "filter", "remove", "take"};
    static final String[] SINKS = {"first", "prefix8", "traverse", "vector",
            "reduceUnretained", "reduceRetained"};
    static final int TAKE_COUNT = 32;

    private Phase3BenchmarkSupport() {
    }

    interface ConstructionPlan {
        Object construct(Object function, Object xform, Object source);
    }

    interface SinkPlan {
        Object sink(Object function, Object xform, Object source);
    }

    interface RetainedPlan {
        Object construct(Object function, Object xform, Object source);

        Object finish(Object retained);
    }

    static Object source(String sourceKind, int size) {
        return calls$_source.invokeStatic(sourceKind, Integer.valueOf(size));
    }

    static Object function(String operation) {
        return calls$_phase3Function.invokeStatic(operation);
    }

    static Object xform(String operation) {
        return calls$_phase3Xform.invokeStatic(operation);
    }

    /**
     * Select one construction plan during benchmark setup.  The returned
     * plan has no implementation/operation dispatch in its timed method;
     * candidate plans still create their owned xform inside that method.
     */
    static ConstructionPlan constructionPlan(String implementation,
                                              String operation) {
        if ("transduce".equals(implementation)) {
            return unsupportedConstructionPlan();
        }
        if ("core-direct".equals(implementation)) {
            return corePlan(operation);
        }
        if ("candidate-direct".equals(implementation)) {
            return candidatePlan(operation);
        }
        if ("xfseq-generic".equals(implementation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_publicXfSeq.invokeStatic(xform, source);
                }
            };
        }
        if ("sequence".equals(implementation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_publicSequence.invokeStatic(xform, source);
                }
            };
        }
        if ("eduction".equals(implementation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_publicEduction.invokeStatic(xform, source);
                }
            };
        }
        throw new IllegalArgumentException("Unknown Phase 3 implementation: "
                + implementation);
    }

    private static ConstructionPlan unsupportedConstructionPlan() {
        return new ConstructionPlan() {
            @Override
            public Object construct(Object function, Object xform, Object source) {
                throw new UnsupportedOperationException(
                        "Transduce has no construct-equivalent sink");
            }
        };
    }

    private static ConstructionPlan corePlan(String operation) {
        if ("map".equals(operation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_coreMap.invokeStatic(function, source);
                }
            };
        }
        if ("filter".equals(operation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_coreFilter.invokeStatic(function, source);
                }
            };
        }
        if ("remove".equals(operation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_coreRemove.invokeStatic(function, source);
                }
            };
        }
        if ("take".equals(operation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_coreTake.invokeStatic(Integer.valueOf(TAKE_COUNT), source);
                }
            };
        }
        throw new IllegalArgumentException("Unknown Phase 3 operation: "
                + operation);
    }

    private static ConstructionPlan candidatePlan(String operation) {
        if ("map".equals(operation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    // The candidate owns xform creation in its timed call.
                    return calls$_candidateMap.invokeStatic(function, source);
                }
            };
        }
        if ("filter".equals(operation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_candidateFilter.invokeStatic(function, source);
                }
            };
        }
        if ("remove".equals(operation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_candidateRemove.invokeStatic(function, source);
                }
            };
        }
        if ("take".equals(operation)) {
            return new ConstructionPlan() {
                @Override
                public Object construct(Object function, Object xform, Object source) {
                    return calls$_candidateTake.invokeStatic(Integer.valueOf(TAKE_COUNT), source);
                }
            };
        }
        throw new IllegalArgumentException("Unknown Phase 3 operation: "
                + operation);
    }

    /**
     * Select the terminal operation once during setup.  A transduce row has
     * no lazy head to construct, so it uses a direct sink-specific transduce
     * wrapper; all other implementations construct once and then call the
     * corresponding AOT sink wrapper.  No sink/implementation branch is
     * present in a timed method.
     */
    static SinkPlan sinkPlan(String implementation, String operation,
                             String sink) {
        if ("transduce".equals(implementation)) {
            if ("construct".equals(sink)) {
                throw new IllegalArgumentException(
                        "Transduce has no construct-equivalent sink");
            }
            if ("reduceRetained".equals(sink)) {
                throw new IllegalArgumentException(
                        "Transduce has no retained-head sink");
            }
            return transduceSinkPlan(sink);
        }
        final ConstructionPlan construction = constructionPlan(
                implementation, operation);
        if ("first".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return calls$_firstValue.invokeStatic(
                            construction.construct(function, xform, source));
                }
            };
        }
        if ("prefix8".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return calls$_prefixChecksum.invokeStatic(
                            construction.construct(function, xform, source),
                            Integer.valueOf(8));
                }
            };
        }
        if ("traverse".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return calls$_checksum.invokeStatic(
                            construction.construct(function, xform, source));
                }
            };
        }
        if ("vector".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return Phase2BenchmarkSupport.vectorValue(
                            construction.construct(function, xform, source));
                }
            };
        }
        if ("reduceUnretained".equals(sink)
                || "reduceRetained".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return Phase2BenchmarkSupport.reduceChecksum(
                            construction.construct(function, xform, source));
                }
            };
        }
        throw new IllegalArgumentException("Unknown Phase 3 sink: " + sink);
    }

    private static SinkPlan transduceSinkPlan(String sink) {
        if ("first".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return calls$_transduceFirst.invokeStatic(xform, source);
                }
            };
        }
        if ("prefix8".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return calls$_transducePrefixChecksum.invokeStatic(
                            xform, source);
                }
            };
        }
        if ("traverse".equals(sink)
                || "reduceUnretained".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return calls$_transduceChecksum.invokeStatic(xform, source);
                }
            };
        }
        if ("vector".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform, Object source) {
                    return calls$_transduceVector.invokeStatic(xform, source);
                }
            };
        }
        throw new IllegalArgumentException("Unknown transduce sink: " + sink);
    }

    /** Select the lazy retained-reduction shape once during setup. */
    static RetainedPlan retainedPlan(String implementation, String operation) {
        final ConstructionPlan construction = constructionPlan(
                implementation, operation);
        return new RetainedPlan() {
            @Override
            public Object construct(Object function, Object xform,
                                    Object source) {
                return construction.construct(function, xform, source);
            }

            @Override
            public Object finish(Object retained) {
                return Phase2BenchmarkSupport.reduceChecksum(retained);
            }
        };
    }

    /**
     * Compare every implementation against direct core using fresh fixtures.
     * This is setup/trial validation only; JMH invocation sources are created
     * independently after this method returns.
     */
    public static void validateTrial() {
        String[] trialSources = {"list", "vector", "iterator"};
        int comparisons = 0;
        int sinkChecks = 0;
        for (String operation : OPERATIONS) {
            for (String sourceKind : trialSources) {
                for (String implementation : IMPLEMENTATIONS) {
                    for (String sink : SINKS) {
                        if ("transduce".equals(implementation)
                                && "reduceRetained".equals(sink)) {
                            continue;
                        }
                        Object expected = sinkPlan("core-direct", operation,
                                sink).sink(function(operation),
                                xform(operation), source(sourceKind, 8));
                        Object actual = sinkPlan(implementation, operation,
                                sink).sink(function(operation),
                                xform(operation), source(sourceKind, 8));
                        if (!(expected == null ? actual == null
                                : expected.equals(actual))) {
                            throw new IllegalStateException(
                                    "Phase 3 trial mismatch for "
                                            + implementation + "/" + operation
                                            + "/" + sourceKind + "/" + sink
                                            + ": expected " + expected
                                            + ", actual " + actual);
                        }
                        sinkChecks++;
                    }
                    comparisons++;
                }
            }
        }
        System.out.println("Phase 3 trial validation passed: " + comparisons
                + " fresh implementation/source cases and " + sinkChecks
                + " sink checks");
        int focusedComparisons = Phase3FocusedBenchmarkSupport.validateTrial();
        System.out.println("Phase 3 focused trial validation passed: "
                + focusedComparisons + " fresh workload/source cases");
    }

    public static void main(String[] args) {
        if (args.length > 0 && !"trial".equals(args[0])) {
            throw new IllegalArgumentException("Unknown Phase 3 support command: "
                    + args[0]);
        }
        validateTrial();
    }
}
