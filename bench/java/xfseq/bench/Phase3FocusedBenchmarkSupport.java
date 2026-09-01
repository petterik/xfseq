package xfseq.bench;

import clojure.lang.IFn;
import clojure.lang.Namespace;
import clojure.lang.PersistentHashMap;
import clojure.lang.RT;
import clojure.lang.Symbol;
import clojure.lang.Var;

import xfseq.bench.calls$_focusedFunction;
import xfseq.bench.calls$_focusedNonReducingXform;
import xfseq.bench.calls$_focusedXform;
import xfseq.bench.calls$_checksum;
import xfseq.bench.calls$_firstValue;
import xfseq.bench.calls$_publicSequence;
import xfseq.bench.calls$_publicXfSeq;
import xfseq.bench.calls$_publicEduction;
import xfseq.bench.calls$_prefixChecksum;
import xfseq.bench.calls$_transduceChecksum;
import xfseq.bench.calls$_transduceFirst;
import xfseq.bench.calls$_transducePrefixChecksum;
import xfseq.bench.calls$_transduceVector;
import xfseq.bench.calls$_focusedCoreFilter;
import xfseq.bench.calls$_focusedCoreMap;
import xfseq.bench.calls$_focusedCoreRemove;
import xfseq.bench.calls$_focusedCoreTake;
import xfseq.bench.calls$_focusedCandidateFilter;
import xfseq.bench.calls$_focusedCandidateMap;
import xfseq.bench.calls$_focusedCandidateRemove;
import xfseq.bench.calls$_focusedCandidateTake;
import xfseq.bench.calls$_candidate;
import xfseq.bench.calls$_source;
import xfseq.bench.calls__init;

/**
 * Setup-selected plans for the Phase 3 workload/source follow-up rows.
 *
 * <p>The primary Phase 3 harness intentionally remains a small exact-key
 * matrix.  This focused harness owns the larger source and workload
 * vocabulary needed by later diagnosis; each implementation/operation plan
 * is selected once in JMH setup, so timed methods contain no string dispatch.</p>
 */
final class Phase3FocusedBenchmarkSupport {

    static {
        // The focused class can be launched directly by JMH, so initialize
        // the AOT caller namespace before any setup fixture obtains a
        // function or xform from it.
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

    private Phase3FocusedBenchmarkSupport() {
    }

    private static final String[] SINKS = {"first", "prefix8", "traverse",
            "vector", "reduceUnretained", "reduceRetained"};

    interface ConstructionPlan {
        Object construct(Object function, Object xform, Object nonReducingXform,
                         Object source, int takeCount);

        Object value(Object constructed);
    }

    interface SinkPlan {
        Object sink(Object function, Object xform, Object nonReducingXform,
                    Object source, int takeCount);
    }

    interface RetainedPlan {
        Object construct(Object function, Object xform, Object nonReducingXform,
                         Object source, int takeCount);

        Object finish(Object retained);
    }

    static Object source(String sourceKind, int size) {
        return calls$_source.invokeStatic(sourceKind, Integer.valueOf(size));
    }

    static Object function(String operation, String workload, int size) {
        return calls$_focusedFunction.invokeStatic(
                operation, workload, Integer.valueOf(size));
    }

    static Object xform(String operation, String workload, int size,
                        int takeCount) {
        return calls$_focusedXform.invokeStatic(
                operation, workload, Integer.valueOf(size),
                Integer.valueOf(takeCount));
    }

    static Object nonReducingXform(String operation, String workload, int size) {
        return calls$_focusedNonReducingXform.invokeStatic(
                operation, workload, Integer.valueOf(size));
    }

    static int takeCount(String specification, int sourceSize,
                         String sourceKind) {
        if ("source-length".equals(specification)) {
            return sourceSize;
        }
        if ("small-prefix".equals(specification)) {
            return 8;
        }
        if ("large-prefix".equals(specification)) {
            // Keep the explicit large/infinite workload finite for every
            // source, including the infinite repeat/iterate fixtures.
            return 1000;
        }
        return Integer.parseInt(specification);
    }

    /**
     * Run a small deterministic correctness lane for the expanded focused
     * vocabulary.  This is invoked by the Phase 3 jar trial, never by a JMH
     * measured method, and creates a fresh source for every comparison.
     */
    static int validateTrial() {
        int comparisons = 0;
        String[] primary = {"core-direct", "candidate-direct", "xfseq-generic",
                "sequence", "eduction", "transduce"};
        String[] mapWorkloads = {"identity", "arithmetic", "heavy"};
        String[] selectivities = {"selectivity-0", "selectivity-50",
                "selectivity-1", "selectivity-99", "selectivity-100"};
        String[] takeCounts = {"0", "1", "8", "33", "source-length",
                "small-prefix"};

        assertOutputSelectivities();

        for (String workload : mapWorkloads) {
            for (String sourceKind : new String[]{"list", "vector",
                    "map-entries"}) {
                comparisons += compareCase(primary, "map", workload,
                        sourceKind, 32, "0");
            }
        }
        for (String operation : new String[]{"filter", "remove"}) {
            for (String workload : selectivities) {
                for (String sourceKind : new String[]{"list", "vector"}) {
                    comparisons += compareCase(primary, operation, workload,
                            sourceKind, 32, "0");
                }
            }
        }
        for (String workload : new String[]{"take"}) {
            for (String count : takeCounts) {
                for (String sourceKind : new String[]{"list", "vector",
                        "repeat", "iterate"}) {
                    comparisons += compareCase(primary, "take", workload,
                            sourceKind, 32, count);
                }
            }
        }

        // Include each repaired reduced-aware/no-reduced candidate in one
        // applicable dechunked or chunked row.  The registry applies the same
        // source-shape rule to focused manifests.
        for (String implementation : Phase2BenchmarkSupport.CANDIDATE_IDS) {
            String sourceKind = implementation.contains("dechunked")
                    ? "list"
                    : implementation.contains("chunked") ? "vector" : "list";
            for (String operation : new String[]{"map", "filter", "remove"}) {
                String workload = "map".equals(operation)
                        ? "identity" : "selectivity-50";
                comparisons += compareCase(new String[]{implementation},
                        operation, workload, sourceKind, 32, "0");
            }
        }
        return comparisons;
    }

    private static int compareCase(String[] implementations,
                                    String operation, String workload,
                                    String sourceKind, int size,
                                    String takeCountSpecification) {
        int takeCount = takeCount(takeCountSpecification, size, sourceKind);
        for (String implementation : implementations) {
            for (String sink : SINKS) {
                if ("transduce".equals(implementation)
                        && "reduceRetained".equals(sink)) {
                    continue;
                }
                Object expected = sinkResult("core-direct", operation,
                        workload, sourceKind, size, takeCount, sink);
                Object actual = sinkResult(implementation, operation, workload,
                        sourceKind, size, takeCount, sink);
                if (!(expected == null ? actual == null
                        : expected.equals(actual))) {
                    throw new IllegalStateException(
                            "Focused Phase 3 trial mismatch for "
                                    + implementation + "/" + operation + "/"
                                    + workload + "/" + sourceKind + "/"
                                    + takeCountSpecification + "/" + sink
                                    + ": expected " + expected + ", actual "
                                    + actual);
                }
            }
        }
        return implementations.length;
    }

    private static Object sinkResult(String implementation, String operation,
                                     String workload, String sourceKind,
                                     int size, int takeCount, String sink) {
        return sinkPlan(implementation, operation, sink).sink(
                function(operation, workload, size),
                xform(operation, workload, size, takeCount),
                isNonReducing(implementation)
                        ? nonReducingXform(operation, workload, size)
                        : null,
                source(sourceKind, size), takeCount);
    }

    /** Check that selectivity names mean output percentage for both forms. */
    private static void assertOutputSelectivities() {
        int size = 100;
        String[] labels = {"selectivity-0", "selectivity-1", "selectivity-50",
                "selectivity-99", "selectivity-100"};
        for (String operation : new String[]{"filter", "remove"}) {
            for (String label : labels) {
                int expected = Integer.parseInt(
                        label.substring("selectivity-".length()));
                Object function = function(operation, label, size);
                Object result = "filter".equals(operation)
                        ? calls$_focusedCoreFilter.invokeStatic(function,
                                source("list", size))
                        : calls$_focusedCoreRemove.invokeStatic(function,
                                source("list", size));
                int actual = RT.count(result);
                if (actual != expected) {
                    throw new IllegalStateException(
                            "Focused output selectivity mismatch for "
                                    + operation + "/" + label + ": expected "
                                    + expected + ", actual " + actual);
                }
            }
        }
    }

    private static boolean isNonReducing(String implementation) {
        return implementation.startsWith("java-")
                && implementation.contains("nonreducing");
    }

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
            return publicPlan(new PublicCall() {
                @Override
                public Object call(Object xform, Object source) {
                    return calls$_publicXfSeq.invokeStatic(xform, source);
                }
            });
        }
        if ("sequence".equals(implementation)) {
            return publicPlan(new PublicCall() {
                @Override
                public Object call(Object xform, Object source) {
                    return calls$_publicSequence.invokeStatic(xform, source);
                }
            });
        }
        if ("eduction".equals(implementation)) {
            return publicPlan(new PublicCall() {
                @Override
                public Object call(Object xform, Object source) {
                    return calls$_publicEduction.invokeStatic(xform, source);
                }
            });
        }
        if (implementation.startsWith("java-")) {
            return repairedCandidatePlan(implementation, operation);
        }
        throw new IllegalArgumentException("Unknown focused implementation: "
                + implementation);
    }

    private static ConstructionPlan unsupportedConstructionPlan() {
        return new ConstructionPlan() {
            @Override
            public Object construct(Object function, Object xform,
                                    Object nonReducingXform, Object source,
                                    int takeCount) {
                throw new UnsupportedOperationException(
                        "Transduce has no construct-equivalent sink");
            }

            @Override
            public Object value(Object constructed) {
                throw new UnsupportedOperationException(
                        "Transduce has no construct-equivalent sink");
            }
        };
    }

    private static ConstructionPlan corePlan(String operation) {
        if ("map".equals(operation)) {
            return directPlan(new DirectCall() {
                @Override
                public Object call(Object function, Object xform,
                                   Object source, int takeCount) {
                    return calls$_focusedCoreMap.invokeStatic(function, source);
                }
            });
        }
        if ("filter".equals(operation)) {
            return directPlan(new DirectCall() {
                @Override
                public Object call(Object function, Object xform,
                                   Object source, int takeCount) {
                    return calls$_focusedCoreFilter.invokeStatic(function, source);
                }
            });
        }
        if ("remove".equals(operation)) {
            return directPlan(new DirectCall() {
                @Override
                public Object call(Object function, Object xform,
                                   Object source, int takeCount) {
                    return calls$_focusedCoreRemove.invokeStatic(function, source);
                }
            });
        }
        if ("take".equals(operation)) {
            return directPlan(new DirectCall() {
                @Override
                public Object call(Object function, Object xform,
                                   Object source, int takeCount) {
                    return calls$_focusedCoreTake.invokeStatic(
                            Integer.valueOf(takeCount), source);
                }
            });
        }
        throw unknownOperation(operation);
    }

    private static ConstructionPlan candidatePlan(String operation) {
        if ("map".equals(operation)) {
            return directPlan(new DirectCall() {
                @Override
                public Object call(Object function, Object xform,
                                   Object source, int takeCount) {
                    return calls$_focusedCandidateMap.invokeStatic(function, source);
                }
            });
        }
        if ("filter".equals(operation)) {
            return directPlan(new DirectCall() {
                @Override
                public Object call(Object function, Object xform,
                                   Object source, int takeCount) {
                    return calls$_focusedCandidateFilter.invokeStatic(function, source);
                }
            });
        }
        if ("remove".equals(operation)) {
            return directPlan(new DirectCall() {
                @Override
                public Object call(Object function, Object xform,
                                   Object source, int takeCount) {
                    return calls$_focusedCandidateRemove.invokeStatic(function, source);
                }
            });
        }
        if ("take".equals(operation)) {
            return directPlan(new DirectCall() {
                @Override
                public Object call(Object function, Object xform,
                                   Object source, int takeCount) {
                    return calls$_focusedCandidateTake.invokeStatic(
                            Integer.valueOf(takeCount), source);
                }
            });
        }
        throw unknownOperation(operation);
    }

    private interface PublicCall {
        Object call(Object xform, Object source);
    }

    private static ConstructionPlan publicPlan(final PublicCall call) {
        return new ConstructionPlan() {
            @Override
            public Object construct(Object function, Object xform,
                                    Object nonReducingXform, Object source,
                                    int takeCount) {
                return call.call(xform, source);
            }

            @Override
            public Object value(Object constructed) {
                return constructed;
            }
        };
    }

    private static ConstructionPlan repairedCandidatePlan(
            final String stableId, String operation) {
        if ("take".equals(operation)) {
            throw new IllegalArgumentException(
                    "No repaired non-reducing candidate applies to take");
        }
        return new ConstructionPlan() {
            @Override
            public Object construct(Object function, Object xform,
                                    Object nonReducingXform, Object source,
                                    int takeCount) {
                Object candidateXform = stableId.contains("nonreducing")
                        ? nonReducingXform : xform;
                return calls$_candidate.invokeStatic(stableId, candidateXform,
                                                     source);
            }

            @Override
            public Object value(Object constructed) {
                return ((IFn) constructed).invoke();
            }
        };
    }

    private interface DirectCall {
        Object call(Object function, Object xform, Object source, int takeCount);
    }

    private static ConstructionPlan directPlan(final DirectCall call) {
        return new ConstructionPlan() {
            @Override
            public Object construct(Object function, Object xform,
                                    Object nonReducingXform, Object source,
                                    int takeCount) {
                return call.call(function, xform, source, takeCount);
            }

            @Override
            public Object value(Object constructed) {
                return constructed;
            }
        };
    }

    /** Select the terminal sink once, including direct transduce controls. */
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
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return calls$_firstValue.invokeStatic(
                            construction.value(construction.construct(
                                    function, xform, nonReducingXform, source,
                                    takeCount)));
                }
            };
        }
        if ("prefix8".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return calls$_prefixChecksum.invokeStatic(
                            construction.value(construction.construct(
                                    function, xform, nonReducingXform, source,
                                    takeCount)), Integer.valueOf(8));
                }
            };
        }
        if ("traverse".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return calls$_checksum.invokeStatic(
                            construction.value(construction.construct(
                                    function, xform, nonReducingXform, source,
                                    takeCount)));
                }
            };
        }
        if ("vector".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return Phase2BenchmarkSupport.vectorValue(
                            construction.value(construction.construct(
                                    function, xform, nonReducingXform, source,
                                    takeCount)));
                }
            };
        }
        if ("reduceUnretained".equals(sink)
                || "reduceRetained".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return Phase2BenchmarkSupport.reduceChecksum(
                            construction.value(construction.construct(
                                    function, xform, nonReducingXform, source,
                                    takeCount)));
                }
            };
        }
        throw new IllegalArgumentException("Unknown focused Phase 3 sink: "
                + sink);
    }

    private static SinkPlan transduceSinkPlan(String sink) {
        if ("first".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return calls$_transduceFirst.invokeStatic(xform, source);
                }
            };
        }
        if ("prefix8".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return calls$_transducePrefixChecksum.invokeStatic(
                            xform, source);
                }
            };
        }
        if ("traverse".equals(sink)
                || "reduceUnretained".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return calls$_transduceChecksum.invokeStatic(xform, source);
                }
            };
        }
        if ("vector".equals(sink)) {
            return new SinkPlan() {
                @Override
                public Object sink(Object function, Object xform,
                                   Object nonReducingXform, Object source,
                                   int takeCount) {
                    return calls$_transduceVector.invokeStatic(xform, source);
                }
            };
        }
        throw new IllegalArgumentException("Unknown transduce sink: " + sink);
    }

    static RetainedPlan retainedPlan(String implementation, String operation) {
        final ConstructionPlan construction = constructionPlan(
                implementation, operation);
        return new RetainedPlan() {
            @Override
            public Object construct(Object function, Object xform,
                                    Object nonReducingXform, Object source,
                                    int takeCount) {
                return construction.construct(function, xform,
                        nonReducingXform, source, takeCount);
            }

            @Override
            public Object finish(Object retained) {
                return Phase2BenchmarkSupport.reduceChecksum(
                        construction.value(retained));
            }
        };
    }

    private static IllegalArgumentException unknownOperation(String operation) {
        return new IllegalArgumentException("Unknown focused operation: "
                + operation);
    }
}
