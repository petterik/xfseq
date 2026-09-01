package xfseq.bench;

import clojure.lang.ISeq;
import clojure.lang.IFn;
import clojure.lang.PersistentHashMap;
import clojure.lang.RT;
import clojure.lang.Namespace;
import clojure.lang.Symbol;
import clojure.lang.Var;

import xfseq.bench.calls$_candidate;
import xfseq.bench.calls$_firstValue;
import xfseq.bench.calls$_nonReducingOperation;
import xfseq.bench.calls$_publicEduction;
import xfseq.bench.calls$_publicSequence;
import xfseq.bench.calls$_publicTransduce;
import xfseq.bench.calls$_publicXfSeq;
import xfseq.bench.calls$_reduceChecksum;
import xfseq.bench.calls$_source;
import xfseq.bench.calls$_vectorValue;
import xfseq.bench.calls$_xform;
import xfseq.bench.calls__init;

/** Shared, non-timed benchmark plumbing. */
final class Phase2BenchmarkSupport {

    static {
        // Calling generated invokeStatic methods bypasses Var dispatch, so
        // load their AOT namespace (and its required candidate namespace)
        // once before any benchmark state is created.
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
            "list", "vector", "subvector", "range", "set", "array", "iterable"
    };
    static final String[] SIZES = {
            "0", "1", "4", "8", "31", "32", "33", "64", "1000",
            "10000", "1000000"
    };
    static final String[] WORKLOADS = {
            "identity", "map", "filter", "map-filter", "five-map", "take"
    };
    static final String[] IMPLEMENTATIONS = {
            "xfseq", "sequence", "eduction", "transduce"
    };
    static final String[] CANDIDATE_IDS = {
            "java-polymorphic-object-reduced-aware-v2",
            "java-mixed-object-reduced-aware-v2",
            "java-mixed-object-nonreducing-v2",
            "java-dechunked-object-reduced-aware-v2",
            "java-dechunked-object-nonreducing-v2",
            "java-chunked-object-reduced-aware-v2",
            "java-chunked-object-nonreducing-v2"
    };
    static final String[] BUFFER_COUNTS = {
            "1", "4", "8", "31", "32", "33", "64", "1000"
    };
    static final String[] BUFFER_POLICIES = {
            "current", "all-chunk"
    };

    private Phase2BenchmarkSupport() {
    }

    interface PublicPlan {
        Object construct(Object xform, Object source);
    }

    static PublicPlan publicPlan(String implementation) {
        if ("xfseq".equals(implementation)) {
            return new PublicPlan() {
                @Override
                public Object construct(Object xform, Object source) {
                    return calls$_publicXfSeq.invokeStatic(xform, source);
                }
            };
        }
        if ("sequence".equals(implementation)) {
            return new PublicPlan() {
                @Override
                public Object construct(Object xform, Object source) {
                    return calls$_publicSequence.invokeStatic(xform, source);
                }
            };
        }
        if ("eduction".equals(implementation)) {
            return new PublicPlan() {
                @Override
                public Object construct(Object xform, Object source) {
                    return calls$_publicEduction.invokeStatic(xform, source);
                }
            };
        }
        if ("transduce".equals(implementation)) {
            return new PublicPlan() {
                @Override
                public Object construct(Object xform, Object source) {
                    return calls$_publicTransduce.invokeStatic(xform, source);
                }
            };
        }
        throw new IllegalArgumentException("Unknown public implementation: "
                + implementation);
    }

    static CandidatePlan candidatePlan(String stableId) {
        if ("java-polymorphic-object-reduced-aware-v2".equals(stableId)) {
            return new CandidatePlan(stableId, SourceMode.MIXED, true,
                    "xfseq.XFSeqStep$ObjectStep");
        }
        if ("java-mixed-object-reduced-aware-v2".equals(stableId)) {
            return new CandidatePlan(stableId, SourceMode.MIXED, true,
                    "xfseq.XFSeqStepSimple");
        }
        if ("java-mixed-object-nonreducing-v2".equals(stableId)) {
            return new CandidatePlan(stableId, SourceMode.MIXED, false,
                    "xfseq.XFSeqStepSimpleNoReduced");
        }
        if ("java-dechunked-object-reduced-aware-v2".equals(stableId)) {
            return new CandidatePlan(stableId, SourceMode.DECHUNKED, true,
                    "xfseq.XFSeqStepSingleOnly");
        }
        if ("java-dechunked-object-nonreducing-v2".equals(stableId)) {
            return new CandidatePlan(stableId, SourceMode.DECHUNKED, false,
                    "xfseq.XFSeqStepSingleOnlyNoReduced");
        }
        if ("java-chunked-object-reduced-aware-v2".equals(stableId)) {
            return new CandidatePlan(stableId, SourceMode.CHUNKED, true,
                    "xfseq.XFSeqStepChunkedOnly");
        }
        if ("java-chunked-object-nonreducing-v2".equals(stableId)) {
            return new CandidatePlan(stableId, SourceMode.CHUNKED, false,
                    "xfseq.XFSeqStepChunkedOnlyNoReduced");
        }
        throw new IllegalArgumentException("Unknown Phase 2 candidate: " + stableId);
    }

    enum SourceMode {
        MIXED, DECHUNKED, CHUNKED
    }

    static final class CandidatePlan {
        final String stableId;
        final SourceMode sourceMode;
        final boolean reducedAware;
        final String implementationClass;

        CandidatePlan(String stableId, SourceMode sourceMode, boolean reducedAware,
                      String implementationClass) {
            this.stableId = stableId;
            this.sourceMode = sourceMode;
            this.reducedAware = reducedAware;
            this.implementationClass = implementationClass;
        }

        boolean accepts(String sourceKind) {
            if (sourceMode == SourceMode.MIXED) {
                return true;
            }
            if (sourceMode == SourceMode.DECHUNKED) {
                return "list".equals(sourceKind);
            }
            return "vector".equals(sourceKind);
        }

        IFn create(Object xform, Object source) {
            IFn candidate = (IFn) calls$_candidate.invokeStatic(stableId, xform, source);
            if (!implementationClass.equals(candidate.getClass().getName())) {
                throw new IllegalStateException("Candidate ID selected "
                        + candidate.getClass().getName() + ", expected "
                        + implementationClass + " for " + stableId);
            }
            return candidate;
        }
    }

    static long checksum(Object value) {
        if (value == null) {
            return 0L;
        }
        long total = 0L;
        ISeq seq = RT.seq(value);
        while (seq != null) {
            total += numericValue(seq.first());
            seq = seq.next();
        }
        return total;
    }

    static long prefixChecksum(Object value, int limit) {
        if (value == null || limit <= 0) {
            return 0L;
        }
        long total = 0L;
        int count = 0;
        ISeq seq = RT.seq(value);
        while (seq != null && count < limit) {
            total += numericValue(seq.first());
            count++;
            seq = seq.next();
        }
        return total;
    }

    static Object firstValue(Object value) {
        return value == null ? null : calls$_firstValue.invokeStatic(value);
    }

    static Object source(String sourceKind, int size) {
        return calls$_source.invokeStatic(sourceKind, size);
    }

    static Object xform(String workload) {
        return calls$_xform.invokeStatic(workload);
    }

    static Object nonReducingOperation(String workload) {
        return calls$_nonReducingOperation.invokeStatic(workload);
    }

    static Object vectorValue(Object value) {
        return calls$_vectorValue.invokeStatic(value);
    }

    static long reduceChecksum(Object value) {
        return ((Number) calls$_reduceChecksum.invokeStatic(value)).longValue();
    }

    private static long numericValue(Object value) {
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        return value == null ? 0L : value.hashCode();
    }

    static Object[] values(int count) {
        Object[] values = new Object[count];
        for (int i = 0; i < count; i++) {
            values[i] = Long.valueOf(i);
        }
        return values;
    }

    static String describeCandidateSource(CandidatePlan plan, String sourceKind) {
        return plan.stableId + " requires " + plan.sourceMode
                + " input, but received " + sourceKind;
    }
}
