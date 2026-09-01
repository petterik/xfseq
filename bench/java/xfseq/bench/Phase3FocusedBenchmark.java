package xfseq.bench;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.infra.Blackhole;

import java.util.concurrent.TimeUnit;

/**
 * Focused Phase 3 workload/source controls for Slice 4 diagnosis.
 *
 * <p>The primary {@link Phase3UnaryBenchmark} remains intentionally small.
 * This class makes the plan's broader dimensions executable without changing
 * the production path or expanding the primary screen/decision manifests.</p>
 */
@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
public class Phase3FocusedBenchmark {

    @Param({
            "core-direct", "candidate-direct", "xfseq-generic", "sequence",
            "eduction", "transduce",
            "java-polymorphic-object-reduced-aware-v2",
            "java-mixed-object-reduced-aware-v2",
            "java-mixed-object-nonreducing-v2",
            "java-dechunked-object-reduced-aware-v2",
            "java-dechunked-object-nonreducing-v2",
            "java-chunked-object-reduced-aware-v2",
            "java-chunked-object-nonreducing-v2"})
    public String implementation;

    @Param({"map", "filter", "remove", "take"})
    public String operation;

    @Param({"list", "lazy-list", "vector", "subvector", "range", "set",
            "map-entries", "sorted-map-entries", "array", "iterable",
            "iterator", "repeat", "iterate"})
    public String sourceKind;

    @Param({"identity", "arithmetic", "heavy", "selectivity-0",
            "selectivity-1", "selectivity-50", "selectivity-99",
            "selectivity-100", "take"})
    public String workload;

    @Param({"0", "1", "8", "31", "32", "33", "source-length",
            "small-prefix", "large-prefix"})
    public String takeCount;

    @Param({"0", "1", "4", "8", "31", "32", "33", "64", "1000",
            "10000", "1000000"})
    public int size;

    private Object source;
    private Object function;
    private Object xform;
    private Object nonReducingXform;
    private Object retainedHead;
    private int resolvedTakeCount;
    private Phase3FocusedBenchmarkSupport.ConstructionPlan constructionPlan;
    private Phase3FocusedBenchmarkSupport.SinkPlan firstPlan;
    private Phase3FocusedBenchmarkSupport.SinkPlan prefixPlan;
    private Phase3FocusedBenchmarkSupport.SinkPlan traversePlan;
    private Phase3FocusedBenchmarkSupport.SinkPlan vectorPlan;
    private Phase3FocusedBenchmarkSupport.SinkPlan reduceUnretainedPlan;
    private Phase3FocusedBenchmarkSupport.RetainedPlan retainedPlan;

    @Setup
    public void setup() {
        resolvedTakeCount = Phase3FocusedBenchmarkSupport.takeCount(
                takeCount, size, sourceKind);
        constructionPlan = Phase3FocusedBenchmarkSupport.constructionPlan(
                implementation, operation);
        firstPlan = Phase3FocusedBenchmarkSupport.sinkPlan(
                implementation, operation, "first");
        prefixPlan = Phase3FocusedBenchmarkSupport.sinkPlan(
                implementation, operation, "prefix8");
        traversePlan = Phase3FocusedBenchmarkSupport.sinkPlan(
                implementation, operation, "traverse");
        vectorPlan = Phase3FocusedBenchmarkSupport.sinkPlan(
                implementation, operation, "vector");
        reduceUnretainedPlan = Phase3FocusedBenchmarkSupport.sinkPlan(
                implementation, operation, "reduceUnretained");
        if (!"transduce".equals(implementation)) {
            retainedPlan = Phase3FocusedBenchmarkSupport.retainedPlan(
                    implementation, operation);
        }
        function = Phase3FocusedBenchmarkSupport.function(
                operation, workload, size);
        xform = Phase3FocusedBenchmarkSupport.xform(
                operation, workload, size, resolvedTakeCount);
        if (isNonReducingCandidate()) {
            nonReducingXform = Phase3FocusedBenchmarkSupport.nonReducingXform(
                    operation, workload, size);
        }
    }

    @Setup(Level.Invocation)
    public void setupInvocation() {
        source = Phase3FocusedBenchmarkSupport.source(sourceKind, size);
    }

    @TearDown(Level.Invocation)
    public void tearDownInvocation() {
        retainedHead = null;
    }

    private boolean isNonReducingCandidate() {
        return implementation.startsWith("java-")
                && implementation.contains("nonreducing");
    }

    private Object construct() {
        return constructionPlan.construct(function, xform, nonReducingXform,
                                          source, resolvedTakeCount);
    }

    private Object value(Object constructed) {
        return constructionPlan.value(constructed);
    }

    @Benchmark
    public void construct(Blackhole blackhole) {
        blackhole.consume(construct());
    }

    @Benchmark
    public void first(Blackhole blackhole) {
        blackhole.consume(firstPlan.sink(function, xform, nonReducingXform,
                                         source, resolvedTakeCount));
    }

    @Benchmark
    public void prefix8(Blackhole blackhole) {
        blackhole.consume(prefixPlan.sink(function, xform, nonReducingXform,
                                          source, resolvedTakeCount));
    }

    @Benchmark
    public void traverse(Blackhole blackhole) {
        blackhole.consume(traversePlan.sink(function, xform, nonReducingXform,
                                            source, resolvedTakeCount));
    }

    @Benchmark
    public void vector(Blackhole blackhole) {
        blackhole.consume(vectorPlan.sink(function, xform, nonReducingXform,
                                          source, resolvedTakeCount));
    }

    @Benchmark
    public void reduceUnretained(Blackhole blackhole) {
        blackhole.consume(reduceUnretainedPlan.sink(
                function, xform, nonReducingXform, source, resolvedTakeCount));
    }

    @Benchmark
    public void reduceRetained(Blackhole blackhole) {
        retainedHead = retainedPlan.construct(function, xform,
                nonReducingXform, source, resolvedTakeCount);
        blackhole.consume(retainedPlan.finish(retainedHead));
    }
}
