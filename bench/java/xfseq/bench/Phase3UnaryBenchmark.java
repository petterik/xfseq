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
 * Direct-unary Phase 3 rows and equivalent generic controls.
 *
 * <p>The core and candidate plans invoke one generated, direct-linked caller
 * per unary function.  Generic controls receive setup xforms, while the
 * public candidate's own xform construction remains inside the timed call.</p>
 */
@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
public class Phase3UnaryBenchmark {

    @Param({"core-direct", "candidate-direct", "xfseq-generic", "sequence",
            "eduction", "transduce"})
    public String implementation;

    @Param({"map", "filter", "remove", "take"})
    public String operation;

    @Param({"list", "vector", "subvector", "range", "set", "array",
            "iterable", "iterator"})
    public String sourceKind;

    @Param({"0", "1", "4", "8", "31", "32", "33", "64", "1000",
            "10000", "1000000"})
    public int size;

    private Object source;
    private Object function;
    private Object xform;
    private Object retainedHead;
    private Phase3BenchmarkSupport.ConstructionPlan constructionPlan;
    private Phase3BenchmarkSupport.SinkPlan firstPlan;
    private Phase3BenchmarkSupport.SinkPlan prefixPlan;
    private Phase3BenchmarkSupport.SinkPlan traversePlan;
    private Phase3BenchmarkSupport.SinkPlan vectorPlan;
    private Phase3BenchmarkSupport.SinkPlan reduceUnretainedPlan;
    private Phase3BenchmarkSupport.RetainedPlan retainedPlan;

    @Setup
    public void setup() {
        function = Phase3BenchmarkSupport.function(operation);
        xform = Phase3BenchmarkSupport.xform(operation);
        constructionPlan = Phase3BenchmarkSupport.constructionPlan(
                implementation, operation);
        firstPlan = Phase3BenchmarkSupport.sinkPlan(
                implementation, operation, "first");
        prefixPlan = Phase3BenchmarkSupport.sinkPlan(
                implementation, operation, "prefix8");
        traversePlan = Phase3BenchmarkSupport.sinkPlan(
                implementation, operation, "traverse");
        vectorPlan = Phase3BenchmarkSupport.sinkPlan(
                implementation, operation, "vector");
        reduceUnretainedPlan = Phase3BenchmarkSupport.sinkPlan(
                implementation, operation, "reduceUnretained");
        if (!"transduce".equals(implementation)) {
            retainedPlan = Phase3BenchmarkSupport.retainedPlan(
                    implementation, operation);
        }
    }

    @Setup(Level.Invocation)
    public void setupInvocation() {
        // Rebuild one-shot adapters for every invocation without charging the
        // source fixture construction to the measured call.
        source = Phase3BenchmarkSupport.source(sourceKind, size);
    }

    @TearDown(Level.Invocation)
    public void tearDownInvocation() {
        // The retained row intentionally keeps its fresh head alive through
        // reduction and releases it only at invocation teardown.
        retainedHead = null;
    }

    private Object construct() {
        return constructionPlan.construct(function, xform, source);
    }

    @Benchmark
    public void construct(Blackhole blackhole) {
        blackhole.consume(construct());
    }

    @Benchmark
    public void first(Blackhole blackhole) {
        blackhole.consume(firstPlan.sink(function, xform, source));
    }

    @Benchmark
    public void prefix8(Blackhole blackhole) {
        blackhole.consume(prefixPlan.sink(function, xform, source));
    }

    @Benchmark
    public void traverse(Blackhole blackhole) {
        blackhole.consume(traversePlan.sink(function, xform, source));
    }

    @Benchmark
    public void vector(Blackhole blackhole) {
        blackhole.consume(vectorPlan.sink(function, xform, source));
    }

    @Benchmark
    public void reduceUnretained(Blackhole blackhole) {
        blackhole.consume(reduceUnretainedPlan.sink(function, xform, source));
    }

    @Benchmark
    public void reduceRetained(Blackhole blackhole) {
        retainedHead = retainedPlan.construct(function, xform, source);
        blackhole.consume(retainedPlan.finish(retainedHead));
    }
}
