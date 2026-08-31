package xfseq.bench;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.infra.Blackhole;

import java.util.concurrent.TimeUnit;

/** Public end-to-end rows with each sink measured as a separate operation. */
@BenchmarkMode(Mode.Throughput)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
public class Phase2PublicBenchmark {

    @Param({"xfseq", "sequence", "eduction", "transduce"})
    public String implementation;

    @Param({"list", "vector", "subvector", "range", "set", "array", "iterable"})
    public String sourceKind;

    @Param({"0", "1", "4", "8", "31", "32", "33", "64", "1000",
            "10000", "1000000"})
    public int size;

    @Param({"identity", "map", "filter", "map-filter", "five-map", "take"})
    public String workload;

    private Object source;
    private Object xform;
    private Phase2BenchmarkSupport.PublicPlan plan;

    @Setup
    public void setup() {
        plan = Phase2BenchmarkSupport.publicPlan(implementation);
        source = Phase2BenchmarkSupport.source(sourceKind, size);
        xform = Phase2BenchmarkSupport.xform(workload);
    }

    private Object construct() {
        return plan.construct(xform, source);
    }

    @Benchmark
    public void construct(Blackhole blackhole) {
        blackhole.consume(construct());
    }

    @Benchmark
    public void first(Blackhole blackhole) {
        blackhole.consume(Phase2BenchmarkSupport.firstValue(construct()));
    }

    @Benchmark
    public void prefix8(Blackhole blackhole) {
        blackhole.consume(Phase2BenchmarkSupport.prefixChecksum(construct(), 8));
    }

    @Benchmark
    public void traverse(Blackhole blackhole) {
        blackhole.consume(Phase2BenchmarkSupport.checksum(construct()));
    }

    @Benchmark
    public void vector(Blackhole blackhole) {
        Object value = construct();
        blackhole.consume(Phase2BenchmarkSupport.vectorValue(value));
    }

    @Benchmark
    public void reduce(Blackhole blackhole) {
        blackhole.consume(Phase2BenchmarkSupport.reduceChecksum(construct()));
    }
}
